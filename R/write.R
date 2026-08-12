# Write verbs. Everything funnels through as_jsonld_payload(), so insert, upsert and
# update all accept the same range of inputs.

#' Write data to a ledger
#'
#' @description
#' `fluree_insert()` adds triples without touching existing ones. `fluree_upsert()`
#' replaces the values of every predicate supplied, leaving unmentioned predicates
#' alone, and is idempotent. `fluree_update()` takes a full `where`/`delete`/`insert`
#' body for the cases where the old value has to be bound before it can be replaced.
#' `fluree_delete()` retracts everything about a subject.
#'
#' `data` may be:
#'
#' - a **triple table** - anything [semanticModelR::as_rdf_triples()] accepts,
#'   converted with [semanticModelR::triples_to_jsonld()];
#' - a **tidygraph graph** from [semanticModelR::triples_to_tbl_graph()], converted
#'   back to triples first;
#' - a **JSON-LD list**, passed through untouched.
#'
#' A plain data frame is refused rather than guessed at, because minting IRIs from
#' columns is a modelling decision. Run it through
#' [semanticModelR::pivot_longer_with_type()] or
#' [semanticModelR::pivotLongerSPO()] first.
#'
#' @param con A `fluree_connection`.
#' @param data The data to write. See above.
#' @param object_type Passed to [semanticModelR::as_rdf_triples()] when a triple
#'   table has no `object_type` column.
#' @param id The subject IRI to retract.
#' @param body A list with `where`, `delete` and `insert` entries.
#'
#' @returns The parsed response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' triples <- tibble::tribble(
#'   ~subject, ~predicate, ~object, ~object_type,
#'   "ex:alice", "rdf:type",     "schema:Person", "uri",
#'   "ex:alice", "schema:name", "Alice",          "literal")
#'
#' fluree_insert(con, triples)
#'
#' # or from a graph you have been working on
#' g <- semanticModelR::triples_to_tbl_graph(triples)
#' fluree_insert(con, g)
#'
#' fluree_upsert(con, triples)
#' fluree_delete(con, "ex:alice")
#' }
fluree_insert <- function(con, data, object_type = NULL) {
  check_connection(con)
  body <- with_ledger(con, list(insert = as_jsonld_payload(data, object_type)))
  invisible(con$instance$insert(body)$send())
}

#' @rdname fluree_insert
#' @export
fluree_upsert <- function(con, data, object_type = NULL) {
  check_connection(con)
  body <- with_ledger(con, list(upsert = as_jsonld_payload(data, object_type)))
  invisible(con$instance$upsert(body)$send())
}

#' @rdname fluree_insert
#' @export
fluree_update <- function(con, body) {
  check_connection(con)
  if (!is.list(body)) stop("`body` must be a list.", call. = FALSE)
  invisible(con$instance$update(with_ledger(con, body))$send())
}

#' @rdname fluree_insert
#' @export
fluree_delete <- function(con, id) {
  check_connection(con)
  id_alias <- findIdAlias(fluree_context(con))
  body <- with_ledger(con, handleDelete(id, id_alias))
  invisible(con$instance$update(body)$send())
}

# Fluree wants the ledger in the transaction body; none of the R6 methods add it.
with_ledger <- function(con, body) {
  if (is.null(body$ledger)) body$ledger <- fluree_ledger(con)
  body
}

# Everything becomes a JSON-LD @graph list
as_jsonld_payload <- function(data, object_type = NULL) {
  if (inherits(data, "tbl_graph")) {
    data <- semanticModelR::as_triples(data)
  }

  if (is.data.frame(data)) {
    if (!any(c("subject", "predicate", "object") %in% names(data))) {
      stop("`data` looks like an ordinary data frame, not a triple table.\n",
           "Convert it first: semanticModelR::pivot_longer_with_type() for one row ",
           "per observation, or semanticModelR::pivotLongerSPO() when you have a ",
           "node and predicate specification. Minting IRIs is a modelling decision, ",
           "so it is not done for you.", call. = FALSE)
    }
    triples <- semanticModelR::as_rdf_triples(data, object_type)
    graph <- semanticModelR::triples_to_jsonld(triples)[["@graph"]]
    # triples_to_jsonld() gives untyped nodes @type owl:Thing, which is a reasonable
    # default for a document but would write a triple nobody asked for.
    graph <- drop_default_types(graph, triples)
    return(coerce_graph_literals(graph, triples))
  }

  if (is.list(data)) {
    # already JSON-LD: accept either a whole document or a bare @graph
    if (!is.null(data[["@graph"]])) return(data[["@graph"]])
    return(data)
  }

  stop("`data` must be a triple table, a tbl_graph, or a JSON-LD list, not ",
       class(data)[1], ".", call. = FALSE)
}

# A triple table's `object` column is character, so without this every number would
# land in the ledger as a string and come back as one. The datatype column that
# pivot_longer_with_type() and friends supply says what each predicate really is.
coerce_graph_literals <- function(graph, triples) {
  if (!"datatype" %in% names(triples)) return(graph)

  literals <- triples[triples$object_type == "literal", , drop = FALSE]
  if (nrow(literals) == 0) return(graph)

  by_predicate <- vapply(
    split(literals$datatype, literals$predicate),
    function(d) {
      d <- unique(d[!is.na(d)])
      if (length(d) == 1) d else NA_character_
    }, character(1))

  by_predicate <- by_predicate[!is.na(by_predicate)]
  if (length(by_predicate) == 0) return(graph)

  lapply(graph, function(node) {
    for (pred in intersect(names(node), names(by_predicate))) {
      node[[pred]] <- coerce_json_literal(node[[pred]], by_predicate[[pred]])
    }
    node
  })
}

# Only types with an unambiguous JSON representation are converted; dates, tags and
# anything unrecognised stay strings, which Fluree reads as xsd:string.
coerce_json_literal <- function(x, datatype) {
  if (!is.character(x) && !is.numeric(x) && !is.logical(x)) return(x)

  converted <- switch(tolower(sub("^.*[:#]", "", datatype)),
    int = , integer = , long = suppressWarnings(as.integer(x)),
    double = , decimal = , float = , numeric = suppressWarnings(as.numeric(x)),
    boolean = ifelse(tolower(as.character(x)) %in% c("true", "1"), TRUE,
               ifelse(tolower(as.character(x)) %in% c("false", "0"), FALSE, NA)),
    return(x)
  )

  # a failed conversion must not silently turn a value into null
  if (anyNA(converted) && !anyNA(x)) x else converted
}

drop_default_types <- function(graph, triples) {
  type_predicates <- c("rdf:type", "@type", "a",
                       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  typed <- unique(triples$subject[
    triples$predicate %in% type_predicates & triples$object_type == "uri"])

  lapply(graph, function(node) {
    id <- node[["@id"]]
    if (!is.null(id) && !id %in% typed && identical(node[["@type"]], "owl:Thing")) {
      node[["@type"]] <- NULL
    }
    node
  })
}
