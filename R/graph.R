#' Pull a ledger extract as a tidygraph graph
#'
#' @description
#' Queries the ledger for whole nodes and converts the result into a
#' [tidygraph::tbl_graph()] via [semanticModelR::triples_to_tbl_graph()], so a
#' subgraph can be filtered, traversed and measured with dplyr verbs and plotted
#' with ggraph.
#'
#' By default it fetches every subject. Narrow it with `subject_type`, or pass a
#' query you built yourself.
#'
#' @param con A `fluree_connection`.
#' @param subject_type Optional class IRI: keep only subjects of this type.
#' @param limit Optional maximum number of subjects.
#' @param query Optionally a `fluree_query` to use instead of the default. It must
#'   return whole nodes, so build it with [fq_crawl()].
#' @param typed Passed to [semanticModelR::triples_to_tbl_graph()]. `TRUE` coerces
#'   literal node attributes using their datatypes.
#'
#' @returns A `tbl_graph`.
#' @seealso [semanticModelR::rdf_nodes_of_type()],
#'   [semanticModelR::rdf_neighbourhood()], [semanticModelR::rdf_ggraph()]
#' @export
#'
#' @examples
#' \dontrun{
#' g <- fluree_graph(con, subject_type = "survey:Survey")
#'
#' g |>
#'   tidygraph::activate(nodes) |>
#'   dplyr::filter(!is.na(`rdfs:label`)) |>
#'   semanticModelR::rdf_ggraph()
#'
#' # round trip: work on the graph, write it back
#' fluree_insert(con, g)
#' }
fluree_graph <- function(con,
                         subject_type = NULL,
                         limit = NULL,
                         query = NULL,
                         typed = FALSE) {
  check_connection(con)

  q <- query %||% default_graph_query(con, subject_type, limit)
  nodes <- collect(q)

  semanticModelR::triples_to_tbl_graph(nodes_to_triples(nodes), typed = typed)
}

default_graph_query <- function(con, subject_type, limit) {
  q <- fluree_query(con)
  q <- if (is.null(subject_type)) {
    fq_where(q, `@id` = "?s", `?p` = "?o")
  } else {
    fq_where(q, `@id` = "?s", `@type` = subject_type)
  }
  q <- fq_crawl(q, "?s")
  if (!is.null(limit)) q <- fq_limit(q, limit)
  q
}

# A crawl result is one row per node, one column per predicate. Melt it back to
# triples so the graph conversion has the uri/literal distinction to work with.
nodes_to_triples <- function(nodes) {
  if (nrow(nodes) == 0 || !"@id" %in% names(nodes)) {
    return(semanticModelR::rdf_triples(character(), character(), character(), character()))
  }

  subjects <- as.character(nodes[["@id"]])
  predicates <- setdiff(names(nodes), "@id")

  rows <- lapply(predicates, function(pred) {
    cells <- nodes[[pred]]
    per_row <- lapply(seq_along(subjects), function(i) {
      flatten_cell(if (is.list(cells)) cells[[i]] else cells[[i]])
    })
    n <- vapply(per_row, nrow, integer(1))
    if (sum(n) == 0) return(NULL)

    out <- dplyr::bind_rows(per_row)
    out$subject <- rep(subjects, n)
    out$predicate <- if (identical(pred, "@type")) "rdf:type" else pred
    out
  })

  out <- dplyr::bind_rows(rows)
  if (is.null(out) || nrow(out) == 0) {
    return(semanticModelR::rdf_triples(character(), character(), character(), character()))
  }

  semanticModelR::rdf_triples(
    subject     = out$subject,
    predicate   = out$predicate,
    object      = out$object,
    object_type = out$object_type
  )
}

# One cell of a crawl result: a scalar literal, an {"@id": ...} reference, or a
# list of either.
flatten_cell <- function(cell) {
  empty <- tibble::tibble(object = character(), object_type = character())

  if (is.null(cell) || (length(cell) == 1 && !is.list(cell) && is.na(cell))) {
    return(empty)
  }

  if (is.list(cell) && !is.null(cell[["@id"]])) {
    return(tibble::tibble(object = as.character(cell[["@id"]]), object_type = "uri"))
  }

  if (is.list(cell)) {
    parts <- lapply(cell, flatten_cell)
    return(dplyr::bind_rows(parts))
  }

  tibble::tibble(
    object = as.character(cell),
    object_type = if (semanticModelR::is_iri(as.character(cell))) "uri" else "literal"
  )
}
