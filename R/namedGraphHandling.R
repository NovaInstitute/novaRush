#' Validate a named-graph IRI
#'
#' @param graph Absolute graph IRI.
#' @return `graph`, invisibly, when valid.
#' @keywords internal
validateGraphIri <- function(graph) {
  if (length(graph) != 1L || is.na(graph) || !nzchar(graph) ||
      !grepl("^[A-Za-z][A-Za-z0-9+.-]*:[^[:space:]]+$", graph)) {
    stop("`graph` must be one absolute IRI.", call. = FALSE)
  }
  invisible(graph)
}

.jsonld_document_parts <- function(document) {
  if (!is.list(document)) {
    stop("`document` must be a JSON-LD list.", call. = FALSE)
  }
  if (!is.null(document[["@graph"]])) {
    nodes <- document[["@graph"]]
    context <- document[["@context"]]
  } else if (!is.null(document[["@id"]])) {
    nodes <- list(document)
    context <- document[["@context"]]
    nodes[[1L]][["@context"]] <- NULL
  } else if (length(document) == 0L ||
             all(vapply(document, is.list, logical(1)))) {
    nodes <- document
    context <- NULL
  } else {
    stop(paste(
      "`document` must contain `@graph`, be one JSON-LD resource,",
      "or be a list of resources."
    ), call. = FALSE)
  }
  if (!all(vapply(nodes, function(node) {
    is.list(node) && !is.null(node[["@id"]])
  }, logical(1)))) {
    stop("Every named-graph resource must contain `@id`.", call. = FALSE)
  }
  list(context = context, nodes = nodes)
}

#' Place JSON-LD resources in a named graph
#'
#' Adds Fluree's JSON-LD resource-placement directive to every top-level
#' resource while retaining the document context. Existing placement in the
#' same graph is accepted; conflicting placement is rejected.
#'
#' @param document JSON-LD document, resource, or list of resources.
#' @param graph Absolute named-graph IRI.
#'
#' @return A JSON-LD document containing `@context` when supplied and `@graph`.
#' @export
namedGraphDocument <- function(document, graph) {
  validateGraphIri(graph)
  parts <- .jsonld_document_parts(document)
  nodes <- lapply(parts$nodes, function(node) {
    placement <- node[["@graph"]]
    if (!is.null(placement) && !identical(placement, graph)) {
      stop(paste0(
        "Resource '", node[["@id"]], "' is already assigned to named graph '",
        placement, "'."
      ), call. = FALSE)
    }
    node[["@graph"]] <- graph
    node
  })
  result <- list()
  if (!is.null(parts$context)) result[["@context"]] <- parts$context
  result[["@graph"]] <- nodes
  result
}

#' Upsert JSON-LD resources into a named graph
#'
#' @param document JSON-LD document, resource, or list of resources.
#' @param graph Absolute named-graph IRI.
#' @param config Fluree configuration created by [setConfig()].
#' @param branch Target branch. Defaults to `config$branch`.
#'
#' @return The parsed Fluree transaction receipt.
#' @export
upsertNamedGraph <- function(document, graph, config,
                             branch = config$branch) {
  validateBranchName(branch)
  placed <- namedGraphDocument(document, graph)
  fluree_request(
    config,
    endpoint = "upsert",
    method = "POST",
    body = placed,
    query = list(ledger = flureeLedgerRef(config$ledger, branch)),
    operation = paste0("upsert named graph ", graph),
    write = TRUE
  )
}

#' Query one named graph
#'
#' Restricts a JSON-LD query to one user-defined graph within a selected ledger
#' branch using Fluree's structured `from` source.
#'
#' @param query JSON-LD query list.
#' @param graph Absolute named-graph IRI.
#' @param config Fluree configuration created by [setConfig()].
#' @param branch Source branch. Defaults to `config$branch`.
#'
#' @return Parsed query results from Fluree.
#' @export
queryNamedGraph <- function(query, graph, config,
                            branch = config$branch) {
  validateGraphIri(graph)
  validateBranchName(branch)
  if (!is.list(query)) {
    stop("`query` must be a JSON-LD query list.", call. = FALSE)
  }
  ledger <- flureeLedgerRef(config$ledger, branch)
  source <- list("@id" = ledger, graph = graph)
  if (!is.null(query$from) && !identical(query$from, source)) {
    stop("`query$from` conflicts with the requested named graph.", call. = FALSE)
  }
  query$from <- source
  defaultContext <- config$defaultContext %||% list()
  queryContext <- query[["@context"]] %||% list()
  if (length(defaultContext) || length(queryContext)) {
    query[["@context"]] <- mergeContexts(defaultContext, queryContext)
  }
  fluree_request(
    config,
    endpoint = "query",
    method = "POST",
    body = query,
    operation = paste0("query named graph ", graph),
    write = FALSE
  )
}
