#' Run a SPARQL query
#'
#' @description
#' Sends a SPARQL 1.1 query to the ledger and returns the bindings as a tibble.
#' Fluree routes by content type, so this posts the query string with
#' `Content-Type: application/sparql-query`.
#'
#' This is also the seam for a SPARQL query builder. The `glitter` package composes
#' SPARQL with tidyverse-style verbs and `glitter::spq_assemble()` returns the query
#' as a string, so its output can be piped straight in - no dependency on it either
#' way.
#'
#' @param con A `fluree_connection`.
#' @param query A SPARQL query, as a single string.
#' @param reasoning Optional reasoning mode: `"rdfs"`, `"owl2ql"`, `"owl2rl"`,
#'   `"datalog"` or `"none"`.
#'
#' @returns A tibble, one column per projected variable, with values coerced from
#'   the datatypes in the response.
#' @seealso [fluree_query()] for the native JSON-LD Query interface.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_sparql(con, "
#'   PREFIX schema: <http://schema.org/>
#'   SELECT ?name ?age
#'   WHERE { ?s a schema:Person ; schema:name ?name ; schema:age ?age . }
#'   ORDER BY ?name
#' ")
#'
#' # composed with glitter
#' library(glitter)
#' spq_init() |>
#'   spq_add("?s schema:name ?name") |>
#'   spq_head(10) |>
#'   spq_assemble() |>
#'   fluree_sparql(con = con)
#' }
fluree_sparql <- function(con, query, reasoning = NULL) {
  check_connection(con)
  if (!is.character(query) || length(query) != 1) {
    stop("`query` must be a single SPARQL string.", call. = FALSE)
  }

  result <- con$instance$sparql(query, reasoning = reasoning)$send()
  fluree_bindings_to_tibble(result)
}

#' The commit history of a ledger
#'
#' @description
#' Returns the commit log as a tibble - one row per commit, with the transaction
#' number and commit identifier - which is what you need to pick a point to travel
#' back to with [fq_at()].
#'
#' @param con A `fluree_connection`.
#' @param limit Maximum number of commits to return.
#'
#' @returns A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_history(con, limit = 5)
#' }
fluree_history <- function(con, limit = NULL) {
  check_connection(con)
  query <- if (is.null(limit)) list() else list(limit = as.integer(limit))
  fluree_result_to_tibble(con$instance$history(query)$send())
}
