#' Run a query and return a tibble
#'
#' @description
#' Sends the assembled query and parses the response. This is the point at which
#' anything is actually transmitted; every `fq_*` verb before it is local.
#'
#' @param x A `fluree_query`.
#' @param ... Unused, for generic consistency.
#'
#' @returns A tibble.
#' @seealso [show_query()] to inspect the query without sending it.
#' @importFrom dplyr collect
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query(con) |>
#'   fq_where(`@id` = "?s", `schema:name` = "?name") |>
#'   fq_select(s, name) |>
#'   collect()
#' }
collect.fluree_query <- function(x, ...) {
  body <- fluree_query_body(x)
  result <- x$con$instance$query(body)$send()
  fluree_result_to_tibble(result, select = x$opts$select)
}

#' Show the JSON-LD Query without running it
#'
#' @description
#' Prints the query as it will be sent. Worth doing whenever a result surprises you,
#' since it shows exactly where the filters ended up.
#'
#' @param x A `fluree_query`.
#' @param ... Unused, for generic consistency.
#'
#' @returns `x`, invisibly.
#' @importFrom dplyr show_query
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query(con) |>
#'   fq_where(`@id` = "?s", `schema:age` = "?age") |>
#'   fq_select(s, age) |>
#'   fq_filter(age > 35) |>
#'   show_query()
#' }
show_query.fluree_query <- function(x, ...) {
  cat(fluree_query_json(x, pretty = TRUE), "\n", sep = "")
  invisible(x)
}

#' The query as a JSON string
#'
#' @param q A `fluree_query`.
#' @param pretty Whether to indent the output.
#'
#' @returns A single string.
#' @export
#'
#' @examples
#' \dontrun{
#' cat(fluree_query_json(fluree_query(con) |> fq_crawl("?s") |>
#'                       fq_where(`@id` = "?s", `schema:name` = "?name")))
#' }
fluree_query_json <- function(q, pretty = TRUE) {
  body <- fluree_query_body(q)
  as.character(do.call(
    jsonlite::toJSON, c(list(x = body), getDefaultToJSONargs(pretty = pretty))))
}

#' @export
print.fluree_query <- function(x, ...) {
  cat("<fluree_query> ", format(x$con), "\n", sep = "")
  cat("  patterns ", length(x$patterns), "  filters ", length(x$filters), "\n", sep = "")

  body <- tryCatch(fluree_query_json(x, pretty = TRUE), error = function(e) NULL)
  if (is.null(body)) {
    cat("  (incomplete: needs at least one fq_where() and an fq_select())\n")
  } else {
    cat(paste0("  ", strsplit(body, "\n")[[1]], collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}

#' @export
format.fluree_query <- function(x, ...) {
  paste0("<fluree_query ", length(x$patterns), " pattern(s), ",
         length(x$filters), " filter(s)>")
}
