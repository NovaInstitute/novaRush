
#' Class for querying the commit log of a Fluree ledger.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET add_headers
#'
#' @export
HistoryQueryInstance <- R6::R6Class("HistoryQueryInstance",
  public = list(
    #' @field query (`list()`)\cr
    #' Optional parameters: `limit` (integer), `from` (start t), `to` (end t).
    query = NULL,

    #' @field config (`list()`)\cr
    #' Configuration parameters of the instance.
    config = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param query (`list()`)\cr
    #'   Optional parameters for the log request: `limit`, `from` (start t), `to` (end t).
    #' @param config (`list()`)\cr
    #'   Configuration parameters of the instance.
    initialize = function(query = list(), config) {
      self$query <- query
      self$config <- config
    },

    #' @description
    #' Fetch the commit log for the configured ledger.
    #' Returns a list with `ledger_id`, `commits` (each having `t`, `commit_id`, `time`,
    #' `asserts`, `retracts`, `flake_count`), `count`, and `truncated`.
    send = function() {
      ledger <- self$query$from %||% self$config$ledger
      if (is.null(ledger)) {
        stop("Ledger is required for history queries", call. = FALSE)
      }

      params <- generateFetchParams(self$config, paste0("log/", ledger))
      url <- params$url

      qp <- list()
      if (!is.null(self$query$limit)) qp$limit <- self$query$limit
      if (!is.null(self$query[["from-t"]])) qp[["from-t"]] <- self$query[["from-t"]]
      if (!is.null(self$query[["to-t"]])) qp[["to-t"]] <- self$query[["to-t"]]

      if (length(qp) > 0) {
        qs <- paste(
          mapply(function(k, v) paste0(k, "=", v), names(qp), qp),
          collapse = "&"
        )
        url <- paste0(url, "?", qs)
      }

      response <- httr::GET(url, add_headers(.headers = params$config$headers))

      resp_text <- httr::content(response, as = "text", encoding = "UTF-8")
      if (httr::http_error(response)) {
        stop("History query failed: ", resp_text)
      }

      do.call(jsonlite::fromJSON, c(list(txt = resp_text), novaRush:::getDefaultFromJSONargs()))
    },

    #' @description
    #' Returns the query parameters.
    #'
    #' @return (`list()`).
    getQuery = function() {
      return(self$query)
    }
  )
)
