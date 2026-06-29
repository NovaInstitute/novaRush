
#' Class providing objects with methods to query a Fluree instance.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON fromJSON
#' @import flureeCrypto
#' @importFrom httr POST
#'
#' @export
QueryInstance <- R6::R6Class("QueryInstance",
  public = list(
    #' @field query (`list()`)\cr
    #' The list representation of a query to be sent to the Fluree instance.
    query = NULL,

    #' @field config (`list()`)\cr
    #' Configuration parameters of the instance.
    config = NULL,

    #' @field signedQuery (`string`)\cr
    #' The JWT of the query.
    signedQuery = '',

    #' @field endpoint (`string`)\cr
    #' The Fluree v4 query endpoint. Always 'query'; SPARQL is detected by content-type.
    endpoint = 'query',

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param query (`list()`)\cr
    #'   The query to be sent to the Fluree instance.
    #' @param config (`list()`)\cr
    #'   Configuration parameters of the instance.
    #' @param endpoint (`string`)\cr
    #'   Ignored — always 'query'. Kept for backward compatibility.
    initialize = function(query, config, endpoint = 'query') {

      self$query <- query
      self$config <- config
      self$endpoint <- 'query'

      if (!is.character(query)) {
        defaultContext <- config$defaultContext %||% list()
        queryContext <- query[["@context"]] %||% list()
        if (length(defaultContext) > 0 || length(queryContext) > 0) {
          self$query[['@context']] <- mergeContexts(defaultContext, queryContext)
        }
      }

      if (isTRUE(config$signMessages) && !is.character(query)) {
        self$sign()
      }
    },

    #' @description
    #' This method sends the configured query to the host.
    #' The Fluree instance must be 'connected' before querying or transacting.
    #' If `signMessages = TRUE` the JWT will be sent to the host.
    send = function() {
      isSparql <- is.character(self$query)

      if (isSparql) {
        ledger <- self$config$ledger
        params <- generateFetchParams(self$config, 'query', 'application/sparql-query', ledger = ledger)
        body <- self$query
      } else if (nzchar(self$signedQuery)) {
        params <- generateFetchParams(self$config, self$endpoint, 'application/jwt')
        body <- self$signedQuery
      } else {
        params <- generateFetchParams(self$config, self$endpoint, 'application/json')
        body <- do.call(jsonlite::toJSON, c(list(x = self$query), novaRush:::getDefaultToJSONargs()))
      }

      response <- POST(
        url = params$url,
        add_headers(`Content-Type` = params$config$headers$`Content-Type`),
        body = body,
        encode = "raw"
      )

      resp_text <- httr::content(response, as = "text", encoding = "UTF-8")
      if (httr::http_error(response)) {
        stop("Query failed: ", resp_text)
      }

      do.call(jsonlite::fromJSON, c(list(txt = resp_text), novaRush:::getDefaultFromJSONargs()))
    },

    #' @description
    #' This method is used to sign the configured query.
    #' This method is called automatically when `signMessages = TRUE` and requires
    #' a privateKey to be either passed as a parameter or configured within the
    #' `config`.
    #'
    #' @param privateKey (`string`)\cr
    #'   The private key to use for message signing (represented as a hex string).
    #' @return [QueryInstance]
    sign = function(privateKey = NULL) {
      if (!is.null(privateKey)) {
        key <- privateKey
      } else {
        key <- self$config$privateKey
      }

      if (is.null(key)) {
        stop("privateKey must be provided in either the transaction or the config")
      }

      input <- toJSON(self$query, auto_unbox = T, pretty = F)
      signedQuery <- flureeCrypto:::serialize_jws(as.character(input), key)

      self$signedQuery <- signedQuery
      return(self)
    },

    #' @description
    #' Returns the signed query (if it has been set).
    #'
    #' @return (`string`).
    getSignedQuery = function() {
      return(self$signedQuery)
    },

    #' @description
    #' Returns the query body.
    #'
    #' @return (`string`).
    getQuery = function() {
      return(self$query)
    }
  )
)
