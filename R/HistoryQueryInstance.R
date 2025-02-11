
#' Class providing objects with methods to perform history queries on a Fluree instance.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON fromJSON
#' @import flureeCrypto
#' @importFrom httr POST
#'
#' @export
HistoryQueryInstance <- R6Class("HistoryQueryInstance",
  public = list(
    #' @field query (`list()`)\cr
    #' The list representation of a history query to be sent to the Fluree instance.
    query = NULL,

    #' @field config (`list()`)\cr
    #' Configuration parameters of the instance.
    config = NULL,

    #' @field signedQuery (`string`)\cr
    #' The JWT of the history query.
    signedQuery = '',

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param query (`list()`)\cr
    #'   The history query to be sent to the Fluree instance.
    #' @param config (`list()`)\cr
    #'   Configuration parameters of the instance.
    initialize = function(query, config) {

      if (is.null(query$history) && is.null(query[['commit-details']])) {
        stop('either the history or commit-details key is required', call. = FALSE)
      }

      self$query <- query
      self$config <- config

      if (isTRUE(config$signMessages)) {
        self$sign()
      }
    },

    #' @description
    #' This method sends the configured history query to the host.
    #' The Fluree instance must be 'connected' before querying or transacting.
    #' If `signMessages = TRUE` the JWT will be sent to the host.
    send = function() {
      if (nzchar(self$signedQuery)) {
        contentType <- 'application/jwt'
      } else {
        contentType <- 'application/json'
      }

      params <- generateFetchParams(self$config, 'history', contentType)
      url <- params$url
      fetchOptions <- params$config

      if (nzchar(self$signedQuery)) {
        params$body <- self$signedQuery
      } else {
        params$body <- toJSON(self$query, auto_unbox = TRUE, pretty = TRUE)
      }

      response <- POST(
        url = url,
        add_headers(`Content-Type` = params$config$headers$`Content-Type`),
        body = params$body,
        encode = "raw"
      )

      print(content(response, as = "text"))
    },

    #' @description
    #' This method is used to sign the configured history query.
    #' This method is called automatically when `signMessages = TRUE` and requires
    #' a privateKey to be either passed as a parameter or configured within the
    #' `config`.
    #'
    #' @param privateKey (`string`)\cr
    #'   The private key to use for message signing (represented as a hex string).
    #' @return [HistoryQueryInstance]
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
      signedHistoryQuery <- flureeCrypto:::serialize_jws(as.character(input), key)

      self$signedQuery <- signedHistoryQuery
      return(self)
    },

    #' @description
    #' Returns the signed history query (if it has been set).
    #'
    #' @return (`string`).
    getSignedQuery = function() {
      return(self$signedQuery)
    },

    #' @description
    #' Returns the history query body.
    #'
    #' @return (`string`).
    getQuery = function() {
      return(self$query)
    }
  )
)
