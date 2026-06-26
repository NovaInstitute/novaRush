
#' Class providing objects with methods to transact to a Fluree instance.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON fromJSON
#' @import flureeCrypto
#' @importFrom httr POST
#'
#' @export
TransactionInstance <- R6::R6Class("TransactionInstance",
  public = list(
    #' @field transaction (`list()`)\cr
    #' The list representation of a transaction to be sent to the Fluree instance.
    transaction = NULL,

    #' @field config (`list()`)\cr
    #' Configuration parameters of the instance.
    config = NULL,

    #' @field signedTransaction (`string`)\cr
    #' The JWT of the transaction.
    signedTransaction = '',

    #' @field endpoint (`string`)\cr
    #' The Fluree v4 endpoint: 'insert', 'upsert', or 'update'.
    endpoint = 'insert',

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param transaction (`list()`)\cr
    #'   The transaction to be sent to the Fluree instance.
    #' @param config (`list()`)\cr
    #'   Configuration parameters of the instance.
    #' @param endpoint (`string`)\cr
    #'   The Fluree v4 endpoint to use: 'insert', 'upsert', or 'update'.
    initialize = function(transaction, config, endpoint = 'insert') {

      self$transaction <- transaction
      self$config <- config
      self$endpoint <- endpoint

      defaultContext <- config$defaultContext %||% list()
      transactionContext <- transaction[["@context"]] %||% list()

      if (!is.null(defaultContext) || !is.null(transactionContext)) {
        self$transaction[['@context']] <- mergeContexts(defaultContext, transactionContext)
      }

      if (isTRUE(config$signMessages)) {
        self$sign()
      }
    },

    #' @description
    #' This method sends the configured transaction to the host.
    #' The Fluree instance must be 'connected' before querying or transacting.
    #' If `signMessages = TRUE` the JWT will be sent to the host.
    send = function() {
      if (nzchar(self$signedTransaction)) {
        contentType <- 'application/jwt'
      } else {
        contentType <- 'application/json'
      }

      params <- generateFetchParams(self$config, self$endpoint, contentType)
      url <- params$url

      body <- if (nzchar(self$signedTransaction)) {
        self$signedTransaction
      } else {
        do.call(jsonlite::toJSON, c(list(x = self$transaction), novaRush:::getDefaultToJSONargs()))
      }

      response <- POST(
        url = url,
        add_headers(`Content-Type` = params$config$headers$`Content-Type`),
        body = body,
        encode = "raw"
      )

      resp_text <- httr::content(response, as = "text", encoding = "UTF-8")
      if (httr::http_error(response)) {
        stop("Transaction failed: ", resp_text)
      }

      do.call(jsonlite::fromJSON, c(list(txt = resp_text), novaRush:::getDefaultFromJSONargs()))
    },

    #' @description
    #' This method is used to sign the configured transaction.
    #' This method is called automatically when `signMessages = TRUE` and requires
    #' a privateKey to be either passed as a parameter or configured within the
    #' `config`.
    #'
    #' @param privateKey (`string`)\cr
    #'   The private key to use for message signing (represented as a hex string).
    #' @return [TransactionInstance]
    sign = function(privateKey = NULL) {
      if (!is.null(privateKey)) {
        key <- privateKey
      } else {
        key <- self$config$privateKey
      }

      if (is.null(key)) {
        stop("privateKey must be provided in either the transaction or the config")
      }

      input <- toJSON(self$transaction, auto_unbox = TRUE, pretty = FALSE)
      signedTransaction <- flureeCrypto:::serialize_jws(as.character(input), key)

      self$signedTransaction <- signedTransaction
      return(self)
    },

    #' @description
    #' Returns the signed transaction (if it has been set).
    #'
    #' @return (`string`).
    getSignedTransaction = function() {
      return(self$signedTransaction)
    },

    #' @description
    #' Returns the transaction body.
    #'
    #' @return (`string`).
    getTransaction = function() {
      return(self$transaction)
    }
  )
)
