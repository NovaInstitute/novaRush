
#' Class providing objects with methods to transact to a Fluree instance.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom flureeCrypto serialize_jws public_key_from_private account_id_from_public
#' @importFrom httr POST
#' 
#' @export
TransactionInstance <- R6Class("TransactionInstance",
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
    
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' 
    #' @param transaction (`list()`)\cr
    #'   The transaction to be sent to the Fluree instance.
    initialize = function(transaction, config) {

      self$transaction <- transaction
      self$config <- config
      
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
      
      params <- generateFetchParams(self$config, 'transact', contentType)
      url <- params$url
      fetchOptions <- params$config
      
      if (isTRUE(self$signedTransaction)) {
        params$body <- self$signedTransaction
      } else {
        params$body <- toJSON(self$transaction, auto_unbox = TRUE, pretty = TRUE)
      }
     
      print(url)
      print(params$config$headers$`Content-Type`)
      print(params$body)
     response <- POST(
       url = url,
       add_headers(`Content-Type` = params$config$headers$`Content-Type`),
       body = params$body,
       encode = "raw"
     )
    
     print(content(response, as = "text"))
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
