library(R6)
library(flureeCrypto)

TransactionInstance <- R6Class(
  "TransactionInstance",
  public = list(
    transaction = NULL,
    config = NULL,
    signedTransaction = '',
    
    # constructor method
    initialize = function(transaction, config) {

      self$transaction <- transaction
      self$config <- config
      
      # merge contexts if applicable
      defaultContext <- config$defaultContext %||% list()
      transactionContext <- transaction[["@context"]] %||% list()
      
      if (!is.null(defaultContext) || !is.null(transactionContext)) {
        self$transaction[['@context']] <- mergeContexts(defaultContext, transactionContext)
      }
      
      if (config$signMessages) {
        self$sign()
      }
    },
    
    send = function() {
      if (!is.null(self$signedTransaction)) {
        contentType <- 'application/jwt'
      } else {
        contentType <- 'application/json'
      }
      
      params <- generateFetchParams(self$config, 'transact', contentType)
      url <- params$url
      fetchOptions <- params$config
      
      if (!is.null(self$signedTransaction)) {
        params$body <- self$signedTransaction
      } else {
        JSONbody <- toJSON(self$transaction, auto_unbox = TRUE, pretty = TRUE)
      }
     
     response <- POST(
       url = url,
       add_headers(`Content-Type` = params$config$headers$`Content-Type`),
       body = params$body,
       encode = "raw"
     )
    
     print(content(response, as = "text"))
    },

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

    getSignedTransaction = function() {
      return(self$signedTransaction)
    },

    getTransaction = function() {
      return(self$transaction)
    }
  )
)
