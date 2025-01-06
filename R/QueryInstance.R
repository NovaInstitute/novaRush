library(R6)
library(flureeCrypto)

QueryInstance <- R6Class(
  "QueryInstance",
  public = list(
    query = NULL,
    config = NULL,
    signedQuery = '',
    
    # constructor method
    initialize = function(query, config) {
      
      self$query <- query
      self$config <- config
      
      # merge contexts if applicable
      defaultContext <- config$defaultContext %||% list()
      queryContext <- query[["@context"]] %||% list()
      
      if (!is.null(defaultContext) || !is.null(queryContext)) {
        self$query[['@context']] <- merge_contexts(defaultContext, transactionContext)
      }
      
      if (config$signMessages) {
        self$sign()
      }
    },
    
    send = function() {
      
    }
    
  ))