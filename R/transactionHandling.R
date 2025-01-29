
#' Transaction configuration
#' 
#' @description
#' This function configures the transaction instance. System variables are set,
#' the default context is configured and if applicable the transaction is signed.
#' 
#' @param transaction A list representing the body of the transaction to be sent
#' 
#' @export
transact = function(transaction) {
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before transacting. Try running connect() before transacting", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  if (is.null(transaction$ledger)) {
    transaction$ledger <- config$ledger
  }
  
  # merge contexts if applicable
  defaultContext <- config$defaultContext %||% list()
  transactionContext <- transaction[["@context"]] %||% list()
  
  if (!is.null(defaultContext) || !is.null(transactionContext)) {
    transaction[['@context']] <- mergeContexts(defaultContext, transactionContext)
  }
  
  json_txn <- toJSON(transaction, auto_unbox = TRUE)
  Sys.setenv(transaction = json_txn)
  
  if (isTRUE(config$signMessages)) {
    signTransaction(transaction)
  }
}


delete = function(id) {
  print('In delete method...')
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before transacting. Try using $connect()$delete() instead", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  idAlias <- findIdAlias(config$defaultContext)
  resultingTransaction <- handleDelete(id, idAlias)
  resultingTransaction$ledger <- config$ledger
  
  print("successful past delete")
  transact(transaction = resultingTransaction)
}

#' Send a transaction
#' 
#' @description
#' This function makes use of httr to send the transaction to the Fluree instance. 
#' The response is then given as output.
#' 
#' @param config A list of configuration parameters for the transaction
#' 
#' @export
sendTransaction = function(config) {
  signedTransaction <- Sys.getenv("signedTransaction")
  if (nzchar(signedTransaction)) {
    contentType <- 'application/jwt'
  } else {
    contentType <- 'application/json'
  }
  
  transaction <- fromJSON(Sys.getenv("transaction"), simplifyVector = F, simplifyDataFrame = T, simplifyMatrix = F)
  
  params <- generateFetchParams(config, 'transact', contentType)
  url <- params$url
  fetchOptions <- params$config
  
  if (nzchar(signedTransaction)) {
    params$body <- signedTransaction
  } else {
    params$body <- toJSON(transaction, auto_unbox = T, pretty = F)
  }
  
  response <- POST(
    url = url,
    add_headers(`Content-Type` = params$config$headers$`Content-Type`),
    body = params$body,
    encode = "raw"
  )
  
  Sys.unsetenv("query")
  Sys.unsetenv("signedQuery")
  
  print(content(response, as = "text"))
}

#' Sign a transaction
#' 
#' @description
#' This function is used to sign a transaction which can then be sent to the Fluree
#' instance as a JWT (JSON Web Token).
#' 
#' @param transaction A list representing the transaction to be signed
#' @param privateKey The hexadecimal string representation of the private key to be used for signing
#' 
#' @export
signTransaction = function(transaction = NULL, privateKey = NULL) {

  if (is.null(transaction)) {
    transaction <- fromJSON(Sys.getenv("transaction"))
  }
  
  if (!is.null(privateKey)) {
    key <- privateKey
  } else {
    config <- fromJSON(Sys.getenv("config"))
    key <- config$privateKey
  }
  
  if (is.null(key)) {
    stop("privateKey must be provided in either the transaction or the config")
  }
  
  input <- toJSON(transaction, auto_unbox = T, pretty = F)
  signedTransaction <- flureeCrypto:::serialize_jws(as.character(input), key)
  
  Sys.setenv(signedTransaction = signedTransaction)
}

#' Get the signed transaction
#' 
#' @description
#' This function returns the JWT representation of the signed transaction.
#' Note this function can only be used if a private key has been configured and 
#' the transaction has already been signed.
#' 
#' @returns String representation of the JWT of the signed transaction
#' 
#' @export
getTransactionSignature = function() {
  signedTxn <- Sys.getenv("signedTransaction")
  return(signedTxn)
}

#' Get the transaction
#' 
#' @description
#' This function returns the transaction body as a JSON string.
#' 
#' @returns JSON string representation of the transaction body
#' 
#' @export
getTransactionText = function() {
  Txn <- Sys.getenv("transaction")
  return(Txn)
}

