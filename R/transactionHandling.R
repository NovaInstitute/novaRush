
#' Transaction configuration
#' 
#' @description
#' This function configures the transaction as follows: the default context is configured 
#' and if applicable the transaction is signed.
#' The function returns a list containing all the information necessary to interact
#' with the Fluree instance via `sendTransaction()`.
#' 
#' @param transaction (`list()`)\cr
#'    The list representation of the transaction body to be sent.
#'    Note alternatively the transaction can simply be passed as a JSON `character` string.
#' @param signTransaction (`logical`)\cr
#'   Determines whether the given transaction should be signed or not.
#' @param privateKey (`character`)\cr
#'   The hexstring representation of the private key to use for message signing.
#' 
#' @return A list containing everything needed to transact with Fluree.
#' This includes all the necessary parameters as well as the signed/unsigned transaction itself.
#' 
#' @examples
#' exampleData <- '{
#'    "insert": [
#'       {
#'         "@id": "freddy",
#'         "name": "Freddy"
#'       },
#'       {
#'         "@id": "alice",
#'         "name": "Alice"
#'       }
#'    ]
#' }'
#' 
#' transactionInstance <- transact(exampleData)
#' 
#' # OR ALTERNATIVELY
#' 
#' dataList <- fromJSON(exampleData, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
#' transactionInstance <- transact(dataList)
#' 
#' @export
transact = function(transaction, signTransaction = NULL, privateKey = NULL) {
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before transacting. Try running connect() before attempting to transact", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  
  if (class(transaction) == "character") {
    if (!validate(transaction)) {
      stop("Please provide a valid JSON transaction string", call. = FALSE)
    }
    transaction <- fromJSON(transaction, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  }
  
  if (is.null(transaction$ledger)) {
    transaction$ledger <- config$ledger
  }
  
  defaultContext <- config$defaultContext %||% list()
  transactionContext <- transaction[["@context"]] %||% list()
  
  if (!is.null(defaultContext) || !is.null(transactionContext)) {
    transaction[['@context']] <- mergeContexts(defaultContext, transactionContext)
  }
  
  body <- list(contentType = 'application/json', txn = transaction)
  
  if (isTRUE(config$signMessages) || isTRUE(signTransaction)) {
    body <- list(contentType = 'application/jwt', txn = signTransaction(list(configuration = config, transaction = body), privateKey))
  }
  
  return(list(configuration = config, transaction = body))
}


#' Delete subjects by id
#' 
#' @description
#' Delete is not an API endpoint in Fluree. This function merely transforms
#' a single or list of subject identifier(s) ( @id's ) into a where/delete transaction
#' that deletes the subject and all facts about the subject.
#' 
#' Delete assumes that all facts for the provided subjects should be retracted
#' from the database.
#' 
#' @param id (`list()`)\cr
#'   The subject identifier/identifiers to retract from the Fluree instance.
#' 
#' @examples
#' # Existing data:
#' #  [
#' #    { "@id": "freddy", "name": "Freddy" },
#' #    { "@id": "alice", "name": "Alice" }
#' #  ]
#' 
#' deleteInstance <- delete(c("freddy"))
#' sendTransaction(deleteInstance)
#' 
#' # New data state after txn:
#' #  [
#' #    { "@id": "alice", "name": "Alice" }
#' #  ]
#' 
#' @export
delete = function(id) {
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before transacting. Try running connect() before attempting to delete", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  idAlias <- findIdAlias(config$defaultContext)
  resultingTransaction <- handleDelete(id, idAlias)
  resultingTransaction$ledger <- config$ledger
  
  transact(transaction = resultingTransaction)
}


#' Upsert into the Fluree database
#' 
#' @description
#' Upsert is not an API endpoint in Fluree. This function merely transforms
#' an upsert transaction into an insert/where/delete transaction.
#' 
#' Upsert assumes that the facts provided in the transaction should be treated
#' as the true & accurate state of the data after the transaction is processed.
#' i.e. the facts in the transaction should be inserted (if new) and should
#' replace existing facts (if they already exist on those subjects & properties).
#' 
#' @param transaction (`list()`)\cr
#'   The upsert transaction to send to the Fluree instance.
#'   Note alternatively the transaction can simply be passed as a JSON `character` string.
#' 
#' @examples
#' # Existing data:
#' #  [
#' #    { "@id": "freddy", "name": "Freddy" },
#' #    { "@id": "alice", "name": "Alice" }
#' #  ]
#' 
#' upsertData <- '{
#'    "insert": [
#'       {
#'         "@id": "freddy",
#'         "name": "Freddy the Yeti"
#'       },
#'       {
#'         "@id": "alice",
#'         "age": "25"
#'       }
#'    ]
#' }'
#' 
#' upsertInstance <- upsert(upsertData)
#' 
#' # OR ALTERNATIVELY
#' 
#' dataList <- fromJSON(upsertData, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
#' upsertInstance <- transact(dataList)
#' 
#' sendTransaction(upsertInstance)
#' 
#' # New data state after txn:
#' #  [
#' #    { "@id": "freddy", "name": "Freddy the Yeti" },
#' #    { "@id": "alice", "name": "Alice", "age": 25 }
#' #  ]
#' 
#' @export
upsert = function(transaction) {
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before transacting. Try running connect() before attempting to upsert", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  
  if (class(transaction) == "character") {
    if (!validate(transaction)) {
      stop("Please provide a valid JSON string", call. = FALSE)
    }
    transaction <- fromJSON(transaction, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  }
  
  idAlias <- findIdAlias(config$defaultContext)
  resultingTransaction <- handleUpsert(transaction, idAlias)
  resultingTransaction$ledger <- config$ledger
  
  transact(transaction = resultingTransaction)
}

#' Send a transaction
#' 
#' @description
#' This function makes use of `httr` to send the configured transaction to the 
#' Fluree instance.
#' 
#' @param transactionVariables (`list()`)\cr
#'   A list representing the transaction specifications.
#' 
#' @return A character string containing the response content.
#' 
#' @examples
#' transactionInstance <- transact(exampleData)
#' sendTransaction(transactionInstance)
#' 
#' @importFrom httr POST
#' 
#' @export
sendTransaction = function(transactionVariables) {
  
  config <- transactionVariables$configuration
  body <- transactionVariables$transaction
  
  contentType <- body$contentType
  
  if (contentType == 'application/json') {
    transaction <- toJSON(body$txn, auto_unbox = TRUE, pretty = FALSE)
  } else {
    transaction <- body$txn
  }
  
  params <- generateFetchParams(config, 'transact', contentType)
  url <- params$url
  fetchOptions <- params$config
  
  params$body <- transaction
  
  response <- POST(
    url = url,
    add_headers(`Content-Type` = params$config$headers$`Content-Type`),
    body = params$body,
    encode = "raw"
  )
  
  return(content(response, as = "text"))
}

#' Sign a transaction
#' 
#' @description
#' This function is used to sign a transaction which can then be sent to the Fluree
#' instance as a JWT (JSON Web Token).
#' 
#' @param transactionVariables (`list()`)\cr
#'   A list representing the transaction to be signed.
#' @param privateKey (`character`)\cr
#'   The hexadecimal string representation of the private key to be used for signing.
#'   If a private key is not explicitly provided, the one stored as an environment variable
#'   will be used (if one had been configured).
#' 
#' @return A list containing everything needed to transact with Fluree.
#' This includes all the necessary parameters as well as the signed transaction itself.
#' 
#' @examples
#' transactionInstance <- transact(exampleData)
#' signedTransactionInstance <- signTransaction(transactionInstance)
#' 
#' @export
signTransaction = function(transactionVariables = NULL, privateKey = NULL) {

  if (is.null(transactionVariables)) {
    stop("Please provide the transaction to be signed", call. = FALSE)
  }
  
  if (!is.null(privateKey)) {
    key <- privateKey
  } else {
    config <- fromJSON(Sys.getenv("config"))
    key <- config$privateKey
  }
  
  if (is.null(key)) {
    stop("privateKey must be provided either as a parameter or in the configuration", call. = FALSE)
  }
  
  config <- transactionVariables$configuration
  body <- transactionVariables$transaction
  
  contentType <- body$contentType
  
  if (contentType == 'application/jwt') {
    stop("The provided transaction has already been signed", call. = FALSE)
  } else {
    input <- toJSON(body$txn, auto_unbox = TRUE, pretty = FALSE)
  }
  
  signedTransaction <- flureeCrypto:::serialize_jws(as.character(input), key)
  
  body$contentType <- 'application/jwt'
  body$txn <- signedTransaction
  
  return(list(configuration = config, transaction = body))
}

#' Get the signed transaction
#' 
#' @description
#' This function returns the JWT representation of the signed transaction.
#' Note this function can only be used if a private key had been configured and 
#' the transaction has been signed.
#' 
#' @returns Character string representing the JWT of the signed transaction.
#' 
#' @examples
#' transactionInstance <- transact(exampleData)
#' signedTransactionInstance <- signTransaction(transactionInstance)
#' 
#' sig <- getTransactionSignature(signedTransactionInstance)
#' 
#' @export
getTransactionSignature = function(transactionVariables = NULL) {
  if (is.null(transactionVariables)) {
    stop("Please provide a valid transaction instance", call. = FALSE)
  }
  
  body <- transactionVariables$transaction
  contentType <- body$contentType
  
  if (contentType != "application/jwt") {
    stop("The provided transaction has not yet been signed. Sign the transaction using 'signTransaction()'
         before attempting to extract the signature", call. = FALSE)
  }
  
  signedTxn <- body$txn
  return(signedTxn)
}

#' Get the transaction
#' 
#' @description
#' This function returns the transaction body as a JSON string.
#' 
#' @returns JSON string representation of the transaction body
#' 
#' @examples
#' transactionInstance <- transact(exampleData)
#' 
#' txn  <- getTransactionText(transactionInstance)
#' 
#' @export
getTransactionText = function(transactionVariables = NULL) {
  if (is.null(transactionVariables)) {
    stop("Please provide a valid transaction instance", call. = FALSE)
  }
  
  body <- transactionVariables$transaction
  contentType <- body$contentType
  
  if (contentType == "application/jwt") {
    jwt <- body$txn
    desrialized <- flureeCrypto:::deserialize_jws(jwt)
    Txn <- toJSON(desrialized$payload, auto_unbox = TRUE, pretty = TRUE)
  } else {
    Txn <- toJSON(body$txn, auto_unbox = TRUE, pretty = TRUE)
  }
  
  return(Txn)
}

