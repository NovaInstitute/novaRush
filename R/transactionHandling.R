
#' Configure and Send a Transaction
#' 
#' @description
#' This convenience function configures and sends the passed transaction 
#' by calling the relevant functions.
#' 
#' @inheritParams transact
#' 
#' @seealso [transact()]
#' @seealso [sendTransaction()]
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
#' Transact(config = conf, ledger = 'demo', exampleData, signTransaction = FALSE)
#' 
#' # OR ALTERNATIVELY
#' 
#' dataList <- fromJSON(exampleData, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
#' Transact(config = conf, ledger = 'demo', dataList, signTransaction = FALSE)
#' 
#' @importFrom jsonlite validate
#' @importFrom jsonlite fromJSON
#' 
#' @export
Transact = function(...) {
  t <- transact(...)
  sendTransaction(t)
}


#' Insert Data into a Fluree Ledger
#'
#' @description
#' Convenience wrapper: configures and sends an insert transaction to the
#' Fluree v4 `/insert` endpoint. Adds new triples without touching existing ones.
#'
#' @inheritParams transact
#'
#' @seealso [transact()] [sendTransaction()]
#'
#' @export
Insert = function(...) {
  t <- transact(...)
  sendTransaction(t)
}


#' Conditionally Update Data in a Fluree Ledger
#'
#' @description
#' Convenience wrapper: configures and sends an update transaction to the
#' Fluree v4 `/update` endpoint. The transaction must contain a `where` clause
#' that binds current values, a `delete` clause that retracts them, and an
#' optional `insert` clause that adds new values.
#'
#' @inheritParams transact
#'
#' @seealso [transact()] [sendTransaction()]
#'
#' @export
Update = function(...) {
  t <- transact(...)
  sendTransaction(t)
}


#' Transaction configuration
#' 
#' @description
#' This function configures the transaction as follows: the default context is configured 
#' and if applicable the transaction is signed.
#' The function returns a list containing all the information necessary to interact
#' with the Fluree instance via `sendTransaction()`.
#' 
#' @param config (`list()`)\cr
#'   The configuration list for the Fluree instance.
#' @param ledger (`character`)\cr
#'   The name of the ledger to transact to.
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
#' transactionInstance <- transact(config = conf, ledger = 'demo', exampleData, signTransaction = FALSE)
#' 
#' # OR ALTERNATIVELY
#' 
#' dataList <- fromJSON(exampleData, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
#' transactionInstance <- transact(config = conf, ledger = 'demo', dataList, signTransaction = FALSE)
#' 
#' @importFrom jsonlite validate
#' @importFrom jsonlite fromJSON
#' 
#' @export
transact = function(
    config = NULL, 
    ledger = NULL, 
    transaction, 
    signTransaction = NULL, 
    privateKey = NULL,
    apiKey = NULL) {
  
  ledgerName <- ledger %||% config$ledger
  if (is.null(ledgerName)) {
    stop("Please provide a ledger name. Either as argument or within the config.")
  }
  if (is.null(config)) {
    config = setConfig(ledger = ledger)
  }
  
  if (is.character(transaction)) {
    if (!jsonlite::validate(transaction)) {
      stop("Please provide a valid JSON transaction string", call. = FALSE)
    }
    transaction <- do.call(
      what = jsonlite::fromJSON, 
      args = c(
        list(txt = transaction),
        novaRush:::getDefaultFromJSONargs()), 
      quote = FALSE)
  }
  
  if (is.null(transaction$ledger)) {
    transaction$ledger <- ledgerName
  }
  
  defaultContext <- config$defaultContext %||% list()
  transactionContext <- transaction[["@context"]] %||% list()
  
  if (length(defaultContext) > 0 || length(transactionContext) > 0) {
    transaction[['@context']] <- mergeContexts(defaultContext, transactionContext)
  }
  
  if (!is.null(signTransaction)) {
    shouldSign <- signTransaction
  } else if (!is.null(config$signMessages)) {
    shouldSign <- config$signMessages
  } else {
    shouldSign <- FALSE
  }
  
  body <- list(contentType = 'application/json', txn = transaction)
  
  if (shouldSign) {
    key <- privateKey %||% getKey()
    if (is.null(key)) {
      stop("Please provide a private key for signing.  Either as argument or set one using `setKey()`.")
    }
    body <- list(
      contentType = 'application/jwt', 
      txn = signTransaction(list(configuration = config, transaction = body), privateKey))
  }
  
  if (length(apiKey) == 1) {
    config$apiKey <- apiKey
  }
  
  return(list(configuration = config, transaction = body))
}


#' Delete Subjects by ID
#'
#' @description
#' Deletes all triples for one or more subjects by routing through the Fluree v4
#' `/update` endpoint with a wildcard where/delete pattern.
#' 
#' @param id (`list()`)\cr
#'   The subject identifier/identifiers to retract from the Fluree database.
#' 
#' @returns And instance of a delete transaction (as a list).
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
delete = function(config, id) {
  idAlias <- findIdAlias(config$defaultContext)
  resultingTransaction <- handleDelete(id, idAlias)
  resultingTransaction$ledger <- config$ledger
  
  transact(transaction = resultingTransaction)
}


#' Upsert Into a Fluree Database
#'
#' @description
#' Sends data to the Fluree v4 native `/upsert` endpoint. Replaces the values
#' of every supplied predicate on each subject; predicates not mentioned are
#' left unchanged. Idempotent.
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
#' @importFrom jsonlite validate
#' 
#' @export
upsert = function(config, transaction) {

  if (is.character(transaction)) {
    if (!jsonlite::validate(transaction)) {
      stop("Please provide a valid JSON string", call. = FALSE)
    }
    transaction <- do.call(
      what = jsonlite::fromJSON,
      args = c(
        list(txt = transaction),
        novaRush:::getDefaultFromJSONargs()),
      quote = FALSE)
  }

  defaultContext <- config$defaultContext %||% list()
  txnContext <- transaction[["@context"]] %||% list()
  if (length(defaultContext) > 0 || length(txnContext) > 0) {
    transaction[["@context"]] <- mergeContexts(defaultContext, txnContext)
  }

  body <- list(contentType = 'application/json', txn = transaction)
  params <- generateFetchParams(config = config, endpoint = 'upsert', contentType = 'application/json')
  url <- params$url
  fetchOptions <- params$config

  txnJson <- do.call(
    what = jsonlite::toJSON,
    args = c(list(x = body$txn), novaRush:::getDefaultToJSONargs()),
    quote = FALSE)

  response <- POST(
    url = url,
    config = add_headers(.headers = fetchOptions$headers),
    body = charToRaw(txnJson),
    encode = "raw")

  resp_text <- httr::content(x = response, as = "text", encoding = "UTF-8")
  if (httr::http_error(response)) {
    stop("Upsert failed: ", resp_text)
  }

  do.call(
    what = jsonlite::fromJSON,
    args = c(list(txt = resp_text), novaRush:::getDefaultFromJSONargs()),
    quote = FALSE)
}

#' Send a Transaction
#' 
#' @description
#' This function makes use of `httr` package to send the configured transaction
#' to the Fluree `/transact` API endpoint.
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
    transaction <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = body$txn), 
        novaRush:::getDefaultToJSONargs()), 
      quote = FALSE)
  } else if (contentType == 'application/jwt') {
    transaction <- body$txn
  } else {
    stop("Unsupported content type: ", contentType)
  }
  
  endpoint <- if (!is.null(body$txn$where)) 'update' else 'insert'
  params <- generateFetchParams(
    config = config,
    endpoint = endpoint,
    contentType = contentType)
  url <- params$url
  fetchOptions <- params$config

  response <- POST(
    url = url,
    config = add_headers(.headers = fetchOptions$headers),
    body = charToRaw(transaction),
    encode = "raw")
  
  resp_text <- httr::content(
    x = response, 
    as = "text", 
    encoding = "UTF-8")
  if (httr::http_error(response)) {
    stop("Transaction failed: ", resp_text)
  }
  
  json_response <- do.call(
    what = jsonlite::fromJSON, 
    args = c(
      list(txt = resp_text),
      novaRush:::getDefaultFromJSONargs()), 
    quote = FALSE)

  pretty_json <- do.call(
    what = jsonlite::toJSON, 
    args = c(
      list(x = json_response), 
      novaRush:::getDefaultToJSONargs(pretty = TRUE)), 
    quote = FALSE)
  
  return(pretty_json)
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
    input <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = body$txn), 
        novaRush:::getDefaultToJSONargs()), 
      quote = FALSE)
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
    
    Txn <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = desrialized$payload), 
        novaRush:::getDefaultToJSONargs(pretty = TRUE)), 
      quote = FALSE)
    
  } else {
    
    Txn <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = body$txn), 
        novaRush:::getDefaultToJSONargs(pretty = TRUE)), 
      quote = FALSE)

  }
  
  return(Txn)
}

