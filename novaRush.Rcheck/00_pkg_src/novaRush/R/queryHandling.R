
#' Configure and Send a Query
#' 
#' @description
#' This convenience function configures and sends the passed query by calling
#' the relevant functions.
#' 
#' @inheritParams query
#' 
#' @seealso [query()]
#' @seealso [sendQuery()]
#' 
#' @examples
#' # Existing data:
#' #  [
#' #    { "@id": "freddy", "name": "Freddy" },
#' #    { "@id": "alice", "name": "Alice" }
#' #  ]
#' 
#' exampleQuery <- '{
#'      "select": {
#'        "?s": ["*"]
#'      },
#'      "where": {
#'        "@id": "?s",
#'        "name": "?name"
#'      }
#' }'
#' 
#' Query(config = conf, ledger = 'demo', exampleQuery, signQuery = FALSE)
#' 
#' # OR ALTERNATIVELY
#' 
#' queryList <- fromJSON(exampleQuery, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
#' Query(config = conf, ledger = 'demo', queryList, signQuery = FALSE)
#' 
#' @importFrom jsonlite validate
#' @importFrom jsonlite fromJSON
#' 
#' @export
Query = function(...) {
  q <- query(...)
  sendQuery(q)
}



#' Query Configuration
#' 
#' @description
#' This function configures the query instance. 
#' The default context is added and if applicable the query is signed.
#' The function returns a list referred to as `queryVariables` which is a single
#' structure containing all the information necessary to interact with the
#' Fluree instance via `sendQuery()`.
#' 
#' @param config (`list()`)\cr
#'   The configuration list for the Fluree instance.
#' @param ledger (`character`)\cr
#'   The name of the ledger to query.
#' @param query (`list()`)\cr
#'   The list representation of the query body to be sent.
#'   Alternatively the query can simply be passed as a JSON `character` string.
#' @param signQuery (`logical`)\cr
#'   Determines whether the given query should be signed or not.
#'   This will override the value set in `config$signMessages`.
#' @param privateKey (`character`)\cr
#'   The hexstring representation of the private key to use for message signing.
#'   Overrides `getKey()` if provided.
#' @param apiKey (`character`)\cr
#'   The API key for accessing the Fluree ledger on the remote host (e.g., when
#'   the host is data.flur.ee).
#' 
#' @return A list containing everything needed to query the Fluree database.
#' This includes all the necessary parameters as well as the signed/unsigned query itself.
#' 
#' @examples
#' # Existing data:
#' #  [
#' #    { "@id": "freddy", "name": "Freddy" },
#' #    { "@id": "alice", "name": "Alice" }
#' #  ]
#' 
#' exampleQuery <- '{
#'      "select": {
#'        "?s": ["*"]
#'      },
#'      "where": {
#'        "@id": "?s",
#'        "name": "?name"
#'      }
#' }'
#' 
#' queryInstance <- query(exampleQuery)
#' 
#' # OR ALTERNATIVELY
#' 
#' queryList <- fromJSON(exampleQuery, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
#' queryInstance <- query(queryList)
#' 
#' @importFrom jsonlite validate
#' @importFrom jsonlite fromJSON
#' @export
query = function(
    config = NULL, 
    ledger = NULL, 
    query, 
    signQuery = NULL, 
    privateKey = NULL, 
    apiKey = NULL) {
  
  ledgerName <- ledger %||% config$ledger
  if (is.null(ledgerName)) {
    stop("Please provide a ledger name. Either as argument or within the config.")
  }
  if (is.null(config)) {
    config = setConfig(ledger = ledger)
  }

  if (is.character(query)) {
    if (!jsonlite::validate(query)) {
      stop("Please provide a valid JSON query string", call. = FALSE)
    }
    query <- do.call(
      what = jsonlite::fromJSON, 
      args = c(
        list(txt = query),
        novaRush:::getDefaultFromJSONargs()), 
      quote = FALSE)
  }
  
  if (is.null(query$from)) {
    query$from <- ledgerName
  }
  
  # merge contexts if applicable
  defaultContext <- config$defaultContext %||% list()
  queryContext <- query[["@context"]] %||% list()
  
  if (length(defaultContext) > 0 || length(queryContext) > 0) {
    query[['@context']] <- mergeContexts(defaultContext, queryContext)
  }
  
  if (!is.null(signQuery)) {
    shouldSign = signQuery
  } else if (!is.null(config$signMessages)) {
    shouldSign = config$signMessages
  } else {
    shouldSign = FALSE
  }
  
  body <- list(contentType = 'application/json', qry = query)
  
  if (shouldSign) {
    key <- privateKey %||% getKey()
    if (is.null(key)) {
      stop("Please provide a private key for signing. Either as argument or set one using `setKey()`.", call. = FALSE)
    } else {
      body <- list(
        contentType = 'application/jwt', 
        qry = signQuery(list(configuration = config, qry = body), key))
    }
  }
  
  if (length(apiKey) == 1) {
    config$apiKey <- apiKey
  }
  
  return(list(configuration = config, query = body))
}

#' Send a Query
#' 
#' @description
#' This function makes use of the `httr` package to send the configured query to
#' the Fluree `/query` API endpoint.
#' 
#' @param queryVariables (`list()`)\cr
#'   A list representing the query specifications. This specification is the
#'   result of the `query()` function call.
#' 
#' @return A character string containing the JSON response content.
#' 
#' @examples
#' queryInstance <- query(exampleQuery)
#' sendQuery(queryInstance)
#' 
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#' 
#' @importFrom httr POST
#' 
#' @export
sendQuery = function(queryVariables) {
  
  config <- queryVariables$configuration
  body <- queryVariables$query
  
  contentType <- body$contentType
  finalQueryString <- ""
  
  if (contentType == 'application/json') {
    finalQueryString <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = body$qry), 
        novaRush:::getDefaultToJSONargs()), 
      quote = FALSE)
  } else if (contentType == 'application/jwt') {
    finalQueryString <- body$qry
  } else {
    stop("Unsupported content type for query:", contentType)
  }
  
  params <- generateFetchParams(config, 'query', contentType)
  url <- params$url
  
  response <- httr::POST(
    url = url,
    config = add_headers(.headers = params$config$headers),
    body = finalQueryString,
    encode = "raw"
  )
  
  resp_text <- httr::content(response, as = "text", encoding = "UTF-8")
  if (httr::http_error(response)) {
    stop("Query failed: ", resp_text)
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



#' History Query Configuration
#' 
#' @description
#' This function configures the history query instance. 
#' The default context is added and if applicable the query is signed.
#' The function returns a list referred to as `queryVariables` which is a single
#' structure containing all the information necessary to interact with the
#' Fluree instance via `sendHistoryQuery()`.
#' 
#' @param config (`list()`)\cr
#'   The configuration list for the Fluree instance.
#' @param ledger (`character`)\cr
#'   The name of the ledger to query.
#' @param query (`list()`)\cr
#'   The list representation of the query body to be sent.
#'   Alternatively the query can simply be passed as a JSON `character` string.
#' @param signQuery (`logical`)\cr
#'   Determines whether the given query should be signed or not.
#'   This will override the value set in `config$signMessages`.
#' @param privateKey (`character`)\cr
#'   The hexstring representation of the private key to use for message signing.
#'   Overrides `getKey()` if provided.
#' @param apiKey (`character`)\cr
#'   The API key for accessing the Fluree ledger on the remote host (e.g., when
#'   the host is data.flur.ee).
#'   
#' @return A list containing everything needed to query the Fluree database.
#' This includes all the necessary parameters as well as the signed/unsigned query itself.
#' 
#' @importFrom jsonlite validate
#' 
#' @export
history = function(
    config = NULL, 
    ledger = NULL, 
    query, 
    signQuery = NULL, 
    privateKey = NULL,
    apiKey = NULL) {
  
  ledgerName <- ledger %||% config$ledger
  if (is.null(ledgerName)) {
    stop("Please provide a ledger name. Either as argument or within the config.")
  }
  if (is.null(config)) {
    config = setConfig(ledger = ledger)
  }
  
  if (is.character(query)) {
    if (!jsonlite::validate(query)) {
      stop("Please provide a valid JSON query string", call. = FALSE)
    }
    query <- do.call(
      what = jsonlite::fromJSON, 
      args = c(
        list(txt = query),
        novaRush:::getDefaultFromJSONargs()), 
      quote = FALSE)
  }
  
  if (is.null(query$from)) {
    query$from <- ledgerName
  }
  
  if (is.null(query$history) && is.null(query[['commit-details']])) {
    stop('Either the history or commit-details key is required', call. = FALSE)
  }
  
  if (!is.null(signQuery)) {
    shouldSign = signQuery
  } else if (!is.null(config$signMessages)) {
    shouldSign = config$signMessages
  } else {
    shouldSign = FALSE
  }
  
  body <- list(contentType = 'application/json', qry = query)
  
  if (isTRUE(shouldSign)) {
    key <- privateKey %||% getKey()
    if (is.null(key)) {
      stop("Please provide a private key for signing. Either as argument or in the configuration object.", call. = FALSE)
    } else {
      body <- list(
        contentType = 'application/jwt', 
        qry = signQuery(list(configuration = config, qry = body), key))
    }
  }
  
  if (length(apiKey) == 1) {
    config$apiKey <- apiKey
  }
  
  return(list(configuration = config, query = body))
}

#' Send a History Query
#' 
#' @description
#' This function makes use of the `httr` package to send the configured history
#' query to the Fluree `/history` API endpoint.
#' 
#' @param queryVariables (`list()`)\cr
#'   A list representing the history query specifications.
#' 
#' @return A character string containing the response content.
#' 
#' @examples
#' historyQueryInstance <- history(exampleHistoryQuery)
#' sendHistoryQuery(historyQueryInstance)
#' 
#' @export
sendHistoryQuery = function(queryVariables) {
  
  config <- queryVariables$configuration
  body <- queryVariables$query
  
  contentType <- body$contentType
  
  if (contentType == 'application/json') {
    query <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = body$qry), 
        novaRush:::getDefaultToJSONargs()), 
      quote = FALSE)
  } else {
    query <- body$qry
  }
  
  params <- generateFetchParams(config, 'history', contentType)
  url <- params$url
  
  response <- httr::POST(
    url = url,
    config = add_headers(.headers = params$config$headers),
    body = query,
    encode = "raw")
  
  resp_text <- httr::content(response, as = "text", encoding = "UTF-8")
  if (httr::http_error(response)) {
    stop("History query failed: ", resp_text)
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


#' Sign a Query
#' 
#' @description
#' This function is used to sign a query which can then be sent to the Fluree
#' instance as a JWT (JSON Web Token).
#' 
#' @param queryVariables (`list()`)\cr
#'   A list representing the query to be signed.
#' @param privateKey (`character`)\cr
#'   The hexadecimal string representation of the private key to be used for signing.
#'   If a private key is not explicitly provided, the one stored as an environment variable
#'   will be used (if one had been configured).
#' 
#' @return A list containing everything needed to query the Fluree database.
#' This includes all the necessary parameters as well as the signed query itself.
#' 
#' @examples
#' queryInstance <- query(exampleQuery)
#' signedQueryInstance <- signQuery(queryInstance)
#' 
#' @export
signQuery = function(queryVariables = NULL, privateKey = NULL) {
  
  if (is.null(queryVariables)) {
    stop("Please provide the full query to be signed. Call `query()` before signing.", call. = FALSE)
  }
  
  key <- privateKey %||% getKey()
  if (is.null(key)) {
    stop("Please provide a private key for signing. Either as argument or set one using `setKey()`.", call. = FALSE)
  }
  
  config <- queryVariables$configuration
  body <- queryVariables$query
  
  contentType <- body$contentType
  
  if (contentType == 'application/jwt') {
    stop("The provided query has already been signed", call. = FALSE)
  } else {
    input <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = body$qry), 
        novaRush:::getDefaultToJSONargs()), 
      quote = FALSE)
  }
  
  signedQuery <- flureeCrypto:::serialize_jws(as.character(input), key)
  
  body$contentType <- 'application/jwt'
  body$qry <- signedQuery
  
  return(list(configuration = config, query = body))
}


#' Get the Signed Query
#' 
#' @description
#' This function returns the JWT representation of the signed query.
#' Note this function can only be used if a private key had been configured and 
#' the query has been signed.
#' 
#' @returns Character string representing the JWT of the signed query.
#' 
#' @examples
#' queryInstance <- query(exampleQuery)
#' signedQueryInstance <- signQuery(queryInstance)
#' 
#' sig <- getQuerySignature(signedQueryInstance)
#' 
#' @export
getQuerySignature = function(queryVariables = NULL) {
  if (is.null(queryVariables)) {
    stop("Please provide a valid query instance (call `query()` first).", call. = FALSE)
  }
  
  body <- queryVariables$query
  contentType <- body$contentType
  
  if (contentType != "application/jwt") {
    stop("The provided query has not yet been signed. Sign the query using 'signQuery()'
         before attempting to extract the signature", call. = FALSE)
  }
  
  signedQry <- body$qry
  return(signedQry)
}

#' Get the Raw Query Text
#' 
#' @description
#' This function returns the query body as a JSON string.
#' If the query instance has already been signed,  the signature is deserialized
#' before returning the raw JSON string.
#' 
#' @returns JSON string representation of the query body
#' 
#' @examples
#' queryInstance <- query(exampleQuery)
#' 
#' qry <- getQueryText(queryInstance)
#' 
#' @export
getQueryText = function(queryVariables = NULL, pretty = TRUE) {
  if (is.null(queryVariables)) {
    stop("Please provide a valid query instance", call. = FALSE)
  }
  
  body <- queryVariables$query
  contentType <- body$contentType
  
  toJsonArgs <- novaRush:::getDefaultToJSONargs(pretty = pretty)
  
  if (contentType == "application/jwt") {
    
    jwt <- body$qry
    desrialized <- flureeCrypto:::deserialize_jws(jwt)
    
    Qry  <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = desrialized$payload),
        toJsonArgs), 
      quote = FALSE)

  } else {
    
    Qry  <- do.call(
      what = jsonlite::toJSON, 
      args = c(
        list(x = body$qry),
        toJsonArgs), 
      quote = FALSE)

  }
  
  return(Qry)
}

