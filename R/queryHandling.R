
#' Query configuration
#' 
#' @description
#' This function configures the query instance. The default context is configured 
#' and if applicable the query is signed.
#' The function returns a list containing all the information necessary to interact
#' with the Fluree instance via `sendQuery()`.
#' 
#' @param query (`list()`)\cr
#'    The list representation of the query body to be sent.
#'    Note alternatively the query can simply be passed as a JSON `character` string.
#' @param signQuery (`logical`)\cr
#'   Determines whether the given query should be signed or not.
#' @param privateKey (`character`)\cr
#'   The hexstring representation of the private key to use for message signing.
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
#' @export
query = function(query, signQuery = NULL, privateKey = NULL) {
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before querying. Try running connect() before querying", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  
  if (class(query) == "character") {
    if (!validate(query)) {
      stop("Please provide a valid JSON query string", call. = FALSE)
    }
    query <- fromJSON(query, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  }
  
  if (is.null(query$from)) {
    query$from <- config$ledger
  }
  
  # merge contexts if applicable
  defaultContext <- config$defaultContext %||% list()
  queryContext <- query[["@context"]] %||% list()
  
  if (!is.null(defaultContext) || !is.null(queryContext)) {
    query[['@context']] <- mergeContexts(defaultContext, queryContext)
  }
  
  body <- list(contentType = 'application/json', qry = query)
  
  if (isTRUE(config$signMessages) || isTRUE(signQuery)) {
    body <- list(contentType = 'application/jwt', qry = signQuery(list(configuration = config, qry = body), privateKey))
  }
  
  return(list(configuration = config, query = body))
}

#' Send a query
#' 
#' @description
#' This function makes use of `httr` to send the configured query to the 
#' Fluree instance.
#' 
#' @param queryVariables (`list()`)\cr
#'   A list representing the query specifications.
#' 
#' @return A character string containing the response content.
#' 
#' @examples
#' queryInstance <- query(exampleQuery)
#' sendQuery(queryInstance)
#' 
#' @export
sendQuery = function(queryVariables) {
  config <- queryVariables$configuration
  body <- queryVariables$query
  
  contentType <- body$contentType
  
  if (contentType == 'application/json') {
    query <- toJSON(body$qry, auto_unbox = TRUE, pretty = FALSE)
  } else {
    query <- body$qry
  }
  
  params <- generateFetchParams(config, 'query', contentType)
  url <- params$url
  fetchOptions <- params$config
  
  params$body <- query
  
  response <- POST(
    url = url,
    add_headers(`Content-Type` = params$config$headers$`Content-Type`),
    body = params$body,
    encode = "raw"
  )
  
  json_response <- fromJSON(content(response, as = "text"))
  pretty_json <- toJSON(json_response, auto_unbox = TRUE, pretty = TRUE)
  
  return(pretty_json)
}



#' History query configuration
#' 
#' @description
#' This function configures the history query instance. The default context is 
#' configured and if applicable the history query is signed.
#' 
#' @param query (`list()`)\cr
#'    The list representation of the history query body to be sent.
#'    Note alternatively the history query can simply be passed as a JSON `character` string.
#' @param signQuery (`logical`)\cr
#'   Determines whether the given history query should be signed or not.
#' @param privateKey (`character`)\cr
#'   The hexstring representation of the private key to use for message signing.
#' 
#' @return A list containing everything needed to query the Fluree database.
#' This includes all the necessary parameters as well as the signed/unsigned history query itself.
#' 
#' @export
history = function(query, signQuery = NULL, privateKey = NULL) {
  
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before querying. Try running connect() before querying", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  
  if (class(query) == "character") {
    if (!validate(query)) {
      stop("Please provide a valid JSON query string", call. = FALSE)
    }
    query <- fromJSON(query, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  }
  
  if (is.null(query$from)) {
    query$from <- config$ledger
  }
  
  if (is.null(query$history) && is.null(query[['commit-details']])) {
    stop('Either the history or commit-details key is required', call. = FALSE)
  }
  
  body <- list(contentType = 'application/json', qry = query)
  
  if (isTRUE(config$signMessages) || isTRUE(signQuery)) {
    body <- list(contentType = 'application/jwt', qry = signQuery(list(configuration = config, qry = body), privateKey))
  }
  
  return(list(configuration = config, query = body))
}

#' Send a history query
#' 
#' @description
#' This function makes use of `httr` to send the configured query to the 
#' Fluree instance.
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
    query <- toJSON(body$qry, auto_unbox = TRUE, pretty = FALSE)
  } else {
    query <- body$qry
  }
  
  params <- generateFetchParams(config, 'history', contentType)
  url <- params$url
  fetchOptions <- params$config
  
  params$body <- query
  
  response <- POST(
    url = url,
    add_headers(`Content-Type` = params$config$headers$`Content-Type`),
    body = params$body,
    encode = "raw"
  )
  
  json_response <- fromJSON(content(response, as = "text"))
  pretty_json <- toJSON(json_response, auto_unbox = TRUE, pretty = TRUE)
  
  return(pretty_json)
}


#' Sign a query
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
    stop("Please provide the query to be signed", call. = FALSE)
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
  
  config <- queryVariables$configuration
  body <- queryVariables$query
  
  contentType <- body$contentType
  
  if (contentType == 'application/jwt') {
    stop("The provided query has already been signed", call. = FALSE)
  } else {
    input <- toJSON(body$qry, auto_unbox = TRUE, pretty = FALSE)
  }
  
  signedQuery <- flureeCrypto:::serialize_jws(as.character(input), key)
  
  body$contentType <- 'application/jwt'
  body$qry <- signedTransaction
  
  return(list(configuration = config, query = body))
}


#' Get the signed query
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
    stop("Please provide a valid query instance", call. = FALSE)
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

#' Get the query
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
getQueryText = function(queryVariables = NULL) {
  if (is.null(queryVariables)) {
    stop("Please provide a valid query instance", call. = FALSE)
  }
  
  body <- queryVariables$query
  contentType <- body$contentType
  
  if (contentType == "application/jwt") {
    jwt <- body$qry
    desrialized <- flureeCrypto:::deserialize_jws(jwt)
    Qry <- toJSON(desrialized$payload, auto_unbox = TRUE, pretty = TRUE)
  } else {
    Qry <- toJSON(body$qry, auto_unbox = TRUE, pretty = TRUE)
  }
  
  return(Qry)
}

