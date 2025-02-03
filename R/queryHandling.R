
#' Query configuration
#' 
#' @description
#' This function configures the query instance. System variables are set,
#' the default context is configured and if applicable the query is signed.
#' 
#' @param query A list representing the body of the query to be made
#' 
#' @export
query = function(query) {
  print('In query method...')
  
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before querying. Try running connect() before querying", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  if (is.null(query$from)) {
    query$from <- config$ledger
  }
  
  # merge contexts if applicable
  defaultContext <- config$defaultContext %||% list()
  queryContext <- query[["@context"]] %||% list()
  
  if (!is.null(defaultContext) || !is.null(queryContext)) {
    query[['@context']] <- mergeContexts(defaultContext, queryContext)
  }
  
  json_qry <- toJSON(query, auto_unbox = T)
  print(json_qry)
  Sys.setenv(query = json_qry)
  
  if (isTRUE(config$signMessages)) {
    signQuery(query)
  }
}

#' Send a query
#' 
#' @description
#' This function makes use of httr to send the query to the Fluree instance. 
#' The response is then given as output.
#' 
#' @param config A list of configuration parameters for the query
#' 
#' @export
sendQuery = function(config) {
  signedQuery <- Sys.getenv("signedQuery")
  if (nzchar(signedQuery)) {
    contentType <- 'application/jwt'
  } else {
    contentType <- 'application/json'
  }
  
  query <- fromJSON(Sys.getenv("query"), simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  
  params <- generateFetchParams(config, 'query', contentType)
  url <- params$url
  fetchOptions <- params$config
  
  if (nzchar(signedQuery)) {
    params$body <- signedQuery
  } else {
    params$body <- toJSON(query, auto_unbox = T, pretty = F)
    
  }
  
  response <- POST(
    url = url,
    add_headers(`Content-Type` = params$config$headers$`Content-Type`),
    body = params$body,
    encode = "raw"
  )
  
  Sys.unsetenv("query")
  Sys.unsetenv("signedQuery")
  
  json_response <- content(response, as = "text")
  pretty_json <- fromJSON(json_response)
  
  cat(toJSON(pretty_json, pretty = T, auto_unbox = T))
}



#' History query configuration
#' 
#' @description
#' This function configures the history query instance. System variables are set,
#' the default context is configured and if applicable the history query is signed.
#' 
#' @param query A list representing the body of the history query to be made
#' 
#' @export
history = function(query) {
  print('In history method...')
  
  connected <- as.logical(Sys.getenv("connected"))
  if (!isTRUE(connected)) {
    stop("You must connect before querying. Try running connect() before querying", call. = FALSE)
  }
  
  config <- fromJSON(Sys.getenv("config"))
  if (is.null(query$from)) {
    query$from <- config$ledger
  }
  
  if (is.null(query$history) && is.null(query[['commit-details']])) {
    stop('either the history or commit-details key is required', call. = FALSE)
  }
  
  json_qry <- toJSON(query, auto_unbox = T)
  print(json_qry)
  Sys.setenv(query = json_qry)
  
  if (isTRUE(config$signMessages)) {
    signQuery(query)
  }
}

#' Send a history query
#' 
#' @description
#' This function makes use of httr to send the history query to the Fluree instance. 
#' The response is then given as output.
#' 
#' @param config A list of configuration parameters for the query
#' 
#' @export
sendHistoryQuery = function(config) {
  signedQuery <- Sys.getenv("signedQuery")
  if (nzchar(signedQuery)) {
    contentType <- 'application/jwt'
  } else {
    contentType <- 'application/json'
  }
  
  history_query <- fromJSON(Sys.getenv("query"), simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F)
  
  params <- generateFetchParams(config, 'history', contentType)
  url <- params$url
  fetchOptions <- params$config
  
  if (nzchar(signedQuery)) {
    params$body <- signedQuery
  } else {
    params$body <- toJSON(history_query, auto_unbox = T, pretty = F)
    
  }
  
  response <- POST(
    url = url,
    add_headers(`Content-Type` = params$config$headers$`Content-Type`),
    body = params$body,
    encode = "raw"
  )
  
  Sys.unsetenv("query")
  Sys.unsetenv("signedQuery")
  
  json_response <- content(response, as = "text")
  pretty_json <- fromJSON(json_response)

  cat(toJSON(pretty_json, pretty = T, auto_unbox = T))
}


#' Sign a query
#' 
#' @description
#' This function is used to sign a query which can then be sent to the Fluree
#' instance as a JWT (JSON Web Token).
#' 
#' @param query A list representing the query to be signed
#' @param privateKey The hexadecimal string representation of the private key to be used for signing
#' 
#' @export
signQuery = function(query = NULL, privateKey = NULL) {
  
  if (is.null(query)) {
    query <- fromJSON(Sys.getenv("query"))
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
  
  input <- toJSON(query, auto_unbox = TRUE, pretty = FALSE)
  signedQuery <- flureeCrypto:::serialize_jws(as.character(input), key)
  
  Sys.setenv(signedQuery = signedQuery)
}


#' Get the signed query
#' 
#' @description
#' This function returns the JWT representation of the signed query.
#' Note this function can only be used if a private key has been configured and 
#' the query has already been signed.
#' 
#' @returns String representation of the signed query as a JWT
#' 
#' @export
getQuerySignature = function() {
  signedQry <- Sys.getenv("signedQuery")
  return(signedQry)
}

#' Get the query
#' 
#' @description
#' This function returns the query body as a JSON string.
#' 
#' @returns JSON string representation of the query body
#' 
#' @export
getTransactionText = function() {
  Qry <- Sys.getenv("query")
  return(Qry)
}

