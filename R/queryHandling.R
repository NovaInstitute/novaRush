
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
  
  json_qry <- toJSON(query, auto_unbox = TRUE)
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
  
  query <- fromJSON(Sys.getenv("query"))
  
  params <- generateFetchParams(config, 'query', contentType)
  url <- params$url
  fetchOptions <- params$config
  
  if (nzchar(signedQuery)) {
    params$body <- signedQuery
  } else {
    params$body <- toJSON(query, auto_unbox = TRUE, pretty = TRUE)
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

