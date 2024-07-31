#' flureeTransact
#' @description Helper function to transact in Fluree
#' @param ledgerName Character. The name of the ledger.
#' @param transactObject json The object to be transacted.
#' @param signQuery Logical. Should the query be signed?
#' @return character
#' @export

flureeTransact <- function(ledgerName, transactObject, signQuery = TRUE){
  require(httr)
  require(jsonlite)
  # Convert the query object to JSON
  bodyObject <- transactObject
  if(signQuery){
    # Sign the query
    #bodyObject <- makeQuerySignature(ledgerName = ledgerName, queryString = bodyObject)
    authId <- Sys.getenv("authId")
    privateKey <- Sys.getenv("privateKey")
    if(is.null(authId) | is.null(privateKey)){
      stop("Please set the authId and privateKey in the environment variables")
    }
    if(nchar(privateKey) <1 | nchar(authId) <1){
      stop("Please set the authId and privateKey in the environment variables")
    }

    jsCode <- signatureText(ledgerName = ledgerName,
                            privateKey = privateKey,
                            endpoint = "transact",
                            body = bodyObject,
                            authId = authId)

    cat(jsCode, file = "temp.js")
    payload <- system(paste('node temp.js'), intern = TRUE)
    unlink("temp.js")
    if(any(grepl("using Node.js", payload))){
      payload <- payload[-1]
    }
    payload <- paste(payload, collapse = "")
    json_payload <- jsonlite::fromJSON(payload)
    return(json_payload)
  }
  # Define the URL
  response <- flureeFetch(path = paste0(Sys.getenv("fluree_link"), ledgerName, "/transact/"),
                          method = "POST",
                          body = bodyObject)
  return( response)
}



#' insertData
#'
#' @description Helper function to create a schema in Fluree
#' @param path Character. The path to the Fluree database.
#' @param data_list List. The schema to be created (predicates).
#' @param fluree_link Character. The link to the Fluree database.
#' @return character
#' @export
#' @import httr
insertData <- function(path,
                       data_list,
                       fluree_link = Sys.getenv("fluree_link")) {
  require(rjson)
  flureeFetch(path = paste0(fluree_link, path, "transact"),
              method = "POST",
              body = toJSON(data_list))
}
