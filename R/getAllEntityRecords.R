
#' getAllEntityRecords
#' @description Helper function to fetch all records of an entity from Fluree
#' @param ledgerName Character. The name of the ledger.
#' @param entityName Character. The name of the entity.
#' @return character
#' @export
#' @examples :
#' dfCollections <- getAllEntityRecords("authority/test", "_collection")
#' dfUser <- getAllEntityRecords("authority/test", "_user")
#' dfAuth <- getAllEntityRecords("authority/test", "_auth")
#' dfPredicate <- getAllEntityRecords("authority/test", "_predicate")

getAllEntityRecords <- function(ledgerName, entityName, signQuery = TRUE){
  require(httr)
  require(tibble)
  # Define the URL
  url <- paste(Sys.getenv("fluree_link"), ledgerName,"/query", sep = "")
  # Define the query object
  queryObj <- list(
    select = list("*"),
    from = entityName
  )
  # Convert the query object to JSON
body <- jsonlite::toJSON(queryObj, auto_unbox = TRUE)
if(signQuery){
# Sign the query
#body <- makeQuerySignature(ledgerName = ledgerName, queryString =  body)
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
                        endpoint = "query/",
                        body = body,
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
  # Make the POST request
  response <- POST(
    url,
    add_headers("Content-Type" = "application/json"),
    body = body
  )
  # Parse JSON data
  data_list <- jsonlite::fromJSON(content(response, "text"))
  # Convert list to tibble
  data_tibble <- as_tibble(data_list)
  return(data_tibble)
}
