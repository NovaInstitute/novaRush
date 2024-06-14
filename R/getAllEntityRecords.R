
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
    body <- makeQuerySignature(ledgerName = ledgerName, queryString =  body)
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
