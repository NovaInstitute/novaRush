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
    bodyObject <- makeQuerySignature(ledgerName = ledgerName, queryString =  bodyObject)
  }
  # Define the URL
  response <- flureeFetch(path = paste0(Sys.getenv("fluree_link"), ledgerName, "/transact/"),
                          method = "POST",
                          body = bodyObject)
  return(response)
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
