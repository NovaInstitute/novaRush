#' createDb
#'
#' @description Helper function to helper create a Fluree database.
#' @param collectionName Character. The name of the Fluree database.
#'
#' @return character
#' @export
#' @import httr
createDb <- function(collectionName){
  system(paste("node js/createDb.js ", collectionName, sep = ""))
}

#' createPredicate
#' @description Helper function to helper create add predicates to a Fluree database.
#' @param collectionName Character. The name of the Fluree database.
#' @param schemaList Character. The list of predicates to add to the database.
#' @return character
#' @export
#' @examples: createSchema("test", [{
#'  _id: '_predicate',
#'   name: '_auth/secretValue',
#'   type: 'string',
#' })
#'
#' @import httr
createPredicate <- function(collectionName, schemaList){
  system(paste("node js/createPredicate.js ", collectionName, " ", jsonify::to_json(schemaList, unbox = TRUE), sep = ""))
}


#' insertData
#' @description Helper function to helper create add predicates to a Fluree database.
#' @param collectionName Character. The name of the Fluree database.
#' @param dataList Character. The list of data objects to add to the database.
#' @return character
#' @export
#' @examples: insertData("test", [{
#'  _id: '_predicate',
#'   name: '_auth/secretValue',
#'   type: 'string',
#' })
#'
#' @import httr
insertData <- function(collectionName, dataList){
  system(paste("node js/insertData.js ", collectionName, " ", jsonify::to_json(dataList), sep = ""))
}


#' deleteDb
#' @description Helper function to helper delete to a Fluree database.
#' @param collectionName Character. The name of the Fluree database.
#' @return character
#' @export
#'
#' @import httr
deleteDb <- function(collectionName){
  system(paste("node js/deleteDb.js ", collectionName, sep = ""))
}


######################################################################################################################












