#' prepareSchema
#' @description Helper function to prepare a schema in Fluree
#' @param predicateNames List. The names of the predicates.
#' @param dfData Dataframe. The data to be added.
#' @return character
#' @export
#' @import httr

prepareSchema <- function(predicateNames = NULL, dfData = NULL){
  if(is.null(predicateNames) & is.null(dfData)){
    stop("Please provide a list of predicate names or a data frame")
  }

  if(is.null(predicateNames)){
    predicateNames <- sapply(dfData, function(x){
      switch(class(x),
             "character" = "string",
             "numeric" = "double",
             "logical" = "boolean",
             "factor" = "string",
             "Date" = "date",
             "POSIXct" = "date",
             "POSIXt" = "date",
             "integer" = "integer",
             "list" = "array",
             "default" = "string")
    })
  }else{
    if(is.null(names(predicateNames))){
      stop("predicateNames must be a named list, containing the predicate names and their types")
    }
  }
  schema_list <- list()
  for(i in 1:length(predicateNames)){
    schema_list[[i]] <- list(
      "_id" = "_predicate",
      "name" = paste0(basename(path), "/", names(predicateNames[i])),
      "type" = predicateNames[[i]]
    )
  }
  return(schema_list)
}


#' createSchema
#' @description Helper function to create a schema in Fluree
#' @param path Character. The path to the Fluree database.
#' @param schema_list List. The schema to be created (predicates), accepts output from prepareSchema().
#' @param fluree_link Character. The link to the Fluree database.
#'
#' @return character
#' @export
#' @import httr
createSchema <- function(path = NULL,
                         schema_list = NULL,
                         fluree_link = Sys.getenv("fluree_link")) {

  if (is.null(path)) stop("please provide a path")
  if (is.null(schema_list)) stop("please provide a schema_list: e.g. output from prepareSchema()")

  link <- paste0(fluree_link, path, "transact")
  flureeFetch(path = link,
              method = "POST",
              body = jsonlite::toJSON(schema_list, auto_unbox = TRUE))
}
