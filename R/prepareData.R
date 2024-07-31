#' prepareData
#'
#' @description Helper function to add data in Fluree
#' @param dfData Dataframe or tibble with the data to be added.
#' @param collectionName Character. The name of the collection.
#' @return character
#' @export
#' @import httr

prepareData <- function(dfData = NULL, collectionName = NULL){
  if(is.null(dfData)){
    stop("Please provide a data frame or a tibble")
  }
  if(is.null(collectionName)){
    stop("Please provide a collection name")
  }

  data_list <- list()
  for(i in 1:nrow(dfData)){
    dfRow <- dfData[i,]
    dfRow[["_id"]] <- collectionName
    data_list[[i]] <- dfRow
  }
  return(data_list)
}
