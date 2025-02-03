
#' Handle a delete transaction
#' 
#' @description
#' This function serves to create the transaction body to be sent to the Fluree
#' instance in order to perform the delete.
#' 
#' @param id (`string`)\cr
#'   The entry to be deleted.
#' @param idAlias (`string`)\cr
#'   The alias used for the @id field in the context of the Fluree instance.
#' 
#' @return (`list`)
#' 
#' @export
handleDelete = function(id, idAlias) {
  idList <- if (is.character(id) && length(id) == 1) list(id) else as.list(id)
    
  whereDelete <- generateWhereDeleteForIds(idList, idAlias)
  
  return(list(
    where = whereDelete,
    delete = whereDelete
  ))
}

#' Format a delete transaction
#' 
#' @description
#' Generates the correct where-delete format to delete the given id.
#' 
#' @param id (`string`)\cr
#'   The entry to be deleted.
#' @param idAlias (`string`)\cr
#'   The alias used for the @id field in the context of the Fluree instance.
#' 
#' @return (`list`)
#' 
#' @export
generateWhereDeleteForIds = function(ids, idAlias) {
  where <- list()
  
  for (index in seq_along(ids)) {
    element <- list()
    
    element[[idAlias]] <- ids[[index]]
    
    element[[sprintf("?p%d", index - 1)]] <- sprintf("?o%d", index - 1)
    
    where[[index]] <- element
  }
  return(where)
}
