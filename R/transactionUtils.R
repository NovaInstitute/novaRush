
processEntity = function(entity, map, idAlias) {
  
  if (!(idAlias %in% names(entity))) {
    stop("Entity must have an ID")
  }
  
  entityId <- entity[[idAlias]]
  
  # Initialize the entity in map if not already present
  if (!entityId %in% names(map)) {
    map[[entityId]] <- list()
    map[[entityId]][[idAlias]] <- entityId
  }
  
  for (key in names(entity)) {
    value <- entity[[key]]
    
    if (is.list(value) && length(value) > 0 && is.list(value[[1]]) && idAlias %in% names(value[[1]])) {
      # Array of nested entities
      map[[entityId]][[key]] <- lapply(value, function(item) {
        if (is.list(item) && idAlias %in% names(item)) {
          processEntity(item, map, idAlias) # Recursively process the entity
          return(setNames(list(item[[idAlias]]), idAlias))
        }
        return(item)
      })
    } else if (is.list(value) && idAlias %in% names(value)) {
      # Single nested entity
      processEntity(value, map, idAlias)
      map[[entityId]][[key]] <- setNames(list(value[[idAlias]]), idAlias)
    } else if (key != idAlias) {
      # Copy direct properties 
      map[[entityId]][[key]] <- value
    }
  }
  return(map)
}


flattenTxn = function(txn, idAlias) {
  return(flattenEntity(txn,idAlias))
}

flattenEntity = function(input, idAlias) {
  map <- list()
  
  txns <- if (is.list(input) && !is.null(names(input))) list(input) else input
  
  for (txn in txns) {
    map <- processEntity(txn, map, idAlias)
  }
  map <- Filter(function(e) length(e) > 1, map)
  
  return(map)
}

convertTxnToWhereDelete <- function(flattenedTxn, idAlias) {
  whereClause <- list()
  deleteClause <- list()
  i <- 1
  
  if (length(flattenedTxn) == 0) {
    return(list(where = whereClause, delete = deleteClause))
  }
  
  for (key in names(flattenedTxn)) {
    entity <- flattenedTxn[[key]]
    entityKeys <- setdiff(names(entity), idAlias)
    
    whereEntity <- list()
    whereEntity[[idAlias]] <- key
    
    for (k in entityKeys) {
      expression <- modifyList(whereEntity, setNames(list(sprintf("?%d", i)), k))
      
      whereClause <- append(whereClause, list(list("optional", expression)))
      deleteClause <- append(deleteClause, list(expression))
      
      i <- i + 1
    }
  }
  
  return(list(where = whereClause, delete = deleteClause))
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
handleUpsert = function(upsertTxn, idAlias) {
  flattenedTxn <- flattenTxn(upsertTxn, idAlias)
  res <- convertTxnToWhereDelete(flattenedTxn, idAlias)
  whereClause <- res[[1]]
  deleteClause <- res[[2]]
  
  return(list(
    where = whereClause,
    delete = deleteClause,
    insert = upsertTxn
  ))
}

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
