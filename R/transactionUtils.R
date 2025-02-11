
#' Process an entity and build a structured map
#' 
#' @description
#' Recursively processes an entity, extracting nested entities and structuring them
#' into a map where each entity is keyed by its ID.
#' 
#' @param entity (`list`)\cr
#'   The entity to process.
#' @param map (`list`)\cr
#'   A map to store processed entities.
#' @param idAlias (`character`)\cr
#'   The alias used for the @id field in the context of the Fluree instance.
#' 
#' @return (`list`)
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

#' Flatten a transaction
#' 
#' @description
#' Calls `flattenEntity` to process the transaction into a structured map.
#' 
#' @param txn (`list`)\cr
#'   The transaction to flatten.
#' @param idAlias (`character`)\cr
#'   The alias used for the @id field in the context of the Fluree instance.
#' 
#' @return (`list`)
flattenTxn = function(txn, idAlias) {
  return(flattenEntity(txn,idAlias))
}

#' Flatten an entity into a structured map
#' 
#' @description
#' Processes an entity (or a list of entities) and structures it into a map
#' where each entity is stored with its ID as a key.
#' 
#' @param input (`list`)\cr
#'   The entity or list of entities to flatten.
#' @param idAlias (`character`)\cr
#'   The alias used for the @id field in the context of the Fluree instance.
#' 
#' @return (`list`)
flattenEntity = function(input, idAlias) {
  map <- list()
  
  txns <- if (is.list(input) && !is.null(names(input))) list(input) else input
  
  for (txn in txns) {
    map <- processEntity(txn, map, idAlias)
  }
  map <- Filter(function(e) length(e) > 1, map)
  
  return(map)
}

#' Convert a flattened transaction to a where-delete format
#' 
#' @description
#' Converts a structured transaction map into a format used for deletion,
#' producing `where` and `delete` clauses.
#' 
#' @param flattenedTxn (`list`)
#'   The structured transaction map.
#' @param idAlias (`character`)
#'   The alias used for the @id field in the context of the Fluree instance.
#' 
#' @return (`list`) A list with `where` and `delete` keys.
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
