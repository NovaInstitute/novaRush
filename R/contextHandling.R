
#' Set the Default Context
#' 
#' @description
#' The default context set here will be used for all queries and transactions.
#' Unlike `addToContext()` this method does not merge new context elements 
#' with existing ones, instead it will replace the existing `defaultContext`
#' entirely.
#' 
#' @param currentConfig (`list()`)\cr
#'   The existing Fluree configuration parameters.
#' @param context (`list()`)\cr
#'   The new default context to set.
#' 
#' @returns The updated configuration list.
#' 
#' @examples
#' config <- setConfig(ledger = "test1")
#' c <- list("f" = "https://ns.flur.ee/ledger#", "ex" = "http://example.org/",
#'           "schema" = "http://schema.org/")
#' config <- setContext(config, c)
#' 
#' 
#' @export
setContext = function(currentConfig, context) {
  currentConfig$defaultContext <- context
  return(currentConfig)                     
}

#' Add to the default context
#' 
#' @description
#' This function adds to the already existing `defaultContext` by merging new
#' context elements with the existing ones.
#' 
#' @param currentConfig The current configuration list.
#' @param context The new context elements to add.
#' 
#' @returns The updated configuration list.
#' 
#' @examples
#' config <- setConfig(ledger = "test1")
#' c <- list("f" = "https://ns.flur.ee/ledger#", "ex" = "http://example.org/",
#'           "schema" = "http://schema.org/")
#' config <- setContext(config, c)
#' config <- addToContext(config, list("rdfs" = ""))
#' 
#' @export
addToContext = function(currentConfig, context) {
  if (!is.null(currentConfig$defaultContext)) {
    newContext <- mergeContexts(currentConfig$defaultContext, context)
    currentConfig$defaultContext = newContext
  } else {
    currentConfig$defaultContext = context
  }
  return(currentConfig)
}


#' Merge Two Contexts
#' 
#' @description
#' Merges a new context into a base context
#' Supports strings, lists, and named lists.
#' Handles conflicts and duplicates in namespace abbreviations:
#' - Solid duplicates (same key = same value) are kept once
#' - Different abbreviations for the same namespace keep the first one
#' - Same abbreviation for different namespaces: context1's value is kept
#'
#' @param context1 The base context.\cr
#'  Which can be a string, a list, or a named list.
#' @param context2 The new context to merge,\cr
#'  Which can also be a string, a list, or a named list.
#' 
#' @returns The merged context.\cr
#' If both contexts are strings, they are combined into a list.
#' 
#' @examples
#' mergeContexts("https://example.org/context1", "https://example.org/context2")
#' mergeContexts("https://example.org/context1", list("https://example.org/context2"))
#' mergeContexts(list("https://example.org/context1"), list("https://example.org/context2"))
#' mergeContexts(list("https://example.org/context1"), list(a = "https://example.org/context2"))
#' 
mergeContexts <- function(context1, context2) {
  if (is.character(context1) && length(context1) == 1) {
    if (is.character(context2) && length(context2) == 1) {
      if (context1 == context2) return(context1)
      return(list(context1, context2))
    } else if (is.list(context2)) {
      return(c(list(context1), context2))
    } else {
      if (length(context2) == 0) return(context1)
      return(list(context1, context2))
    }
    
  } else if (is.list(context1)) {
    if (is.character(context2) && length(context2) == 1) {
      return(c(context1, list(context2)))
    } else if (is.list(context2)) {
      return(mergeContextLists(context1, context2))
    } else {
      if (length(context2) == 0) return(context1)
      return(c(context1, list(context2)))
    }
    
  } else {
    if (length(context1) == 0) return(context2)
    if (is.character(context2) && length(context2) == 1) {
      return(list(context1, context2))
    } else if (is.list(context2)) {
      return(c(list(context1), context2))
    } else {
      return(modifyList(context1, context2))
    }
  }
  
  stop("Unsupported context types provided.")
}

#' Merge Two Context Lists
#' 
#' @description
#' Helper function to merge two context lists while handling duplicates and conflicts.
#' 
#' @param context1 The base context list.
#' @param context2 The new context list to merge.
#' 
#' @returns The merged context list with duplicates removed and conflicts resolved.
#' 
mergeContextLists <- function(context1, context2) {
  # Get named and unnamed elements from both contexts
  names1 <- names(context1)
  names2 <- names(context2)
  
  # Check if either context has named elements (namespace mappings)
  has_named1 <- !is.null(names1) && any(names1 != "")
  has_named2 <- !is.null(names2) && any(names2 != "")
  
  # If neither has named elements, just concatenate with duplicate removal
  if (!has_named1 && !has_named2) {
    return(removeDuplicateUnnamed(c(context1, context2)))
  }
  
  # Separate named and unnamed elements
  if (is.null(names1)) names1 <- rep("", length(context1))
  if (is.null(names2)) names2 <- rep("", length(context2))
  
  named1_idx <- which(names1 != "")
  unnamed1_idx <- which(names1 == "")
  named2_idx <- which(names2 != "")
  unnamed2_idx <- which(names2 == "")
  
  # Get named elements (namespace mappings)
  named1 <- if (length(named1_idx) > 0) context1[named1_idx] else list()
  named2 <- if (length(named2_idx) > 0) context2[named2_idx] else list()
  
  # Get unnamed elements (string URLs, etc.)
  unnamed1 <- if (length(unnamed1_idx) > 0) context1[unnamed1_idx] else list()
  unnamed2 <- if (length(unnamed2_idx) > 0) context2[unnamed2_idx] else list()
  
  # Merge named elements handling duplicates and conflicts
  mergedNamed <- mergeNamedContexts(named1, named2)
  
  # Combine unnamed elements, removing duplicates
  mergedUnnamed <- removeDuplicateUnnamed(c(unnamed1, unnamed2))
  
  # Combine both parts
  if (length(mergedUnnamed) > 0 && length(mergedNamed) > 0) {
    return(c(mergedUnnamed, mergedNamed))
  } else if (length(mergedNamed) > 0) {
    return(mergedNamed)
  } else {
    return(mergedUnnamed)
  }
}

#' Merge Named Contexts
#' 
#' @description
#' Merges two named context lists (namespace abbreviation mappings).
#' Handles three types of duplicates/conflicts:
#' 1. Solid duplicates (same key = same value): kept once
#' 2. Different keys for same value: keep the first one encountered
#' 3. Same key for different values: keep context1's value (base context wins)
#' 
#' @param named1 The base named context list.
#' @param named2 The new named context list to merge.
#' 
#' @returns The merged named context list.
#' 
mergeNamedContexts <- function(named1, named2) {
  if (length(named1) == 0) return(named2)
  if (length(named2) == 0) return(named1)
  
  result <- named1
  existingValues <- unlist(named1)
  existingKeys <- names(named1)
  
  for (key in names(named2)) {
    value <- named2[[key]]
    
    # Case 3: Same key exists in context1
    if (key %in% existingKeys) {
      # If same value (solid duplicate), skip - already in result
      # If different value (conflict), context1 wins - already in result
      next
    }
    
    # Case 2: Check if the value already exists with a different key
    if (is.character(value) && length(value) == 1 && value %in% existingValues) {
      # Value already exists with a different key, skip this entry
      next
    }
    
    # No conflict or duplicate, add to result
    result[[key]] <- value
    if (is.character(value) && length(value) == 1) {
      existingValues <- c(existingValues, value)
    }
    existingKeys <- c(existingKeys, key)
  }
  
  return(result)
}

#' Remove Duplicate Unnamed Context Elements
#' 
#' @description
#' Removes duplicate unnamed elements from a context list.
#' 
#' @param contextList The context list with potential duplicates.
#' 
#' @returns The context list with duplicates removed.
#' 
removeDuplicateUnnamed <- function(contextList) {
  if (length(contextList) == 0) return(contextList)
  
  seen <- list()
  result <- list()
  
  for (i in seq_along(contextList)) {
    item <- contextList[[i]]
    
    # For simple character strings, check for duplicates
    if (is.character(item) && length(item) == 1) {
      if (!(item %in% seen)) {
        seen <- c(seen, item)
        result <- c(result, list(item))
      }
    } else {
      # For complex items, always add them
      result <- c(result, list(item))
    }
  }
  
  return(result)
}

#' Find the Alias of the ID Field
#' 
#' @description
#' This function looks up if any alias for '@id' has been defined within the
#' `defaultContext` of the current instance. If none is found the default value:
#' '@id' is used.
#' 
#' @param context (`list()`)\cr
#'   The default context of the Fluree instance.
#' 
#' @return (`string`)
#' 
findIdAlias = function(context) {
  
  if (is.null(context)) {
    return("@id")
  }
  
  if (is.character(context)) {
    return("@id")
  } else if (is.list(context)) {
    if ("@id" %in% names(context)) {
      return(context[["@id"]])
    }
    for (key in names(context)) {
      if (is.list(context[[key]])) {
        result <- findIdAlias(context[[key]])
        if (result != "@id") {
          return(result)
        }
      }
    }
    return("@id")
  } else {
    stop("Unsupported context type provided.")
  }
}
