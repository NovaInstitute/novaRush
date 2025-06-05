
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
      return(c(context1, context2))
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
