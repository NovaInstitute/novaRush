
#' Set the default context
#' 
#' @description
#' The default context set here will be used for all queries and transactions.
#' Unlike `addToContext()` this method does not merge new context elements 
#' with existing ones, instead it will replace the existing 
#' `defaultContext` entirely.
#' 
#' @export
setContext = function(context) {
  updateConfiguration(list(defaultContext = context))                     
}

# TODO: Fix this function to work for functional implementation
#' Add to the default context
#' 
#' @description
#' This function adds to the already existing `defaultContext`.
#' 
#' @export
addToContext = function(context) {
  if (!is.null(self$config$defaultContext)) {
    newContext <- mergeContexts(self$config$defaultContext, context)
    self$config$defaultContext = newContext
  } else {
    self$config$defaultContext = context
  }
  return(self)
}

# TODO: Fix this function to work for functional implementation
#' Get the context
#' 
#' @description
#' This function extracts the `defaultContext` from the `config` of the current
#' Fluree instance.
#' 
#' @return (`list()`)
#' 
#' @export
getContext = function() {
  return(self$config$defaultContext)
}

#' Merge Two Contexts
#'
#' Merges a new context into a base context, supporting strings, lists, and named lists (objects).
#'
#' @param context1 The base context, which can be a string, a list, or a named list.
#' @param context2 The new context to merge, which can also be a string, a list, or a named list.
#' @return A merged context. If both contexts are strings, they are combined into a list.
#' @examples
#' merge_contexts("https://example.org/context1", "https://example.org/context2")
#' merge_contexts("https://example.org/context1", list("https://example.org/context2"))
#' merge_contexts(list("https://example.org/context1"), list("https://example.org/context2"))
#' merge_contexts(list("https://example.org/context1"), list(a = "https://example.org/context2"))
mergeContexts <- function(context1, context2) {
  if (is.character(context1) && length(context1) == 1) {  # context1 is a single string
    if (is.character(context2) && length(context2) == 1) {
      return(list(context1, context2))  # Combine two strings into a list
    } else if (is.list(context2)) {
      return(c(list(context1), context2))  # Add string to the beginning of the list
    } else {
      if (length(context2) == 0) return(context1)  # context2 is an empty object
      return(list(context1, context2))  # Combine string with named list
    }
  } else if (is.list(context1)) {  # context1 is a list (array in JS terms)
    if (is.character(context2) && length(context2) == 1) {
      return(c(context1, list(context2)))  # Append string to list
    } else if (is.list(context2)) {
      return(c(context1, context2))  # Concatenate two lists
    } else {
      if (length(context2) == 0) return(context1)  # context2 is an empty object
      return(c(context1, list(context2)))  # Append named list to the list
    }
  } else {  # context1 is a named list (object in JS terms)
    if (length(context1) == 0) return(context2)  # context1 is an empty object
    if (is.character(context2) && length(context2) == 1) {
      return(list(context1, context2))  # Combine named list with string
    } else if (is.list(context2)) {
      return(c(list(context1), context2))  # Add named list to the beginning of the list
    } else {
      return(modifyList(context1, context2))  # Merge two named lists
    }
  }
  
  stop("Unsupported context types provided.")  # Catch invalid inputs
}

#' Find the alias of the id field
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
#' @export
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
