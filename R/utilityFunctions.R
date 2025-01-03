
runFluree <- function(port = "58090", name = NULL, dockerImage = Sys.getenv("current_stable_docker_image")) {
  cmd <- sprintf("docker run -p %s:8090 --name %s fluree/server:%s", port, name, dockerImage)
  Sys.setenv("port" = port, "container_name" = name)
  system(cmd)
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

generateFetchParams <- function(config, endpoint, contentType = "application/json") {
  
  host <- config$host
  port <- config$port
  isFlureeHosted <- config$FlureeHosted
  apiKey <- config$apiKey
  
  if (!is.null(isFlureeHosted) && isFlureeHosted)  {
    url <- "https://data.flur.ee"
  } else {
    url <- paste0("http://", host)
    if (!is.null(port)) {
      url <- paste0(url, ":", port)
    }
  }
  url <- paste0(url, "/fluree/", endpoint)
  
  
  headers <- list(
    'Content-Type' = contentType
  )
  
  if (!is.null(apiKey)) {
    headers[['Authorization']] <- paste0("Bearer ", apiKey)
  }
  
  params <- list(
    url = url,
    config = list(
      method = "POST",
      headers = headers
    )
  )
  return(params)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
