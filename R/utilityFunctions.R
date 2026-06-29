
#' Function to run the docker container
#' 
#' @description
#' This function takes flags as parameter inputs and adds these flags to the
#' container when the docker image is run.
#' 
#' @param port (`string`)\cr
#'   The port where the instance is running (e.g. '58090')
#' @param name (`string`)\cr
#'   The name to be linked to the docker container.
#' @param dockerImage (`string`)\cr
#'   The current stable version of the fluree/server docker image.
#' 
#' @export
runDockerContainer <- function(
    port = "58090", 
    name = NULL, 
    dockerImage = "5839ffe273062b8da972b120deb54dd62e7c3d1f") {
  
  if (is.null(name)) {
    cmd <- sprintf("docker run -d -p %s:8090 fluree/server:%s", port, name, dockerImage)
    res <- system(cmd)
    Sys.setenv("container_name" = res)
  } else {
    cmd <- sprintf("docker run -d -p %s:8090 --name %s fluree/server:%s", port, name, dockerImage)
    system(cmd)
    Sys.setenv("container_name" = name)
  }
}

#' Function to stop the docker container
#' 
#' @description
#' This function stops the specified docker container.
#' If no container name (or ID) is provided,  the last container that was started
#' will be stopped.
#' 
#' @param name (`string`)\cr
#'   The name of the container to be stopped. 
#'   (Note this could also be the container ID).
#' 
#' @export
stopDockerContainer <- function(name = NULL) {
  if (is.null(name)) {
    name <- Sys.getenv("container_name")
  }
  cmd <- sprintf("docker stop %s", name)
  system(cmd)
  
  # This part makes testing easier as it allows the same name to be used for a 
  # new docker container (might need to be removed later)
  cmd <- sprintf("docker rm %s", name)
  system(cmd)
}

#' Constructs arguments for httr::POST
#' 
#' @description
#' This function is a generic function to construct the parameters needed by
#' `httr` to execute a `POST()` request of a transaction or query.
#' 
#' @param config (`list()`)\cr
#'   The configuration parameters of the current active transaction or query instance.
#' @param endpoint (`string`)\cr
#'   The Fluree v4 endpoint name: 'query', 'insert', 'upsert', 'update', 'create', or 'history'.
#'   Appended to the versioned base path `/v1/fluree/`.
#' @param contentType (`string`)\cr
#'   In the case of an unsigned message 'application/json' is used ('application/jwt' if signed).
#'   Use 'application/sparql-query' for raw SPARQL strings.
#' @param ledger (`string`)\cr
#'   Optional ledger name appended to the URL path (e.g. 'myorg/mydb').
#'   When provided the ledger is taken from the URL rather than the request body.
#'
#' @return (`list()`)
generateFetchParams <- function(config, endpoint, contentType = "application/json", ledger = NULL) {
  
  host <- config$host
  port <- config$port
  apiKey <- config$apiKey
  
  protocol <- if (isTRUE(config$isFlureeHosted) || identical(host, "data.flur.ee")) "https" else "http"
  url <- paste0(protocol, "://", host)
  if (!is.null(port)) {
    url <- paste0(url, ":", port)
  }
  url <- paste0(url, "/v1/fluree/", endpoint)
  if (!is.null(ledger)) {
    url <- paste0(url, "/", ledger)
  }

  header <- c(
    'Content-Type' = contentType)
  
  if (!is.null(apiKey)) {
    header <- c(header, 'Authorization' = paste0("Bearer ", apiKey))
  }
  
  params <- list(
    url = url,
    config = list(
      method = "POST",
      headers = header
    )
  )
  return(params)
}

deep_merge <- function(x, y) {
  for (name in names(y)) {
    if (is.list(x[[name]]) && is.list(y[[name]])) {
      x[[name]] <- deep_merge(x[[name]], y[[name]])
    } else {
      x[[name]] <- y[[name]]
    }
  }
  x
}

#' getDefaultToJSONargs
#' 
#' @description
#'  To be called by any function in this package that makes use of 
#'  jsonlite::toJSON. Ensures toJSON conversion consistency across all functions.
#'
getDefaultToJSONargs <- function(pretty = FALSE) {
  return(
    list(
      auto_unbox = TRUE, 
      pretty = pretty, 
      dataframe = "rows", 
      matrix = "rowmajor", 
      Date = "ISO8601", 
      POSIXt = "ISO8601", 
      factor = "string", 
      complex = "list", 
      null = "list", 
      na = "null"))
}

#' getDefaultFromJSONargs
#' 
#' @description
#'  To be called by any function in this package that makes use of 
#'  jsonlite::fromJSON. Ensures fromJSON conversion consistency across all functions.
#'
getDefaultFromJSONargs <- function() {
  return(
    list(
      simplifyDataFrame = FALSE, 
      simplifyMatrix = FALSE, 
      flatten = FALSE, 
      simplifyVector = FALSE))
}


#' Null-coalescing operator
#' 
#' @description
#' This helper function provides a null-coalescing operator, which returns 
#' the first non-`NULL` value between two arguments. It is useful for 
#' providing default values in cases where the first argument might be `NULL`.
#' 
#' @param a The primary value to check. If it is not null this value is returned.
#' @param b The fallback value to return if `a` is `NULL`.
#' 
#' @return The value of `a` if it is not `NULL`, otherwise the value of `b`.
`%||%` <- function(a, b) if (!is.null(a)) a else b
