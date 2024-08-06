#' flureeFetch
#' @description Helper function to fetch data from Fluree
#' @param path Character. The path to the Fluree database.
#' @param body List. The body of the request.
#' @param fluree_link Character. The link to the Fluree database.
#'
#' @return character
#' @export
#' @import httr


# Helper function to fetch data from Fluree
flureeFetch <- function(path, method, body) {
  res <- httr::VERB(
    verb = method,
    url = paste0(path),
    body = body,
    encode = "json",
    content_type_json()
  )
  delay(1000)
  response <- list()
  response[["code"]] <- res$status_code
  response[["content"]] <- content(res, "text")
  json_reponse <- jsonlite::toJSON(response, auto_unbox = TRUE)
  print(json_reponse)
  return(json_reponse)
}

# Helper function to delay
delay <- function(ms) {
  Sys.sleep(ms / 1000)
}
