
#' Create a New Ledger
#' 
#' @description
#' This function creates a new ledger with the specified name.
#' If no `config` is provided, the default parameters will be used.
#' If no initial transaction is provided, a default transaction will be sent.
#' 
#' @param config (`list`)\cr
#'   The configuration parameters of the Fluree instance.
#' @param ledgerName (`string`)\cr
#'   The name of the ledger to be created. If NULL, use `config$ledger`.
#' @param transaction (`list`)\cr
#'   The body of the initial transaction to add to the new ledger.
#' 
#' @importFrom jsonlite toJSON
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom httr http_error
#' 
#' @export
createLedger <- function(config = NULL, ledgerName = NULL, transaction = NULL, signMessage = FALSE, privateKey = NULL) {
  ledger <- ledgerName %||% config$ledger
  if (is.null(ledger)) {
    stop("Please provide a ledger name. Either as argument or within the config.")
  }
  if (is.null(config)) {
    config = setConfig(ledger = ledger)
  }
  
  body <- list(
    ledger = ledger,
    insert = list(message = "success")
  )
  
  if (!is.null(transaction)) {
    body <- transaction
    body$ledger <- ledger
  }
  
  finalBody <- jsonlite::toJSON(body, auto_unbox = TRUE, pretty = FALSE)
  
  contentType <- "application/json"
  shouldSign <- signMessage %||% config$signMessages
  if (isTRUE(shouldSign)) {
    key <- privateKey %||% getKey()
    if (is.null(key)) {
      stop("Please provide a key for signing. Either as argument or set one using setKey()")
    } else {
      finalBody <- flureeCrypto:::serialize_jws(finalBody, pk)
      contentType <- "application/jwt"
    }
    
  }
  
  url <- paste0("https://", config$host)
  if (!is.null(config$port)) {
    url <- paste0(url, ":", config$port)
  }
  url <- paste0(url, "/fluree/create")
  
  response <- httr::POST(
    url,
    httr::add_headers(`Content-Type` = contentType),
    body = finalBody,
    encode = "raw"
  )
  
  if (httr::http_error(response)) {
    stop("Ledger creation failed: ", httr::content(response, "text", encoding = "UTF"))
  }
  
  return(httr::content(response, "text", encoding = "UTF-8"))
}

