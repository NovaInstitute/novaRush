
#' Create a new ledger
#' 
#' @description
#' This function creates a new ledger with the specified name. If a name is not
#' explicitly provided, the previously configured ledger name (stored in the
#' system environment) will be used. The same applies to the `config` parameters
#' of the Fluree instance. As for the `transaction` parameter if no
#' initial transaction is provided a default transaction will be sent to the newly
#' created ledger.
#' 
#' @param ledgerName (`string`)\cr
#'   The name of the ledger to be created.
#' @param config (`list`)\cr
#'   The configuration parameters of the Fluree instance.
#' @param transaction (`list`)\cr
#'   The body of the initial transaction to add to the new ledger.
#' 
#' @export
createLedger = function(ledgerName = NULL, config = NULL, transaction = NULL) {
  
  if (is.null(config)) {
    config <- fromJSON(Sys.getenv("config"))
  }
  
  isFlureeHosted <- config$isFlureeHosted
  create <- config$create
  host <- config$host
  port <- config$port
  ledger <- config$ledger
  signMessages <- config$signMessages
  privateKey <- config$privateKey
  apiKey <- config$apiKey
    
  url <- paste0('http://', host)
  if (!is.null(port)) {
    url <- paste(url, sep = ":", port)
  }
  url <- paste0(url, "/fluree/create")
    
  body <- list(
    ledger = ledgerName %||% ledger,
    insert = list(message = "success")
  )
    
  if (!is.null(transaction)) {
    body <- modifyList(body, transaction)
  }
    
  header = 'application/json'
  finalBody = toJSON(body, auto_unbox = TRUE, pretty = FALSE)
    
  if (!is.null(signMessages) && signMessages && !is.null(privateKey)) {
    finalBody <- flureeCrypto:::serialize_jws(finalBody, privateKey)
    header = 'application/jwt'
  }
    
  response <- POST(
    url = url,
    add_headers(`Content-Type` = header),
    body = finalBody,
    encode = "raw"
  )
    
  if (http_error(response)) {
    stop("Failed to create ledger: ", content(response, "text"))
  }
    
  # Output the results
  print(content(response, as = "text"))
}

