createLedger = function(ledgerName = NULL, transaction = NULL) {
  config <- fromJSON(Sys.getenv("config"))
  
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

