#' flureeInsert
#'
#'@description Inserts data into a Fluree ledger.
#'@param data list A list representing a JSON-LD object that must be inserted
#'  into a Fluree ledger.
#'@param config list The configuration list for the Fluree instance.
#'@param signTransaction logical Indicates whether the transaction should be 
#'  signed or not.
#'@param apiKey character A Fluree API key.
#'@return The output of novaRush::sendTransaction.
#'
#'@export
#'
flureeInsert <- function(data, config, signTransaction, apiKey = NULL) {
  
  if (length(names(data)) == 0) {
    res <- lapply(
      X = data, 
      FUN = flureeInsert, 
      config = config, 
      signTransaction = signTransaction, 
      apiKey = apiKey)
    return(res)
  }
  
  if ("@context" %in% names(data)) {
    ctx <- data[["@context"]]; data[["@context"]] <- NULL
    x <- list(
      '@context' = ctx,
      'insert' = list(data))
  } else {
    x <- list(
      'insert' = list(data))
  }
  
  # Convert the data to a JSON-LD Fluree insert statement.
  q <- do.call(
    what = jsonlite::toJSON, 
    args = c(
      list(x = x), 
      novaRush:::getDefaultToJSONargs()), 
    quote = FALSE)
  
  # Create the transaction.
  tx <- novaRush::transact(
    config = config, 
    transaction = q, 
    signTransaction = signTransaction, 
    apiKey = apiKey)
  
  # Send the transaction.
  res <- tryCatch({
    novaRush::sendTransaction(tx)
  }, error = function(e) {
    message(sprintf("ERROR: %s", as.character(e)))
    NULL
  })
  
  return(res)
  
}