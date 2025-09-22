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
  
  # Convert the data to a JSON-LD Fluree insert statement.
  q <- jsonlite::toJSON(
    x = list('insert' = if (length(names(data)) == 0) { data } else { list(data) }), 
    dataframe = "rows",
    matrix = "rowmajor", 
    POSIXt = "string", 
    factor = "string",
    auto_unbox = TRUE, 
    pretty = FALSE, 
    Date = "ISO8601", 
    complex = "string", 
    null = "list")
  
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