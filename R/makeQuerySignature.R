#' makeQuerySignature
#' @description Helper function to sign a query in Fluree
#' @param ledgerName Character. The name of the ledger.
#' @param privateKey Character. The private key.
#' @param queryString Character. The query string.
#' @param authId Character. The authId.
#' @return character
#' @export

makeQuerySignature <- function(ledgerName,
                               privateKey = Sys.getenv("privateKey") ,
                               queryString,
                               authId = Sys.getenv("authId")){
  # Construct the inline Node.js code
  jsCode <- paste0("
    const { signQuery } = require('@fluree/crypto-utils');
    console.log(JSON.stringify(signQuery(
      '", privateKey, "',
      '", queryString, "',
      'query',
      '", ledgerName, "',
      '", authId, "'
    )))")

  # Execute the JavaScript code inline with node
  signedQuery <- system(paste('node -e "', jsCode, '"'), intern = TRUE)

  if(any(grepl("using Node.js", signedQuery))){
    signedQuery <- signedQuery[-1]
  }
  signedQuery <- paste(signedQuery, collapse = "")

 return(signedQuery)
}
