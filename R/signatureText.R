#' description: This function generates the signature text for the query
#' @param ledgerName Character. The name of the ledger.
#' @param privateKey Character. The private key.
#' @param body Character. The body of the query.
#' @param authId Character. The authId.
#' @return character
#' @export

signatureText <- function(ledgerName,
                          privateKey,
                          endpoint,
                          body,
                          authId){

fluree_link <- Sys.getenv("fluree_link")

signText <- paste0("const { signQuery } = require('@fluree/crypto-utils');
const fetch = require('node-fetch');
const queryAsUser = () =>
  fetch(
    `", fluree_link, ledgerName,"/", endpoint,"`,
    signQuery(
      '", privateKey,"',
      '", jsonlite::toJSON(as.character(body)),"',
      '", endpoint,"',
      '",ledgerName,"',
      '", authId,"'
    )
  )
    .then((res) => res.json())
    .then((res) =>
      console.log(JSON.stringify(res))
    );
queryAsUser();")
return(signText)
}
