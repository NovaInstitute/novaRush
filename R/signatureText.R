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
signText <- paste0("const { signQuery } = require('@fluree/crypto-utils');
const fetch = require('node-fetch');
const queryObj = {
  select: ['*'],
  from: '_auth',
};
const queryString = JSON.stringify(queryObj);
const queryAsUser = () =>
  fetch(
    `http://localhost:8090/fdb/authority/test/", endpoint,"`,
    signQuery(
      '", privateKey,"',
      '", as.character(body),"',
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
