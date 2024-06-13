#' generateKeyPair
#' @description Helper function to generate a key pair in Fluree
#' @return character
#' @export
generateKeyPair <- function(){
  # Construct the inline Node.js code
  jsCode <- paste0("
   const { generateKeyPair, getSinFromPublicKey } = require('@fluree/crypto-utils');
  const { publicKey: authorityPubKey, privateKey: authorityPrivKey } = generateKeyPair();
  const authorityAuthId = getSinFromPublicKey(authorityPubKey);
  authority = {
      authId: authorityAuthId,
      pubKey: authorityPubKey,
      privKey: authorityPrivKey,
    };
  console.log(JSON.stringify(authority))")

  # Escape double quotes for inline execution
  jsCode <- gsub("\"", "\\\"", jsCode)

  # Execute the JavaScript code inline with node
  keyPair <- system(paste('node -e "', jsCode, '"'), intern = TRUE)

  if(any(grepl("using Node.js", keyPair))){
    keyPair <- keyPair[-1]
  }
  keyPair <- paste(keyPair, collapse = "")
  return(keyPair)
}
