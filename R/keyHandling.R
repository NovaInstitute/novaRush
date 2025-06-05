
#' Store Private Key
#'
#' @description
#' This function securely stores a private key in the system keyring
#' using the `key_set()` function from the `keyring` package.
#' The key is stored under service name `"privateKey"` and keyring `"Fluree"`.
#'
#' NOTE: This function will prompt for input.
#'
#' @importFrom keyring key_set
#'
#' @export
setKey = function() {
  keyring::key_set("privateKey", keyring = "Fluree")
}

#' Retrieve Private Key
#' 
#' @description
#' This method retrieves the stored private key using the `key_get()` method of
#' the `keyring` package. Note the private key must have been previously stored
#' using the `setKey()` method.
#' 
#' NOTE: This function should be used with caution. Only invoke this function
#' withing another function call and never store your private key as an
#' environment variable.
#' 
#' @importFrom keyring key_get
#' 
#' @export
getKey = function() {
  return(key_get("privateKey", keyring = "Fluree"))
}

#' Generate a New Key Pair
#' 
#' @description
#' This method uses the `flureeCrypto` package to generate a new public and
#' private key pair.
#' 
#' @return A list containing the generated keys (the first element being the
#' private key and the second being the public key).
#' 
#' @importFrom flureeCrypto generate_keypair
#' @importFrom flureeCrypto account_id_from_public
#' 
#' @export
generateKeyPair = function() {
  kp <- flureeCrypto::generate_keypair()
  return(kp)
}
