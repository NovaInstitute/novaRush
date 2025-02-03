
#' Set keys for encryption
#' 
#' @description
#' Add a private key to use for signing.
#' This key will be added to the config of the current instance and  will also 
#' be used to sign messages by default when using the `signTransaction()` or
#' `signQuery()` functions on any future queries or transactions.
#' The public key and DID will be derived from this private key and added to 
#' the `config` of the current Fluree instance as well.
#' 
#' @param privateKey (`string`)\cr
#'   The private key to use for message signing as a hex string.
#' 
#' @export
setKeys = function(privateKey) {
  publicKey <- flureeCrypto::public_key_from_private(privateKey)
  accountId <- flureeCrypto::account_id_from_public(publicKey)
  did <- sprintf('did:fluree:%s', accountId)
  
  updateConfiguration(list(privateKey = privateKey, publicKey = publicKey, did = did))
}

#' Generate a new key pair.
#' 
#' @description
#' This method makes use of the flureeCrypto package to generate a new private key.
#' The public key and DID are then derived from the private key and these are added 
#' to the `config` of the current Fluree instance.
#' 
#' @export
generateKeyPair = function() {
  kp <- flureeCrypto::generate_keypair()
  privateKey <- kp[[1]]
  publicKey <- kp[[2]]
  accountId <- flureeCrypto::account_id_from_public(publicKey)
  did <- paste0('did:fluree:', accountId)
  updateConfiguration(list(privateKey, publicKey, did))
}

#' Get the private key
#' 
#' @description
#' Get the currently configured private key (if one has been set).
#' 
getPrivateKey = function() {
  return(Sys.getenv(config$privateKey))
}

#' Get the public key
#' 
#' @description
#' Get the currently configured public key (if one has been set).
#' 
#' @returns (`string`) | (`undefined`).
#' 
#' @export
getPublicKey = function() {
  return(Sys.getenv(config$publicKey))
}

#' Get the DID
#' 
#' @description
#' Get the currently configured DID (if one has been set).
#' 
#' @returns (`string`) | (`undefined`).
#' 
#' @export
getDid = function() {
  return(Sys.getenv(config$did))
}
