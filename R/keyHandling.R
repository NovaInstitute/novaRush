setKeys = function(privateKey) {
  publicKey <- flureeCrypto::public_key_from_private(privateKey)
  accountId <- flureeCrypto::account_id_from_public(publicKey)
  did <- sprintf('did:fluree:%s', accountId)
  
  updateConfiguration(list(privateKey = privateKey, publicKey = publicKey, did = did))
}

generateKeyPair = function() {
  kp <- flureeCrypto::generate_keypair()
  privateKey <- kp[[1]]
  publicKey <- kp[[2]]
  accountId <- flureeCrypto::account_id_from_public(publicKey)
  did <- paste0('did:fluree:', accountId)
  updateConfiguration(list(privateKey, publicKey, did))
}


getPrivateKey = function() {
  return(Sys.getenv(config$privateKey))
}


getPublicKey = function() {
  return(Sys.getenv(config$publicKey))
}


getDid = function() {
  return(Sys.getenv(config$did))
}
