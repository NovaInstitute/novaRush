
initializeEnvironmentVariables = function(config = list()) {
  checkConfiguration(config)
  
  Sys.setenv(isFlureeHosted = config$isFlureeHosted)
  Sys.setenv(create = config$create)
  Sys.setenv(host = config$host)
  Sys.setenv(ledger = config$ledger)
  Sys.setenv(signMessages = config$signMessages)
  Sys.setenv(apiKey = config$apiKey)
  
  privateKey <- config$privateKey
  if (!is.null(privateKey)) {
    setKeys(privateKey)
  }
  
  Sys.setenv(connected = FALSE)
}


checkConfiguration = function(config, isConnecting = FALSE) {
  isFlureeHosted <- config$isFlureeHosted
  create <- config$create
  host <- config$host
  ledger <- config$ledger
  signMessages <- config$signMessages
  privateKey <- config$privateKey
  apiKey <- config$apiKey
  
    if (isConnecting) {
      if (!is.null(isFlureeHosted) && isFlureeHosted) {
        if (!is.null(create) && create) {
          stop("Cannot create a ledger through the Fluree hosted service API", call. = FALSE)
        }
      } else {
        if (is.null(host)) {
          stop("Host is required on either FlureeInstance or connect", call. = FALSE)
        }
      }
      if (is.null(ledger)) {
        stop("Ledger is required on either FlureeInstance or connect", call. = FALSE)
      }
    }
    
    if (!is.null(signMessages) && signMessages && is.null(privateKey)) {
      stop("Private key is required when signMessages is TRUE", call. = FALSE)
    }
    
    if (!is.null(isFlureeHosted) && isFlureeHosted) {
      if (!is.null(host)) {
        stop("Host should not be set when using the Fluree hosted service")
      }
      if (!is.null(port)) {
        stop("Port should not be set when using the Fluree hosted service")
      }
      if (is.null(apiKey) && is.null(privateKey)) {
        stop("Either an apiKey or a privateKey is required for signing messages when using the Fluree hosted service")
      }
    }
}

updateConfiguration = function(newConfig = list()) {
  mergedConfig <- modifyList(Sys.getenv(config), newConfig)
  if (!is.null(newConfig$defaultContext) && !is.null(self$config$defaultContext)) {
    mergedConfig$defaultContext <- mergeContexts(self$config$defaultContext, newConfig$defaultContext)
  }
  
  self$checkConfig(mergedConfig)
  self$config <- mergedConfig
  return(self)
}

setKeys = function(privateKey) {
  publicKey <- flureeCrypto::public_key_from_private(privateKey)
  accountId <- flureeCrypto::account_id_from_public(publicKey)
  did <- sprintf('did:fluree:%s', accountId)
  
  Sys.setenv(privateKey = privateKey, publicKey = publicKey, did = did)
}