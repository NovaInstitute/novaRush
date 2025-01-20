
initializeEnvironmentVariables = function(config = list()) {
  checkConfiguration(config)
  
  json_list <- toJSON(config, auto_unbox = TRUE)
  Sys.setenv(config = json_list)
  
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
  config <- fromJSON(Sys.getenv("config"))
  mergedConfig <- modifyList(config, newConfig)
  if (!is.null(newConfig$defaultContext) && !is.null(config$defaultContext)) {
    mergedConfig$defaultContext <- mergeContexts(config$defaultContext, newConfig$defaultContext)
  }
  
  checkConfiguration(mergedConfig)
  json_list <- toJSON(mergedConfig, auto_unbox = TRUE)
  Sys.setenv(config = json_list)
}


connect = function() {
  config <- fromJSON(Sys.getenv("config"))
  checkConfiguration(config, TRUE)
  Sys.setenv(connected = TRUE)
  
  tryCatch({
    if (isTRUE(config$create)) {
      createLedger()
    }
    #TODO:  #self$testLedger()
    
  }, error = function(err) {
    Sys.setenv(connected = FALSE)
    stop(err)
  })
}

