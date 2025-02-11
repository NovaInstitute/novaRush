
#' Set the configuration for the Fluree instance
#' 
#' @description
#' This function validates the configuration parameters.
#' Once checked the configuration parameters are added to the system environment.
#' 
#' @param config (`list()`)\cr
#'   The parameters to configure the Fluree instance with.
#' 
#' @examples
#' config <- list(host = "localhost", port = "58090", ledger = "ledger1", create = TRUE)
#' setConfiguration(config)
#' 
#' @importFrom jsonlite toJSON
#' 
#' @export
setConfiguration = function(config = list()) {
  checkConfiguration(config)
  
  json_list <- toJSON(config, auto_unbox = TRUE)
  Sys.setenv(config = json_list)
  
  privateKey <- config$privateKey
  if (!is.null(privateKey)) {
    setKeys(privateKey)
  }
  
  Sys.setenv(connected = FALSE)
}

#' Check the configuration parameters
#' 
#' @description
#' This function performs a check on all the configuration parameters to ensure
#' validity and that they are used in correct combination.
#' 
#' @param config (`list()`)\cr
#'   The parameters to configure the Fluree instance with.
#' @param isConnecting (`logical`)\cr
#'   If TRUE the instance is attempting to establish a connection to the host.
#' 
checkConfiguration = function(config, isConnecting = FALSE) {
  isFlureeHosted <- config$isFlureeHosted
  create <- config$create
  host <- config$host
  ledger <- config$ledger
  signMessages <- config$signMessages
  privateKey <- config$privateKey
  apiKey <- config$apiKey
  
    if (isConnecting) {
      if (isTRUE(isFlureeHosted)) {
        if (isTRUE(create)) {
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
        stop("Host should not be set when using the Fluree hosted service", call. = FALSE)
      }
      if (!is.null(port)) {
        stop("Port should not be set when using the Fluree hosted service", call. = FALSE)
      }
      if (is.null(apiKey) && is.null(privateKey)) {
        stop("Either an apiKey or a privateKey is required for signing messages when using the Fluree hosted service", call. = FALSE)
      }
    }
}

#' Update the current Fluree configuration
#' 
#' @description
#' Update the configuration parameters of the current Fluree instance.
#' The existing `config` (stored in the system environment) will be merged with
#' the new one and all existing fields will be replaced with the new ones if
#' applicable; except for the `defaultContext` field. Instead of replacing the
#' `defaultContext` the old and new contexts are merged to form the updated `defaultContext`.
#' The new merged `config` will then be added back as an environment variable.
#' 
#' @param newConfig (`list()`)\cr
#'   The new parameters to configure the Fluree instance with.
#' 
#' @examples
#' newConfig <- list(ledger = "ledger2", port = "8090")
#' updateConfiguration(newConfig)
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
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

#' Connect to the Fluree instance
#' 
#' @description
#' This function checks that a connection to the running Fluree instance can be
#' established. The connection needs to be made before any transactions or
#' queries can be sent.
#' 
#' @examples
#' config <- list(
#'    host = 'localhost', 
#'    port = 58090, 
#'    ledger = 'policy-view-age', 
#'    create = TRUE, 
#'    privateKey = key_get("privateKey", keyring = "Fluree"))
#' 
#' setConfiguration(config = config)
#' 
#' connect()
#' 
#' @export
connect = function() {
  config <- fromJSON(Sys.getenv("config"))
  checkConfiguration(config, TRUE)
  Sys.setenv(connected = TRUE)
  
  tryCatch({
    if (isTRUE(config$create)) {
      createLedger()
    }
    testLedger()
    
  }, error = function(err) {
    Sys.setenv(connected = FALSE)
    stop(err)
  })
}


#' @description
#' This function is used within the `connect()` method to test that the ledger was
#' created successfully and that it can be interacted with.
#' 
testLedger = function() {
  print("Testing ledger")
  qry <- query(list(where = list("@id" = "?s", "?p" = "?o"), select = list("?s"), limit = 1))
  query(query = qry)
}

