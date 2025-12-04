
#' Set the Fluree Configuration Parameters
#' 
#' @description
#' This function returns a default configuration for the Fluree instance.
#' It is invoked whenever a transaction/query is attempted without providing a
#' relevant config. 
#' Optionally partial parameters may be provided with defaults configured for 
#' any omitted ones.
#' 
#' @param host (`string`)\cr
#'   The host name to send the transactions/queries to.
#' @param ledger (`string`)\cr
#'   The name of the ledger to be transacted to.
#' @param signMessages (`logical`)\cr
#'   Set to `TRUE` if all messages should be signed before transacting.
#' 
#' @returns The configuration parameters configured as a R list.
#' 
#' @examples
#' config <- setConfig(ledger = "test1")
#' 
#' @export
setConfig = function(host = NULL, port = NULL, ledger, signMessages = NULL) {
  if (is.null(host)) {
    host = "datadudes2.xyz"
  }
  
  if (is.null(ledger)) {
    stop("Please provide a valid ledger to transact with.")
  }
  
  if (is.null(signMessages)) {
    signMessages = TRUE
  }

  config <- list(
    host = host,
    port = port,
    ledger = ledger,
    signMessages = signMessages)
  
  return(config)
}


#' Update the Current Fluree Configuration
#' 
#' @description
#' Update the configuration parameters of the current Fluree instance.
#' The `existingConfig` will be merged with the `newConfig` and all existing 
#' fields will be replaced with the new ones if applicable; except for the 
#' `defaultContext` field. Instead of replacing the context field the old and 
#' new contexts are merged to form the updated `defaultContext`.
#' The merged `newConfig` is then returned.
#' 
#' @param config (`list()`)\cr
#'   The existing Fluree configuration parameters.
#' @param newConfig (`list()`)\cr
#'   The new parameters to configure the Fluree instance with.
#' 
#' @returns The new combined list of configuration parameters.
#' 
#' @examples
#' newConfig <- list(ledger = "test2", port = "8090")
#' updatedConfig <- updateConfig(config, newConfig)
#' 
#' @export
updateConfig = function(config, newConfig = list()) {
  
  mergedConfig <- modifyList(config, newConfig)
  if (!is.null(newConfig$defaultContext) && !is.null(config$defaultContext)) {
    mergedConfig$defaultContext <- mergeContexts(config$defaultContext, newConfig$defaultContext)
  }
  return(mergedConfig)
}

