#' Configure a Fluree connection
#'
#' Creates the connection configuration used by the functional API and the
#' [FlureeInstance] R6 client. The legacy `host` and `port` arguments remain
#' supported, while `baseUrl` is the preferred Fluree v4 form.
#'
#' @param host Host name without a protocol. Retained for compatibility.
#' @param port Optional HTTP port. Retained for compatibility.
#' @param ledger Ledger name without a branch suffix.
#' @param signMessages Whether requests should use legacy JWS signing.
#' @param baseUrl Complete server URL, for example `http://localhost:8090`.
#' @param branch Default ledger branch.
#' @param apiKey Optional bearer token. Prefer supplying this through an
#'   environment variable rather than source code.
#' @param timeout Request timeout in seconds.
#' @param apiPath Fluree HTTP API base path.
#' @param defaultContext Default JSON-LD context merged into operations.
#' @param create Whether [FlureeInstance] should create the ledger when it
#'   connects.
#' @param isFlureeHosted Whether the connection targets a hosted Fluree server.
#'
#' @return A `nova_fluree_config` list.
#' @export
setConfig <- function(
    host = NULL,
    port = NULL,
    ledger = NULL,
    signMessages = FALSE,
    baseUrl = NULL,
    branch = "main",
    apiKey = NULL,
    timeout = 60,
    apiPath = "/v1/fluree",
    defaultContext = NULL,
    create = FALSE,
    isFlureeHosted = FALSE) {
  if (is.null(ledger) || length(ledger) != 1L || !nzchar(ledger)) {
    stop("Please provide a valid ledger name.", call. = FALSE)
  }
  if (length(branch) != 1L || is.na(branch) || !nzchar(branch)) {
    stop("Please provide a valid branch name.", call. = FALSE)
  }
  timeout <- suppressWarnings(as.numeric(timeout))
  if (length(timeout) != 1L || is.na(timeout) || !is.finite(timeout) || timeout <= 0) {
    stop("`timeout` must be one positive number of seconds.", call. = FALSE)
  }

  if (is.null(baseUrl)) {
    host <- host %||% if (isTRUE(isFlureeHosted)) "data.flur.ee" else "localhost"
    protocol <- if (isTRUE(isFlureeHosted) || identical(host, "data.flur.ee")) {
      "https"
    } else {
      "http"
    }
    baseUrl <- paste0(protocol, "://", host)
    if (!is.null(port)) {
      baseUrl <- paste0(baseUrl, ":", port)
    }
  } else {
    if (length(baseUrl) != 1L || is.na(baseUrl) ||
        !grepl("^https?://", baseUrl)) {
      stop("`baseUrl` must begin with http:// or https://.", call. = FALSE)
    }
    parsed <- httr::parse_url(baseUrl)
    host <- parsed$hostname
    port <- parsed$port
  }

  apiPath <- paste0("/", gsub("^/+|/+$", "", apiPath))
  config <- list(
    baseUrl = sub("/+$", "", baseUrl),
    apiPath = apiPath,
    host = host,
    port = port,
    ledger = ledger,
    branch = branch,
    signMessages = isTRUE(signMessages),
    apiKey = apiKey,
    timeout = timeout,
    defaultContext = defaultContext,
    create = isTRUE(create),
    isFlureeHosted = isTRUE(isFlureeHosted)
  )
  structure(config, class = c("nova_fluree_config", "list"))
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
#' \dontrun{
#' newConfig <- list(ledger = "test2", port = "8090")
#' updatedConfig <- updateConfig(config, newConfig)
#'
#' }
#' @export
updateConfig <- function(config, newConfig = list()) {
  mergedConfig <- modifyList(config, newConfig)
  if (!is.null(newConfig$defaultContext) && !is.null(config$defaultContext)) {
    mergedConfig$defaultContext <- mergeContexts(
      config$defaultContext, newConfig$defaultContext
    )
  }
  class(mergedConfig) <- unique(c(class(config), "list"))
  mergedConfig
}
