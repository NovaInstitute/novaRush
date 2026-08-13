#' Create a Fluree ledger
#'
#' Creates a ledger through the Fluree v4 `/create` endpoint. Creation is
#' idempotent: an HTTP 409 response indicating that the ledger already exists is
#' returned as an existing-ledger result rather than raised as an error.
#'
#' @param config Fluree configuration created by [setConfig()].
#' @param ledgerName Ledger name. Defaults to `config$ledger`.
#' @param transaction Optional initial JSON-LD transaction fields supported by
#'   the server's create endpoint.
#' @param signMessage Retained for compatibility. Signed create requests are not
#'   implemented by this helper; use bearer authentication in `config`.
#' @param privateKey Retained for compatibility.
#'
#' @return The parsed Fluree response. For an existing ledger, a list with
#'   `created = FALSE`, `exists = TRUE`, and the server response is returned.
#' @export
createLedger <- function(config = NULL, ledgerName = NULL, transaction = NULL,
                         signMessage = FALSE, privateKey = NULL) {
  ledger <- ledgerName %||% if (!is.null(config)) config$ledger else NULL
  if (is.null(ledger) || length(ledger) != 1L || !nzchar(ledger)) {
    stop("Please provide a ledger name. Either as argument or within the config.",
         call. = FALSE)
  }
  if (is.null(config)) config <- setConfig(ledger = ledger)
  if (isTRUE(signMessage) || !is.null(privateKey)) {
    stop(paste(
      "Signed create requests are not supported by `createLedger()`.",
      "Configure a bearer token with `setConfig(apiKey = ...)`."
    ), call. = FALSE)
  }

  body <- list(ledger = ledger)
  if (!is.null(transaction)) body <- modifyList(body, transaction)
  body$ledger <- ledger
  response <- fluree_request(
    config, endpoint = "create", method = "POST", body = body,
    operation = paste0("create ledger ", ledger), allowStatus = 409L,
    returnResponse = TRUE
  )
  if (identical(response$status, 409L)) {
    return(list(created = FALSE, exists = TRUE, ledger = ledger,
                response = response$body))
  }
  response$body
}
