
#' dataFlureeInsert
#'
#' @param tx_body
#' @param api_key
#' @param handle
#' @param endpoint
#' @param include_auth_headers
#'
#' @returns
#' @export
#'
#' @examples
#' tx1 <- dataFlureeBuildTx(handle = "christiaanpauw", datasetName = "dataset")
dataFlureeInsert <- function(tx_body,
                             api_key = Sys.getenv("dataFlureeAPIKEY"),
                             handle,
                             endpoint = "https://data.flur.ee/fluree/transact",
                             include_auth_headers = TRUE){


  if (!is.list(tx_body)) stop("transaction_body must be a list returned by dataFlureeBuidsTx()")
  hdrs <- c(
    "Content-Type" = "application/json",
    "Accept"       = "application/json"
  )
  if (isTRUE(include_auth_headers)) {
    if (is.null(api_key) || !nzchar(api_key)) stop("api_key is required for private datasets")
    if (is.null(handle)  || !nzchar(handle))  stop("handle is required when sending auth headers")
    hdrs <- c(hdrs,
              "Authorization" = paste("Bearer", api_key),
              "x-user-handle" = handle
    )
  }

  res_tx <- POST(
    endpoint,  # NOTE: no /api
    add_headers(
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "Authorization" = paste("Bearer", api_key),
      "x-user-handle" = handle
    ),
    body   = toJSON(tx_body, auto_unbox = TRUE),
    encode = "raw"
  )
  cat(res_tx$status_code, "\n")
  res_tx

}
