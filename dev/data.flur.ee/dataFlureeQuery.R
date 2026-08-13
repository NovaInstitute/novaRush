#' dataFlureeQuery
#'
#' @param query_body list resulting from dataFlureeBuildQuery()
#' @param api_key Character string API key for private datasets. Defaults to Sys.getenv("dataFlureeAPIKEY")
#' @param handle Character string user handle. Typically username
#' @param endpoint Character string URL of the endpoint. Defaults to "https://data.flur.ee/api/fluree/query"
#' @param timeout_s Integer timeout in seconds. Defaults to 20
#' @param include_auth_headers Logical whether to include auth headers. Defaults to TRUE
#'
#' @returns
#' @export
#'
#' @examples
dataFlureeQuery <- function(query_body,
                            api_key = Sys.getenv("dataFlureeAPIKEY"),
                            handle,
                            endpoint = "https://data.flur.ee/fluree/query",
                            include_auth_headers = TRUE) {
  if (!is.list(query_body)) stop("query_body must be a list returned by dataFlureeBuildQuery()")
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

  res <- httr::POST(
    endpoint,
    httr::add_headers(.headers = hdrs),
    body   = jsonlite::toJSON(query_body, auto_unbox = TRUE),
    encode = "raw")
  res$status_code

  # Return parsed JSON + status for easy debugging
  list(
    status  = res$status_code,
    text    = httr::content(res, "text", encoding = "UTF-8"),
    parsed  = tryCatch(fromJSON(content(res, "text", encoding = "UTF-8")), error = function(e) NULL)
  )
}
