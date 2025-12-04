
# Build a FlureeQL query body with safe defaults
#' dataFlureeBuildQuery
#'
#' @param handle Character. The Fluree handle (typically your user name or org name)
#' @param datasetName Character. The name of the dataset
#' @param wh A list representing the "where" clause of the query
#' @param s A list representing the "select" clause of the query
#' @param context A list representing the "context" clause of the query
#' @param limit Integer. The maximum number of results to return. Default is 50.
#' @param orderBy Character or list. The "orderBy" clause of the query
#' @param offset Integer. The "offset" clause of the query
#'
#' @returns
#' @export
#'
#' @examples
#' qq <- dataFlureeBuildQuery(handle = "christiaanpauw", datasetName = "test1")
#'
dataFlureeBuildQuery <- function(handle,
                                 datasetName,
                                 wh      = NULL,
                                 s       = NULL,
                                 context = NULL,
                                 orderBy = NULL,
                                 offset  = NULL) {
  if (missing(handle) || !nzchar(handle)) stop("handle is required")
  if (missing(datasetName) || !nzchar(datasetName)) stop("datasetName is required")

  # Defaults that mean "all subjects with all properties"
  if (is.null(wh)) wh <- list("@id" = "?s")
  if (is.null(s))  s  <- setNames(list(list("*")), "?s")  # {"?s": ["*"]}

  body <- list(
    from   = paste0(handle, "/", datasetName),
    where  = wh,
    select = s
  )
  if (!is.null(context)) body[["@context"]] <- context
  if (!is.null(orderBy)) body$orderBy <- orderBy
  if (!is.null(offset))  body$offset  <- offset
  body
}
