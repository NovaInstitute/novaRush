# generate-sparql

#' dataFlureeGenerateSparql
#' @description
#' This doesn't work yet or is associated with a paid version
#' @param api_key Character. Your DataFluree API key. You can set it as an environment variable `dataFlureeAPIKEY`.
#' @param handle Character. Your DataFluree handle (typically your username).
#' @param datasetName Character. The name of the dataset you want to query.
#' @param prompt Character. A prompt describing the SPARQL query you want to generate.
#'
#' @returns An httr response object containing the generated SPARQL query.
#' @export

dataFlureeGenerateSparql <- function(api_key = Sys.getenv("dataFlureeAPIKEY"),
                                     handle  = NULL,
                                     ledger = NULL,
                                     datasetName = NULL,
                                     wh = NULL,
                                     s = NULL){

  if (is.null(api_key) || api_key == "") {stop("Please provide an API key.")}
  if (is.null(handle)  || handle  == "") {stop("Please provide a handle name.")}
  if (is.null(dataset) || dataset == "") {stop("Please provide a dataset name.")}
  if (is.null(prompt)  || prompt  == "") {stop("Please provide a prompt.")}

  purrr::map(datasetName[1], ~{
    url <- sprintf("https://data.flur.ee/api/%s/generate-sparql", handle)
    q <- list(datasets = list(.), prompt   = prompt)
    res <- httr::POST(
      url,
      httr::add_headers(
        "Content-Type"  = "application/json",
        "Accept"        = "application/json",
        "Authorization" = paste("Bearer", api_key),
        "x-user-handle" = handle
        ),
      body = jsonlite::toJSON(q, auto_unbox = TRUE),
      encode = "raw"
      )
    cat(res$status_code, "\n")
    cat(httr::content(res, "text", encoding = "UTF-8"), "\n")
    res
  })

}
