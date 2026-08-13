
#' dataFlureeCreateDataset
#'
#' @param api_key fluree API key for data.flur.ee, default from Sys.getenv("dataFlureeAPIKEY")
#' @param handle fluree handle. Typically your username
#' @param datasetName A unique name for the dataset
#' @param storageType c("default", "ipfs")
#' @param description A description of the dataset
#' @param visibility c("public", "private")
#' @param tags A list of tags
#'
#' @returns
#' @export
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON
#'
#' @examples
#' res3 <- dataFlureeCreateLedger(
#'   datasetName = paste0("toets3/", as.integer(Sys.time())),
#'   handle = "christiaanpauw",
#'   description = "toets3", tags = list("R", "test"))
#' {"message":"Dataset created","data":{"ledgerId":387028092979389,"path":"fluree-jld/387028092979389"}}

dataFlureeCreateLedger <- function(api_key = Sys.getenv("dataFlureeAPIKEY"),
                                    handle  = NULL,
                                    datasetName = NULL,
                                    storageType = c("default", "ipfs")[1],
                                    description = "",
                                    visibility  = c("public", "private")[1],
                                    tags = list()) {

  if (is.null(api_key) || api_key == "") {stop("api_key is required") }
  if (is.null(datasetName) || datasetName == "") {stop("datasetName is required") }
  if (is.null(handle) || handle == "") {stop("handle is required") }

 url <- sprintf("https://data.flur.ee/api/%s/create-dataset", handle)
  q <- list(
    datasetName = datasetName,  # ensure unique
    storageType = storageType,                                      # or "ipfs"
    description = description,
    visibility  = "public",                                       # or "private"
    tags        = tags
  )

  res <- POST(
    url,
    add_headers(
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "Authorization" = paste("Bearer", api_key),
      "x-user-handle" = handle
    ),
    body   = toJSON(q, auto_unbox = TRUE),
    encode = "raw"
  )

  cat(res$status_code, "\n")
  cat(content(res, "text", encoding = "UTF-8"), "\n")
  res
}

# {"message":"Dataset created","data":{"ledgerId":387028092979389,"path":"fluree-jld/387028092979389"}}

# {"message":"Dataset created","data":{"ledgerId":387028092979388,"path":"fluree-jld/387028092979388"}}
