
#' getOntology
#' Fetch and parse an ontology from a given URL
#' @param ontology_url Character string representing the URL of the ontology in JSON-LD format.
#'
#' @returns A list representing the parsed ontology.
#' @export
#'
#' @examples
#' # Download the InfoComm ontology
#' ont <- getOntology(ontology_url = "https://datadudes.xyz/infocomm")
#' # Create the f:DataModel to be used in Fluree
#' dM <- dataFlureeOnt2Model( ont,
#' model_id = "https://datadudes.xyz/infocomm",
#' label = "InfoComm Ontology",
#' comment = "Ontology for information and comunication",
#' prefLabel = "nfoComm Ontology")
#' # Prepare the transaction
#' tt <- list( ledger = "christiaanpauw/KiA_adaptation_ACTIVE", insert = dM)
#' # # Insert into Fluree
#' dataFlureeInsert(tx_body = tt, handle = "christiaanpauw")

getOntology <- function(ontology_url) {

    # Download from URL
    response <- httr::GET(
      url = ontology_url,
      httr::add_headers(Accept = "application/json")
    )
    if (httr::status_code(response) != 200) {
      stop("Failed to fetch ontology from URL. Status code: ", status_code(response))
    }
    jsonld_text <- httr::content(response, as = "text", encoding = "UTF-8")
    jsonlite::fromJSON(jsonld_text, simplifyVector = FALSE, flatten = FALSE)
}


# ttx <- list(ledger = "christiaanpauw/KiA_adaptation_ACTIVE", insert = tt)
# dataFlureeInsert(tx_body = ttx, handle = "christiaanpauw")
