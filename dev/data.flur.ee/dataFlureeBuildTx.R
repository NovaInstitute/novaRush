

#' dataFlureeBuildTx
#'
#' @param handle
#' @param context A list representing the JSON-LD context. If NULL, a default SurveyCTO context will be used.
#' @param datasetName
#' @param wh A named list representing the "where" clause of the transaction. Optional.
#' @param kv A named list of key-value pairs representing the data to be inserted.
#' The keys should be valid RDF predicates, and the values can be literals or IRIs. !!! We should have a function for this
#'
#' @returns list with keys `@context`, `ledger`, `insert`
#' @export
#'
#' @examples
#' # Get data
#' srv <- "KiA_adaptation_ACTIVE"
#' dataFlureeCreateLedger(handle = "christiaanpauw", datasetName = srv, description = paste("Survey CTO survey:", srv))
#' kia_adapt <- novaCTO::readCTO(srv)
#' # Create the vocabulary in Fluree first
#' data <- kia_adapt$data[[1]]
#' datatriples <- pivot_longer_with_type(data) # inspect and edit if needed
#' # Make the insert by hand
#' vocab_tx <- list(ledger = "christiaanpauw/KiA_adaptation_ACTIVE", insert = schema_from_tripples(df = datatriples, name = "adapt"))
#' jsonlite::toJSON(vocab_tx, auto_unbox = TRUE, pretty = TRUE)
#' # Or use the helper functions
#' kv <- schema_from_tripples(df = datatriples, name = "adapt")
#' vocab_tx2 <-  dataFlureeBuildTx(handle = "christiaanpauw", datasetName = srv, kv = kv)
#' dataFlureeInsert(handle = "christiaanpauw", tx = vocab_tx2)
#' # Now insert quesitons
#' formdef <- kia_adapt$fromschema$kia_adaptation
#' formRDF <- map_cto_to_rdf(formdef, base_uri = glue::glue("https://novapc.surveycto.com/{srv}/"), instrument = "KiA_adaptation_ACTIVE")
#' kvform <- properties2kv(formRDF, id = "https://novapc.surveycto.com/KiA_adaptation_ACTIVE")
#' tx <- dataFlureeBuildTx(handle = "christiaanpauw", datasetName = srv, kv = kvform)
#' dataFlureeInsert(handle = "christiaanpauw", tx = tx)
#' # Now load the data itself
#' kvdata <- datatriples %>% group_by(subject) %>% nest() %>% mutate(kv = map(data, ~properties2kv(., id = subject, typename = "type", to_type = "survey:Response")))
#' txdata <- datatriples %>% group_by(subject) %>% nest() %>%
#' mutate(kv = map(data, ~properties2kv(., id = subject, typename = "type", to_type = "survey:Response"))) %>%
#' mutate(tx = map(kv, ~dataFlureeBuildTx(handle = "christiaanpauw", datasetName = srv, kv = .x, debug = FALSE)))
#' map(txdata$tx, ~dataFlureeInsert(handle = "christiaanpauw", tx = .x))
#' # Or do everyting all at once
#' dataFlureeInsert(handle = "christiaanpauw", tx = dataFlureeBuildTx(handle = "christiaanpauw", datasetName = srv, kv = txdata$tx, debug = FALSE))
dataFlureeBuildTx <- function(context = NULL,
                              handle = NULL,
                              datasetName,
                              wh = NULL,
                              kv = list(
                                "@id"          = "ex:alice3",
                                "@type"        = "schema:Person",
                                "schema:name"  = "Alice3",
                                "schema:email" = "alice3@example.org"
                              ),
                              debug = TRUE) {

  if (is.null(context_df)) {
    ctx <- make_surveycto_context_list()
  } else {
    ctx <- context
  }

  if (is.null(datasetName)) {
    stop("Please provide a dataset name (ledger) for this transaction.")
  }

  if (!is.null(handle)) {
    datasetName <- paste0(handle, "/", datasetName)
  }

  if (debug) {
    message("Using dataset (ledger): ", datasetName)
  }

  l <- list(
    #"@context" = ctx,
    ledger     = datasetName,  # transact expects `ledger`, not `from`
    insert     = kv

  )

  if (!is.null(wh)) {
    l$where <- wh
  }

  l
}
