
#' cto_to_jsonld
#' @description
#' Standalone function to convert triples to JSON-LD
#'
#' @param formdef A data frame representing the form definition from SurveyCTO
#' @param base_uri Base URI for the identities formed from the form definition
#' @param instrument Name of the instrument (form) being processed
#'
#' @returns
#' @export
#'
#' @examples
#' srv <- "KiA_adaptation_ACTIVE"
#' kia_adapt <- novaCTO::readCTO(srv)
#' formdef <- kia_adapt$fromschema$kia_adaptation
#' cto_to_jsonld(formdef,
#'   base_uri = glue::glue("https://novapc.surveycto.com/{srv}"),
#'   instrument = "KiA_adaptation_ACTIVE")

cto_to_jsonld <- function(formdef,
                          base_uri = "https://https://novapc.surveycto.com/",
                          instrument = NULL) {
  if (is.null(instrument)) {
    stop("Instrument name must be provided.")
  }
  triples <- map_cto_to_rdf(formdef, base_uri, instrument )
  surveycto_context <- make_surveycto_centext()
  jsonld_obj <- triples_to_jsonld(triples, surveycto_context, base_uri)
  return(jsonld_obj)
}
