
#' dataFlureeOnt2Model
#' @description Generate full f:DataModel from ontology list downloaded with getOntology()
#' @param ontology_list List of ontologies as returned by getOntology()
#' @param model_id Character string for the model ID
#' @param label Character string for the model label
#' @param comment Character string for the model comment
#' @param prefLabel Character string for the model preferred label
#' @param include_properties Logical, whether to include properties in the model (default TRUE)
#' @param context List defining the JSON-LD context (default includes common prefixes)
#' Default context includes: f, claimont, rdfs, skos, owl
#'
#' @returns A list representing the f:DataModel suited to use as insert item in a Fluree transaction
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

dataFlureeOnt2Model <- function(ontology_list,
                                model_id = "ex:MyDataModel",
                                label = "Default Data Model Label",
                                comment = "Default comment for the data model",
                                prefLabel = "Default Preferred Label",
                                include_properties = TRUE,
                                context = list(
                                  f = "https://ns.flur.ee/ledger#",
                                  claimont = "https://w3id.org/claimont#",
                                  rdfs = "http://www.w3.org/2000/01/rdf-schema#",
                                  skos = "http://www.w3.org/2004/02/skos/core#",
                                  owl = "http://www.w3.org/2002/07/owl#"
                                )) {
  # Generate f:classes
  f_classes <- generate_f_classes(ontology_list, include_properties)

  # Construct f:DataModel
  data_model <- list(
    `@id` = model_id,
    `@type` = "f:DataModel",
    `rdfs:label` = label,
    `rdfs:comment` = comment,
    `skos:prefLabel` = prefLabel,
    `f:classes` = f_classes
  )

  return(list(data_model))
}
