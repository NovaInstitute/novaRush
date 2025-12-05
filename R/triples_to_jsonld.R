
#' triples_to_jsonld
#' @description
#' Function to convert RDF triples to JSON-LD
#' @param triples data.frame containing RDF triples with columns `subject`, `predicate`, `object`, and `object_type`.
#' resulting from the `map_cto_to_rdf` function.
#' @param context_df  data.frame containing the context for JSON-LD conversion. Should have columns `prefix` and `namespace`.
#' @param base_uri string representing the base URI for the JSON-LD document. Default is "https://example.org/survey/".
#'
#' @returns
#' @export
#'
#' @examples
#' srv <- "KiA_adaptation_ACTIVE"
#' kia_adapt <- novaCTO::readCTO(srv)
#' formdef <- kia_adapt$fromschema$kia_adaptation
#' fromRDF <- map_cto_to_rdf(formdef, base_uri = glue::glue("https://novapc.surveycto.com/{srv}/"), instrument = "KiA_adaptation_ACTIVE")
#' js <- triples_to_jsonld(fromRDF, make_surveycto_centext())
#'

triples_to_jsonld <- function(triples,
                              context_df = NULL,
                              base_uri = "https://example.org/survey/") {

  if (is.null(context_df)) {
    context_df <- make_surveycto_centext()
  }

  # Create context object from context_df
  context <- as.list(setNames(context_df$namespace, context_df$prefix))

  # Add base URI and additional context items
  context[["@base"]] <- base_uri
  context[["@vocab"]] <- "https://w3id.org/survey-ontology#"

  # Group triples by subject to create JSON-LD objects
  grouped_triples <- triples %>%
    group_by(subject) %>%
    summarise(
      properties = list(tibble(predicate = predicate, object = object, object_type = object_type)),
      .groups = "drop"
    )

  jsonld_objects <- purrr::map2(grouped_triples$properties, grouped_triples$subject, ~properties2kv(dfprops = ..1, id = ..2))

  # Create final JSON-LD document
  jsonld_doc <- list(
    "@context" = context,
    "@graph" = jsonld_objects
  )

  return(jsonld_doc)
}

# ___________________________________________________________
# Helper functions

# Function to export JSON-LD as pretty JSON string
export_jsonld <- function(jsonld_obj) {
  toJSON(jsonld_obj, pretty = TRUE, auto_unbox = TRUE)
}

# Enhanced demo function with JSON-LD output
demo_mapping <- function(formdef) {
  cat("Mapping SurveyCTO form definition to RDF triples...\n\n")

  # Generate triples
  triples <- map_cto_to_rdf(formdef)

  cat("Generated", nrow(triples), "RDF triples\n\n")

  # Show sample triples
  cat("Sample triples:\n")
  print(head(triples, 10))

  cat("\n\nGenerating JSON-LD...\n")

  surveycto_context <- make_surveycto_centext()

  # Convert to JSON-LD
  jsonld_obj <- triples_to_jsonld(triples, surveycto_context)
  jsonld_string <- export_jsonld(jsonld_obj)

  cat("\nJSON-LD output (first 1000 characters):\n")
  cat(substr(jsonld_string, 1, 1000))
  if (nchar(jsonld_string) > 1000) {
    cat("\n... (truncated)")
  }

  surveycto_context <- make_surveycto_centext()

  cat("\n\nTurtle format output:\n")
  turtle_output <- export_turtle(triples, surveycto_context)

  # Show first few lines of Turtle output
  turtle_lines <- strsplit(turtle_output, "\n")[[1]]
  cat(paste(head(turtle_lines, 15), collapse = "\n"))
  cat("\n... (truncated)")

  return(list(
    triples = triples,
    turtle = turtle_output,
    jsonld_obj = jsonld_obj,
    jsonld_string = jsonld_string
  ))
}


