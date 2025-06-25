# Function to convert RDF triples to JSON-LD
#' triples_to_jsonld
#'
#' @param triples data.frame containing RDF triples with columns `subject`, `predicate`, `object`, and `object_type`.
#' resulting from the `map_cto_to_rdf` function.
#' @param context_df  data.frame containing the context for JSON-LD conversion. Should have columns `prefix` and `namespace`.
#' @param base_uri string representing the base URI for the JSON-LD document. Default is "https://example.org/survey/".
#'
#' @returns
#' @export
#'
#' @examples
triples_to_jsonld <- function(triples, context_df = make_surveycto_centext(), base_uri = "https://example.org/survey/") {

  library(jsonlite)

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

  # Function to convert predicate to short form
  shorten_predicate <- function(pred) {
    for (i in 1:nrow(context_df)) {
      full_ns <- context_df$namespace[i]
      prefix <- context_df$prefix[i]
      if (startsWith(pred, full_ns)) {
        return(paste0(prefix, ":", gsub(full_ns, "", pred)))
      }
    }

    # Handle common RDF predicates
    if (pred == "rdf:type" || pred == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") {
      return("@type")
    }
    if (startsWith(pred, "http://www.w3.org/1999/02/22-rdf-syntax-ns#")) {
      return(paste0("rdf:", gsub("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "", pred)))
    }

    return(pred)
  }

  # Function to convert object based on type
  format_object <- function(obj, obj_type) {
    if (obj_type == "uri") {
      # Try to shorten URI using context
      for (i in 1:nrow(context_df)) {
        full_ns <- context_df$namespace[i]
        prefix <- context_df$prefix[i]
        if (startsWith(obj, full_ns)) {
          return(paste0(prefix, ":", gsub(full_ns, "", obj)))
        }
      }
      # If not shortened, return as @id
      return(list("@id" = obj))
    } else {
      return(obj)
    }
  }

  # Convert each subject to JSON-LD object
  jsonld_objects <- purrr::map(grouped_triples$properties, function(props) {
    obj <- list()

    for (i in 1:nrow(props)) {
      pred <- shorten_predicate(props$predicate[i])
      value <- format_object(props$object[i], props$object_type[i])

      # Handle @type specially
      if (pred == "@type") {
        if (is.list(value) && !is.null(value[["@id"]])) {
          value <- value[["@id"]]
        }
        # Try to shorten type URIs
        for (j in 1:nrow(context_df)) {
          full_ns <- context_df$namespace[j]
          prefix <- context_df$prefix[j]
          if (startsWith(value, full_ns)) {
            value <- paste0(prefix, ":", gsub(full_ns, "", value))
            break
          }
        }
      }

      # Add to object, handling multiple values
      if (pred %in% names(obj)) {
        if (!is.list(obj[[pred]]) || is.null(names(obj[[pred]]))) {
          obj[[pred]] <- list(obj[[pred]], value)
        } else {
          obj[[pred]] <- append(obj[[pred]], list(value))
        }
      } else {
        obj[[pred]] <- value
      }
    }

    return(obj)
  })

  # Add @id to each object
  for (i in 1:length(jsonld_objects)) {
    jsonld_objects[[i]][["@id"]] <- grouped_triples$subject[i]
  }

  # Create final JSON-LD document
  jsonld_doc <- list(
    "@context" = context,
    "@graph" = jsonld_objects
  )

  return(jsonld_doc)
}

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

  # Convert to JSON-LD
  jsonld_obj <- triples_to_jsonld(triples, surveycto_context)
  jsonld_string <- export_jsonld(jsonld_obj)

  cat("\nJSON-LD output (first 1000 characters):\n")
  cat(substr(jsonld_string, 1, 1000))
  if (nchar(jsonld_string) > 1000) {
    cat("\n... (truncated)")
  }

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

# Standalone function to convert triples to JSON-LD
cto_to_jsonld <- function(formdef, base_uri = "https://example.org/survey/", instrument = "SurveyCTO_Form") {
  triples <- map_cto_to_rdf(formdef, base_uri, instrument )
  jsonld_obj <- triples_to_jsonld(triples, surveycto_context, base_uri)
  return(jsonld_obj)
}
