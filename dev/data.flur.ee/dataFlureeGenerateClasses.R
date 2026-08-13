#' generate_f_classes
#'  Generate f:classes from an ontology list.
#'  Typically not used alone. Used inside dataFlureeOnt2Model()
#' @param ontology_list A list of ontology entities (as parsed from JSON-LD).
#' Typically from function getOntology()
#' @param include_properties Logical whether to include properties in the output. Defaults to TRUE.
#'
#' @returns A list of f:classes suitable for inclusion in an f:DataModel.
#' @export
#'
#' @examples
#' ont <- getOntology(ontology_url = "https://datadudes.xyz/infocomm")
#' f_classes <- generate_f_classes(ont, include_properties = TRUE)

generate_f_classes <- function(ontology_list, include_properties = TRUE,
                               classes = c("http://www.w3.org/2002/07/owl#Class", "rdfs:Class" )) {
  # Validate input
  if (!is.list(ontology_list) || !all(sapply(ontology_list, is.list))) {
    stop("Input must be a list of ontology entities.")
  }

  # Extract classes (owl:Class)
  classes <- lapply(ontology_list, function(entity) {
    if (!is.null(entity$`@type`) && entity$`@type`[[1]] %in% classes) {
      list(`@id` = entity$`@id`)
    } else {
      NULL
    }
  })
  classes <- classes[!sapply(classes, is.null)]
  if (length(classes) == 0) {
    stop("No classes found in the ontology list.")
  }

  # Add metadata and properties
  f_classes <- lapply(classes, function(class_entry) {
    class_id <- class_entry$`@id`
    entity <- ontology_list[sapply(ontology_list, function(e) e$`@id` == class_id)][[1]]

    # Add rdfs:label, rdfs:comment, skos:definition if present
    if (!is.null(entity$`http://www.w3.org/2000/01/rdf-schema#label`)) {
      class_entry$`rdfs:label` <- entity$`http://www.w3.org/2000/01/rdf-schema#label`[[1]]$`@value`
    }
    if (!is.null(entity$`http://www.w3.org/2000/01/rdf-schema#comment`)) {
      class_entry$`rdfs:comment` <- entity$`http://www.w3.org/2000/01/rdf-schema#comment`[[1]]$`@value`
    }
    if (!is.null(entity$`http://www.w3.org/2004/02/skos/core#definition`)) {
      class_entry$`skos:definition` <- entity$`http://www.w3.org/2004/02/skos/core#definition`[[1]]$`@value`
    }

    # Add rdfs:subClassOf if present
    if (!is.null(entity$`http://www.w3.org/2000/01/rdf-schema#subClassOf`)) {
      class_entry$`rdfs:subClassOf` <- lapply(
        entity$`http://www.w3.org/2000/01/rdf-schema#subClassOf`,
        function(sc) list(`@id` = sc$`@id`)
      )
    }

    # Add properties if include_properties is TRUE
    if (include_properties) {
      # Find properties where this class is in the domain
      properties <- lapply(ontology_list, function(prop) {
        if (!is.null(prop$`@type`) && prop$`@type`[[1]] == "http://www.w3.org/2002/07/owl#ObjectProperty" &&
            !is.null(prop$`http://www.w3.org/2000/01/rdf-schema#domain`) &&
            any(sapply(prop$`http://www.w3.org/2000/01/rdf-schema#domain`, function(d) d$`@id` == class_id))) {
          prop_entry <- list(`@id` = prop$`@id`)
          # Add rdfs:range if present
          if (!is.null(prop$`http://www.w3.org/2000/01/rdf-schema#range`)) {
            prop_entry$`rdfs:range` <- list(`@id` = prop$`http://www.w3.org/2000/01/rdf-schema#range`[[1]]$`@id`)
          }
          # Add skos:prefLabel from local name
          prop_entry$`skos:prefLabel` <- sub(".*#", "", prop$`@id`)
          return(prop_entry)
        } else {
          NULL
        }
      })
      properties <- properties[!sapply(properties, is.null)]
      if (length(properties) > 0) {
        class_entry$`f:properties` <- properties
      }
    }

    return(class_entry)
  })

  return(f_classes)
}

