# Function to map CTO form definition to RDF triples
#' @Title map_cto_to_rdf
#'
#' @param formdef
#' @param base_uri
#'
#' @returns
#' @export
#' @import dplyr
#' @import purrr
#' @import tibble
#' @examples
#' kia_adapt <- novaCTO::readCTO("KiA_adaptation_ACTIVE")
#' formdef <- kia_adapt$fromschema$kia_adaptation
#' fromRDF <- map_cto_to_rdf(formdef, base_uri = "https://novapc.surveycto.com/", instrument = "KiA_adaptation_ACTIVE")
#' js <- triples_to_jsonld(fromRDF, surveycto_context)

map_cto_to_rdf <- function(formdef,
                           base_uri = "https://novapc.surveycto.com/",
                           instrument = NULL) {

  if (is.null(instrument)) {
    stop("Instrument name must be provided.")
  }

  # Initialize triples dataframe
  triples <- tibble::tibble(
    subject = character(),
    predicate = character(),
    object = character(),
    object_type = character()
  )

  # Create survey instrument URI
  survey_uri <- paste0(base_uri, instrument)

  # Add survey-level triples
  survey_triples <- tibble::tribble(
    ~subject, ~predicate, ~object, ~object_type,
    survey_uri, "rdf:type", "survey:Survey", "uri",
    survey_uri, "rdfs:label", "SurveyCTO Form", "literal"
  )

  triples <- dplyr::bind_rows(triples, survey_triples)

  # Process each field in the form definition
  for (i in 1:nrow(formdef)) {
    field <- formdef[i, ]
    #cat("\n", field[[1]], "\n")

    # Create question URI
    question_uri <- paste0(base_uri, "question/", create_uri_safe(field$name))

    # Map CTO type to semantic type
    semantic_type <- cto_semantic_mapping %>%
      filter(CTO_label == field$type) %>%
      pull(Semantic_label)

    if (length(semantic_type) == 0) {
      semantic_type <- "survey:Question"  # Default fallback
    }

    # Determine rdf:type based on caption presence and semantic mapping
    field_rdf_type <- "survey:Question" # Default to survey:Question
    if (is.na(field$caption) || field$caption == "") {
      # If caption is missing, check for specific semantic mapping for metadata/administrative fields
      if (field$name == "deviceid") {
        field_rdf_type <- "prov:Entity"
      } else if (field$name == "starttime") {
        field_rdf_type <- "prov:startedAtTime"
      } else if (field$name == "endtime") {
        field_rdf_type <- "prov:endedAtTime" # Assuming an 'end' type exists or is implied
      } else if (field$name == "today") {
        field_rdf_type <- "xsd:date"
      } else if (field$name == "username") {
        field_rdf_type <- "foaf:accountName"
      } else if (field$name %in% "instanceid|instanceID") { # Assuming 'instanceid' is a possible type
        field_rdf_type <- "dcterms:identifier"
      } else if (field$name %in% "phonenumber|devicephonenum") {
        field_rdf_type <- "vcard:hasPhone"
      }
      else {
        # Fallback for other metadata fields not explicitly mapped above
        field_rdf_type <- "owl:Thing" # Or a more general provenance concept
      }
    }

    # Basic question triples
    question_triples <- tibble::tribble(
      ~subject, ~predicate, ~object, ~object_type,
      survey_uri, "survey:hasQuestion", question_uri, "uri",
      question_uri, "rdf:type", field_rdf_type, "uri",
      question_uri, "survey:hasQuestionType", semantic_type, "uri",
      question_uri, "survey:hasFieldName", field$name, "literal",
      question_uri, "dcterms:identifier", field$name, "literal"
    )

    # Add question label if caption exists
    if (!is.na(field$caption) && field$caption != "") {
      question_triples <- bind_rows(
        question_triples,
        tibble(
          subject = question_uri,
          predicate = "rdfs:label",
          object = field$caption,
          object_type = "literal"
        )
      )
    }

    # Add group information if exists
    if (!is.na(field$group) && field$group != "") {
      group_uri <- paste0(base_uri, "group/", create_uri_safe(field$group))

      group_triples <- tibble::tribble(
        ~subject, ~predicate, ~object, ~object_type,
        group_uri, "rdf:type", "survey:QuestionGroup", "uri",
        group_uri, "rdfs:label", field$group, "literal",
        group_uri, "survey:contains", question_uri, "uri",
        question_uri, "survey:belongsToGroup", group_uri, "uri"
      )

      question_triples <- bind_rows(question_triples, group_triples)
    }

    # Add required property
    if (!is.na(field$required) && field$required) {
      question_triples <- bind_rows(
        question_triples,
        tibble(
          subject = question_uri,
          predicate = "survey:isRequired",
          object = "true",
          object_type = "literal"
        )
      )
    }

    # Add choices for select questions
    if (field$type %in% c("select_one", "select_multiple") &&
        !is.null(field$choices[[1]]) &&
        nrow(field$choices[[1]]) > 0) {

      choices_df <- field$choices[[1]]

      # Create a URI for the skos:Collection of options for this question
      options_collection_uri <- paste0(question_uri, "/options")

      # Add triples to link the question to its collection of options
      # and define the collection itself
      collection_triples <- tibble::tribble(
        ~subject, ~predicate, ~object, ~object_type,
        question_uri, "survey:hasAvailableOptions", options_collection_uri, "uri", # New property to link question to its options collection
        options_collection_uri, "rdf:type", "skos:Collection", "uri",
        options_collection_uri, "rdfs:label", paste0("Options for '", field$name, "' question"), "literal"
      )
      question_triples <- bind_rows(question_triples, collection_triples)


      # Process each individual choice and add it as a member of the skos:Collection
      for (j in 1:nrow(choices_df)) {
        choice_uri <- paste0(question_uri, "/choice/", create_uri_safe(choices_df$choice[j]))

        choice_triples <- tibble::tribble(
          ~subject, ~predicate, ~object, ~object_type,
          choice_uri, "rdf:type", "survey:ClosedAnswer", "uri", # Changed from survey:ResponseOption to existing survey:ClosedAnswer
          choice_uri, "survey:hasValue", choices_df$choice[j], "literal", # Keep existing survey:hasValue
          choice_uri, "rdfs:label", choices_df$choice_label[j], "literal",
          options_collection_uri, "skos:member", choice_uri, "uri" # Link the individual choice to the collection
        )

        question_triples <- bind_rows(question_triples, choice_triples)
      }
    }

    # Add constraint information
    if (!is.na(field$constraint) && field$constraint != "") {
      question_triples <- bind_rows(
        question_triples,
        tibble(
          subject = question_uri,
          predicate = "survey:hasConstraint",
          object = field$constraint,
          object_type = "literal"
        )
      )
    }

    # Add repeat group information
    if (!is.na(field$repeatGroupCount) && field$repeatGroupCount > 0) {
      question_triples <- bind_rows(
        question_triples,
        tibble(
          subject = question_uri,
          predicate = "survey:isRepeatable",
          object = "true",
          object_type = "literal"
        )
      )
    }

    # Add metadata flag
    if (!is.na(field$metadataField) && field$metadataField) {
      question_triples <- bind_rows(
        question_triples,
        tibble(
          subject = question_uri,
          predicate = "survey:isMetadata",
          object = "true",
          object_type = "literal"
        )
      )
    }

    triples <- bind_rows(triples, question_triples)
  }

  return(triples)
}

# Function to export triples to Turtle format
#' export_turtle
#' @description
#' Takes the output of `map_cto_to_rdf()` and converts it to Turtle format.
#'
#' @param triples
#' @param context_df
#'
#' @returns
#' @export
#'
#' @examples
#' fromRDF <- map_cto_to_rdf(formdef, base_uri = "https://novapc.surveycto.com/", instrument = "KiA_adaptation_ACTIVE")
#' export_turtle(fromRDF, context_df = make_surveycto_centext())

export_turtle <- function(triples, context_df = make_surveycto_centext()) {

  # Create prefix declarations
  prefixes <- context_df %>%
    dplyr::mutate(prefix_line = paste0("@prefix ", prefix, ": <", namespace, "> .")) %>%
    dplyr::pull(prefix_line)

  # Add common prefixes not in context
  additional_prefixes <- c(
    "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
    "@prefix owl: <http://www.w3.org/2002/07/owl#> ."
  )

  all_prefixes <- c(prefixes, additional_prefixes)

  # Convert triples to Turtle format
  turtle_triples <- triples %>%
    mutate(
      object_formatted = case_when(
        object_type == "uri" ~ paste0("<", object, ">"),
        object_type == "literal" ~ paste0('"', gsub('"', '\\"', object), '"'),
        TRUE ~ object
      ),
      triple_line = paste0("<", subject, "> <", predicate, "> ", object_formatted, " .")
    ) %>%
    pull(triple_line)

  # Combine prefixes and triples
  turtle_output <- c(
    all_prefixes,
    "",
    turtle_triples
  )

  return(paste(turtle_output, collapse = "\n"))
}

# Function to create URI-safe identifiers
#' @title
#' create_uri_safe
#' @description
#' Function to create URI-safe identifiers from text strings.
#' @param text
#'
#' @returns
#' @export
#'
#' @examples
#' create_uri_safe("Example Text with Spaces & Special Characters!")

create_uri_safe <- function(text) {
  text %>%
    tolower() %>%
    gsub("[^a-z0-9_]", "_", .) %>%
    gsub("_{2,}", "_", .) %>%
    gsub("^_|_$", "", .)
}
