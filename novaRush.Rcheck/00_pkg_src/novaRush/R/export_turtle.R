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
