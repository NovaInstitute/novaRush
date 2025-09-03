#' plot_rdf_triples_interactive
#' @description
#' Take the dataframe of tripples made by map_cto_to_rdf and create a graph.
#'
#' @param triples_df
#' @param context_map
#'
#' @returns
#' @export
#'
#' @examples
#' @examples
#' kia_adapt <- novaCTO::readCTO("KiA_adaptation_ACTIVE")
#' surveycto_context <- make_surveycto_context()
#' formdef <- kia_adapt$fromschema$kia_adaptation
#' fromRDF <- map_cto_to_rdf(formdef, base_uri = "https://novapc.surveycto.com/", instrument = "KiA_adaptation_ACTIVE")
#' p1 <- plot_rdf_triples_interactive(fromRDF, surveycto_context)
#' p1
#' htmlwidgets::saveWidget(p1, file = "~/tmp/rdf_graph.html", selfcontained = TRUE)
#' utils::browseURL("~/tmp/rdf_graph.html")

plot_rdf_triples_interactive <- function(triples_df, context_map) {
  # --- 1. Load Necessary Libraries ---
  # These lines ensure the required packages are available.
  # It's good practice to install them once if you haven't already:
  # install.packages(c("dplyr", "igraph", "visNetwork", "purrr", "tibble"))
  library(dplyr)
  library(igraph)
  library(visNetwork)
  library(purrr)
  library(tibble)
  # --- 3. Prepare Data for `visNetwork` ---

  # Extract node labels (rdfs:label)
  node_labels_df <- triples_df %>%
    filter(predicate == "rdfs:label") %>%
    select(id = subject, label = object) %>%
    distinct()

  # Extract node types (rdf:type)
  node_types_df <- triples_df %>%
    filter(predicate == "rdf:type") %>%
    select(id = subject, type = object) %>%
    distinct() %>%
    # Apply prefixes to types for easier use in coloring/shaping
    mutate(type = purrr::map_chr(type, ~apply_prefixes_for_display(.x, context_map)))

  # Filter out predicates that are better represented as node attributes or are too verbose
  # These are usually not meant to be direct edges in a visual graph for readability.
  graph_edges_df <- triples_df %>%
    filter(!predicate %in% c("rdfs:label", "rdf:type", "survey:hasValue", "survey:isRequired", "survey:hasFieldName", "survey:hasQuestionType", "dcterms:identifier")) %>%
    mutate(
      from = subject,
      to = object,
      # Shorten predicate for edge label
      label = purrr::map_chr(predicate, ~apply_prefixes_for_display(.x, context_map))
    ) %>%
    select(from, to, label)

  # Get all unique nodes from the `from` and `to` columns of the filtered edges
  all_nodes_uris <- unique(c(graph_edges_df$from, graph_edges_df$to))

  # Create nodes dataframe for visNetwork, with labels and types
  nodes <- tibble(id = all_nodes_uris) %>%
    left_join(node_labels_df, by = "id") %>%
    left_join(node_types_df, by = "id") %>%
    mutate(
      # Use rdfs:label if available, otherwise use a shortened URI
      label = ifelse(is.na(label), purrr::map_chr(id, ~apply_prefixes_for_display(.x, context_map)), label),
      # Assign colors based on type - now more dynamic with more cases and a default
      color = case_when(
        type == "survey:Survey" ~ "#00cc99",          # Teal for survey root
        type == "survey:Question" ~ "#00b2e3",        # Light blue for questions
        type == "skos:Collection" ~ "#ffae42",        # Orange for collections
        type == "survey:ClosedAnswer" ~ "#ff7f50",    # Coral for closed answers
        type == "prov:startTime" ~ "#a0d9b4",         # Light green for start time
        type == "xsd:dateTime" ~ "#a0d9b4",           # Light green for datetime type
        type == "prov:Activity" ~ "#a080ff",          # Purple for activities
        type == "foaf:Agent" ~ "#b3b3e6",             # Light purple for agents
        TRUE ~ "#cccccc"                             # Default grey for other types
      ),
      # Assign shapes based on type
      shape = case_when(
        type == "survey:Survey" ~ "box",
        type == "survey:Question" ~ "square",
        type == "skos:Collection" ~ "triangle",
        type == "survey:ClosedAnswer" ~ "dot",
        type == "prov:startTime" ~ "diamond",
        type == "xsd:dateTime" | type == "prov:Activity" | type == "foaf:Agent" ~ "ellipse",
        TRUE ~ "ellipse"
      ),
      # Add title for hover text
      title = paste0("<b>URI:</b> ", id, "<br><b>Type:</b> ", type, "<br><b>Label:</b> ", label)
    )

  # Create edges dataframe for visNetwork
  edges <- graph_edges_df %>%
    rename(from = from, to = to) %>%
    mutate(
      arrows = "to", # All edges are directed
      color = "#888888", # Default edge color (single hex code)
      font = list(align = "top"), # Position edge labels
      title = paste0("<b>Predicate:</b> ", label) # Add title for hover text on edges
    )

  # --- 4. Create and Return the Interactive visNetwork Plot ---
  visNetwork_plot <- visNetwork(nodes, edges, main = "RDF Graph from SurveyCTO Data") %>%
    visOptions(
      highlightNearest = TRUE, # Highlight connected nodes on hover
      nodesIdSelection = TRUE, # Add a dropdown to select nodes by ID
      selectedBy = "type"      # Allow selection by node type
    ) %>%
    visPhysics(solver = "barnesHut", stabilization = FALSE) %>% # Choose a physics solver for layout
    visEdges(
      smooth = TRUE,
      color = list(highlight = "#333333") # Edge highlight color now set here
    ) %>%
    visLayout(randomSeed = 123) %>% # For reproducible layout
    visInteraction(navigationButtons = TRUE, zoomView = TRUE) # Add navigation buttons and enable zoom

  return(visNetwork_plot)
}


# --- 2. Helper Function to Apply Prefixes and Clean URIs ---
# This function shortens full URIs to more readable prefixed names or base names.
apply_prefixes_for_display <- function(uri, context_map) {
  for (i in 1:nrow(context_map)) {
    prefix <- context_map$prefix[i]
    namespace <- context_map$namespace[i]
    if (startsWith(uri, namespace)) {
      return(paste0(prefix, ":", substr(uri, nchar(namespace) + 1, nchar(uri))))
    }
  }
  # For SurveyCTO specific URIs, get the last part after the last slash
  if (startsWith(uri, "https://novapc.surveycto.com/")) {
    return(basename(uri))
  }
  return(uri) # Return as-is if no specific prefixing logic applies
}

