
#' plot_rdf_triples_generic
#' @description
#' Create an interactive network diagram from a tibble of RDF triples, treating all objects as nodes, with stabilized layout, optional edge labels, optional ID shortening, complex query filtering, and customizable physics parameters.
#'
#' @param triples_df A tibble with columns `subject`, `predicate`, `object`, and optionally `type`.
#' @param show_edge_labels Logical, whether to display edge labels (default: TRUE).
#' @param stabilize Logical, whether to enable layout stabilization (default: TRUE).
#' @param shorten_ids Logical, whether to shorten node and edge labels (default: TRUE).
#' @param object_types Character vector, filter objects by `type` column (default: NULL, no filtering).
#' @param predicates Character vector, filter by specific predicates (default: NULL, no filtering).
#' @param subjects Character vector, filter by specific subjects (default: NULL, no filtering).
#' @param objects Character vector, filter by specific objects (default: NULL, no filtering).
#' @param gravitationalConstant Numeric, negative force for node repulsion (default: -2000).
#' @param centralGravity Numeric, attraction to graph center (default: 0.3).
#' @param springLength Numeric, ideal edge length (default: 95).
#' @param springConstant Numeric, edge stiffness (default: 0.04).
#' @param damping Numeric, motion damping (default: 0.09).
#' @param avoidOverlap Numeric, node overlap prevention (default: 0.1).
#'
#' @returns A visNetwork object representing the interactive graph.
#'
#' @importFrom dplyr filter, mutate, select, left_join, case_when
#' @importFrom tibble tibble
#' @importFrom stringr str_detect, str_sub, basename
#' @importFrom purrr map_chr
#' @import visNetwork
#'
#' @export
#'
#' @examples
#' triples <- tibble::tibble(
#'   subject = c("id1", "id1", "id1", "id1", "id1", "id2"),
#'   predicate = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "Sepal.Length"),
#'   object = c("5.1", "3.5", "1.4", "0.2", "setosa", "4.9"),
#'   type = c("double", "double", "double", "double", "tag", "double")
#' )
#' p <- plot_rdf_triples_generic(
#'   triples,
#'   show_edge_labels = FALSE,
#'   shorten_ids = TRUE,
#'   object_types = c("double"),
#'   gravitationalConstant = -3000,
#'   springLength = 150
#' )
#' htmlwidgets::saveWidget(p, file = "rdf_graph.html", selfcontained = TRUE)
#' utils::browseURL("rdf_graph.html")

plot_rdf_triples_generic <- function(
  triples_df,
  show_edge_labels = TRUE,
  stabilize = TRUE,
  shorten_ids = TRUE,
  object_types = NULL,
  predicates = NULL,
  subjects = NULL,
  objects = NULL,
  gravitationalConstant = -2000,
  centralGravity = 0.3,
  springLength = 95,
  springConstant = 0.0,
  damping = 0.09,
  avoidOverlap = 0.1
) {
  # --- 3. Validate Input ---
  required_cols <- c("subject", "predicate", "object")
  if (!all(required_cols %in% names(triples_df))) {
    stop("triples_df must contain columns: subject, predicate, object")
  }

  # --- 4. Apply Query Filters ---
  triples_df <- triples_df %>%
    filter(
      if (!is.null(subjects)) subject %in% subjects else TRUE,
      if (!is.null(objects)) object %in% objects else TRUE,
      if (!is.null(predicates)) predicate %in% predicates else TRUE,
      if (!is.null(object_types) && "type" %in% names(triples_df)) type %in% object_types else TRUE
    )

  # --- 5. Prepare Nodes ---
  # All subjects and objects are nodes
  all_nodes <- unique(c(triples_df$subject, triples_df$object))

  # Create node role indicators using joins
  subjects <- tibble(id = unique(triples_df$subject), is_subject = TRUE)
  objects <- tibble(id = unique(triples_df$object), is_object = TRUE)

  # Create nodes dataframe
  nodes <- tibble(id = all_nodes) %>%
    left_join(subjects, by = "id") %>%
    left_join(objects, by = "id") %>%
    mutate(
      is_subject = if_else(is.na(is_subject), FALSE, is_subject),
      is_object = if_else(is.na(is_object), FALSE, is_object),
      group = case_when(
        is_subject & is_object ~ "subject_object",
        is_subject ~ "subject",
        is_object ~ "object",
        TRUE ~ "unknown"
      ),
      # Assign colors based on group
      color = case_when(
        group == "subject" ~ "#FF9999",          # Red for subjects
        group == "object" ~ "#99CCFF",           # Blue for objects
        group == "subject_object" ~ "#FFCC99",   # Orange for nodes that are both
        TRUE ~ "#CCCCCC"                         # Default grey
      ),
      # Assign shapes based on group
      shape = case_when(
        group == "subject" ~ "box",
        group == "object" ~ "circle",
        group == "subject_object" ~ "diamond",
        TRUE ~ "ellipse"
      ),
      # Shorten IDs for labels if requested
      label = case_when(
        shorten_ids ~ map_chr(id, shorten_id),
        TRUE ~ id
      ),
      # Add type info to hover text with full ID
      title = purrr::map_chr(id, function(x) {
        type_info <- if ("type" %in% names(triples_df)) {
          types <- triples_df$type[triples_df$object == x]
          if (length(types) > 0) paste0("<b>Type:</b> ", types[1], "<br>")
          else ""
        } else ""
        paste0("<b>ID:</b> ", x, "<br>", type_info)
      })
    ) %>%
    select(id, label, group, color, shape, title)

  # --- 6. Prepare Edges ---
  edges <- triples_df %>%
    select(from = subject, to = object, predicate) %>%
    mutate(
      arrows = "to", # Directed edges
      color = "#888888", # Default edge color
      font = case_when(
        show_edge_labels ~ list(align = "top"),
        TRUE ~ list(NULL)
      ),
      label = case_when(
        show_edge_labels ~ predicate,
        TRUE ~ NA_character_
      ),
      # Shorten predicate for edge labels if requested and showing labels
      label = case_when(
        show_edge_labels & shorten_ids ~ map_chr(predicate, shorten_id),
        TRUE ~ label
      ),
      title = paste0("<b>Predicate:</b> ", predicate) # Use full predicate for hover text
    ) %>%
    select(-predicate) # Remove predicate column after use

  # --- 7. Create Interactive visNetwork Plot ---
  visNetwork_plot <- visNetwork(nodes, edges, main = "RDF Triples Graph") %>%
    visOptions(
      highlightNearest = TRUE, # Highlight connected nodes on hover
      nodesIdSelection = TRUE, # Dropdown to select nodes
      selectedBy = "group"     # Allow selection by group
    ) %>%
    visPhysics(
      solver = "barnesHut",
      stabilization = stabilize,
      barnesHut = list(
        gravitationalConstant = gravitationalConstant,
        centralGravity = centralGravity,
        springLength = springLength,
        springConstant = springConstant,
        damping = damping,
        avoidOverlap = avoidOverlap
      )
    ) %>%
    visEdges(
      smooth = TRUE,
      color = list(highlight = "#333333")
    ) %>%
    visLayout(randomSeed = 123) %>%
    visInteraction(navigationButtons = TRUE, zoomView = TRUE)

  return(visNetwork_plot)
}

