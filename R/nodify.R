#' Identify RDF nodes from dataframe and provided schema
#' 
#' This function creates node specifications implied by a dataframe according to a specified schema and the Nova ontology.
#' It amends @param data to include columns that are used as the ID (in RDF, the subject) for triples concerning each class. 
#' 
#' @param node_spec [list] Node specification as a list of named lists. There should be an entry for each class of entity implied by the dataframe.
#' Each named list should have the following fields:
#' a) rdf:type for each node according to nova-o
#' b) the variable that can be used as ID for the node.
#' c) if b) is NULL, which combination of columns can be used to create an ID for the node. The specified columns are hashed to create the ID.
#' d) if both b) and c) are null, the constant identifier for this node. For example, the identifier for the single survey of which all responses form part.
#' Exactly one of b), c) or d) must be non-NULL
#' 
#' @param data [data.frame] Data as a tibble
#'
#' @return [list] data with ID columns appended; a tibble containing the node specifications from @param node_spec and the variable name used to identify each node (i.e. the subject in each triple) 
#' @export
#'
#' @examples 
#' interview_spec <- list(
#'   type = "https://nova.org.za/nova-o#Interview",
#'   id_col = "instanceid",
#'   comb_id_col = NULL,
#'   const_id = NULL
#' )
#' 
#' hh_spec <- list(
#'   type = "https://nova.org.za/nova-o#Household",
#'   id_col = NULL,
#'   comb_id_col = c("village", "stand_number_1", "respondent_surname"),
#'   const_id = NULL
#' )
#' 
#' survey_spec <- list(
#'   type = "https://nova.org.za/nova-o#Survey",
#'   id_col = NULL,
#'   comb_id_col = NULL,
#'   const_id = "KiA_adaptation_Q"
#' )
#' 
#' node_spec <- list(interview_spec, hh_spec, hh_addr_spec, survey_spec)
#' result <- identify_nodes(node_spec, small_kia_data)
#' small_kia_data <- result$data
#' id_tb <- result$id_tb
identify_nodes <- function(node_spec, data) {
  # create the specification
  id_tb <- node_spec %>% 
    tibble(node_spec = node_spec) %>%
    unnest_wider(node_spec) %>%
    # create variable ID names
    mutate(var_id_name = paste0(str_extract(type, "(?<=#)[^#]*$"), "_ID"))  %>% 
    select(-.)
  
  # check that exactly one of id_col, comb_id_col and const_id is non-NULL
  if(!all(apply(id_tb[, c("id_col", "comb_id_col", "const_id")], 1, function(row) sum(!is.na(row) & !sapply(row, is.null)) == 1))) {
    stop("Exactly one of id_col, comb_id_col, and const_id should be non-NULL.")
  }
  
  # create ID variables one at a time
  for (vin in id_tb$var_id_name) {
    # get the relevant node specification
    var_info <- id_tb %>% filter(var_id_name == vin)

    # create ID variable:
    # a) if there is an identifying column
    if (!is.na(var_info$id_col)) {
      # get the value of the identifying variable for each row as a vector
      id_val = data %>% pull(var_info$id_col)
      
      # create new ID variable
      data <- data %>%
        mutate(!!vin := !!id_val)
      
    # b) if an ID needs to be created from a combination of other columns
    } else if (!is.null(unlist(var_info$comb_id_col))) {
      id_cols_vector <- as.character(unlist(var_info$comb_id_col))
      
      data <- data %>%
        encrypt_ids(id_cols_vector, var_info$var_id_name)
      
    # c) if a constant ID can be used
    } else {
      const_id <- var_info$const_id
      
      data <- data %>% 
        mutate(!!vin := const_id)
    }
  }
  
  cat("Using the following node specification:\n")
  print(id_tb)

  return(list(data = data, id_tb = id_tb))
}