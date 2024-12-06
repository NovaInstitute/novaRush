#' Identify RDF nodes from dataframe and provided schema
#' 
#' This function creates node specifications implied by a dataframe according to a specified schema and the Nova ontology.
#' It amends @param data to include columns that are used as the ID (in RDF, the subject) for triples concerning each class. 
#' 
#' @param [list] node_spec Node specification as a list of named lists. There should be an entry for each class of entity implied by the dataframe.
#' Each named list should have the following fields:
#' a) rdf:type for each node according to nova-o
#' b) the variable that can be used as ID for the node.
#' c) if b) is NULL, which combination of columns can be used to create an ID for the node. The specified columns are encrypted in a reversible way to create the ID.
#' A NULL value for both b) and c) simultaneously implies that the node is a blank node that can be assigned a random ID later in the data pipeline.
#' @param data [data.frame] Data as a tibble
#'
#' @return [list] data with ID columns appended; a tibble containing the node specifications from @param node_spec and the variable name used to identify each node (i.e. the subject in each triple) 
#' @export
#'
#' @examples 
#' survey_spec <- list(
#' type = "https://nova.org.za/nova-o#Survey",
#' id_col = "instanceid",
#' comb_id_col = NULL
#' )
#' 
#' hh_spec <- list(
#'   type = "https://nova.org.za/nova-o#Household",
#'   id_col = NULL,
#'   comb_id_col = c("village", "stand_number_1", "respondent_surname")
#' )
#' 
#' hh_addr_spec <- list(
#'   type = "https://nova.org.za/nova-o#HouseholdAddress",
#'   id_col = NULL,
#'   comb_id_col = NULL
#' )
#'
#' node_spec <- list(survey_spec, hh_spec, hh_addr_spec)
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
  
  print("Using the following node specification:")
  print(id_tb)
  
  # create ID variables one at a time
  for (vin in id_tb$var_id_name) {
    # get the relevant node specification
    var_info <- id_tb %>% filter(var_id_name == vin)

    # create ID variable:
    # a) if there is an identifying column
    if (!is.na(var_info$id_col)) {
      # get the value of the identifying variable for each row as a vector
      id_val = data %>% pull(var_info$id_col)
      data <- data %>%
        mutate(!!vin := !!id_val)
    # b) if an ID needs to be created from a combination of other columns
    } else if (!is.null(unlist(var_info$comb_id_col))) {
      id_cols_vector <- as.character(unlist(var_info$comb_id_col))
      data <- data %>%
        encrypt_ids(id_cols_vector, var_info$var_id_name)
    # c) if an ID needs to generated randomly (i.e. blank node)
    } else {
      data <- data %>% 
        rowwise() %>% 
        mutate(!!vin := digest::digest(pick(everything()), algo = "md5"))
    }
  }

  return(list(data = data, id_tb = id_tb))
}

#' Create a node ID by encrypting identifying variables 
#' 
#' This function courtesy of ChatGPT. It is used in `identify_nodes`.
#'
#' @param data [data.frame]
#' @param id_cols [character] Names of identifying columns
#' @param id_col_name [string] What to call the resulting ID column
#'
#' @return data with the ID column appended
#'
#' @examples
encrypt_ids <- function(data, id_cols, id_col_name) {
  # TODO functionality to use encryption keys
  # # Retrieve the encryption key from environment variables
  # encryption_key <- Sys.getenv(key_env_var)
  # 
  # # Check if the key is available
  # if (encryption_key == "" | is.null(encryption_key)) {
  #   stop("Encryption key not found in the environment. Set it using Sys.setenv().")
  # }
  
  # TODO remove this when functionality above added
  encryption_key = "default"
  
  # Ensure the key is a raw vector
  key <- sha256(charToRaw(encryption_key))
  
  # Encrypt a row by serializing its contents
  encrypt_row <- function(row, key) {
    serialized_row <- paste(as.character(row), collapse = ",") # Combine row into a string
    serialized_row_raw <- charToRaw(serialized_row)
    base64_encode(aes_cbc_encrypt(serialized_row_raw, key))
  }
  
  # Apply encryption to each row and add the ID column
  data_with_id <- data %>%
    rowwise() %>%
    mutate(!!id_col_name := encrypt_row(across(all_of(id_cols)), key)) %>%
    ungroup()
  
  return(data_with_id)
}

# Helper function to decrypt encrypted node IDs
# This function courtesy of ChatGPT. Not surprisingly, it currently DOES NOT WORK. Don't use it.
decrypt_id <- function(encrypted_id) {
  # TODO functionality to use encryption keys
  # # Retrieve the encryption key from environment variables
  # encryption_key <- Sys.getenv(key_env_var)
  # 
  # # Check if the key is available
  # if (encryption_key == "") {
  #   stop("Encryption key not found in the environment. Set it using Sys.setenv().")
  # }
  
  # TODO remove this when incorporating functionality above
  encryption_key = "default"
  
  # Ensure the key is a raw vector
  key <- sha256(charToRaw(encryption_key))
  
  # Decrypt the encrypted text back into the serialized string
  raw_decrypted <- aes_cbc_decrypt(base64_decode(encrypted_id), key)
  dec_string <- rawToChar(raw_decrypted)
  
  print(paste("The decrypted string is:", dec_string))
}
