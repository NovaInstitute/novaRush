#' pivot_longer_with_type
#'
#' @param data A data frame to pivot longer. T
#' @param names_to Character. The name of the column to store the variable names in. Default is "name".
#' @param tripplenames Logical. If TRUE, the resulting column names will be "subject", "predicate", "object", and "type". If FALSE, the column names will be "ID", "name", "value", and "type".
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples

pivot_longer_with_type <- function(data, names_to = "name", tripplenames = TRUE, ...) {

  # Get original data types
  types <- sapply(data, class)
  dftypes <- dplyr::tibble(name = names(types), type = types)
  names(dftypes)[1] <- names_to

  # Create ID as hash of content
  data %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(ID = digest::digest(pick(everything()), algo = "md5")) %>%
    dplyr::select(ID, everything() )

  # Make all character
  data <- dplyr::mutate(data, across(everything(), ~as.character(.)))

  # Pivot longer
  res <- tidyr::pivot_longer(data, cols = -c(ID), names_to = names_to, ...) %>%
    dplyr::left_join(dftypes)
  if (tripplenames) {names(res) <- c("subject", "predicate", "object", "type")}
  res %>% dplyr::mutate(type = dplyr::case_when(
    type == "character" ~ "string",
    type ==  "numeric" ~ "double",
    type == "logical" ~ "boolean",
    type == "factor" ~ "tag",
    type ==  "Date" ~ "instant",
    type == "POSIXct" ~ "instant",
    type == "POSIXt" ~ "instant",
    type ==  "integer" ~ "int",
    type ==  "list" ~ "json",
    TRUE ~ NA_character_))

}
