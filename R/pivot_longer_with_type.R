#' pivot_longer_with_type
#'
#' @param data data.frame
#' @param ... Passed to pivot_longer
#' @param names_to Character. What you want to call the attribute column. Default "name"
#' @param tripplenames Logical. Default TRUE. If TRUE, name columns subject, predicate, object and type
#'
#' @return tibble with four columns:"subject", "predicate", "object", "type"
#' @export
#' @importFrom magrittr %<>%
#' @import dplyr
#' @examples
#' df1 = pivot_longer_with_type(iris)
#' df1 %>% select(predicate, type) %>% distinct()

pivot_longer_with_type <- function(data, names_to = "name", tripplenames = TRUE, ...) {

# Get original data types
types <- sapply(data, class)
dftypes <- tibble(name = names(types), type = types)
names(dftypes)[1] <- names_to

# Create ID as hash of content
data %<>%
  rowwise() %>%
  mutate(ID = digest::digest(pick(everything()), algo = "md5")) %>%
  select(ID, everything() )

# Make all character
data <- mutate(data, across(everything(), ~as.character(.)))

# Pivot longer
res <- pivot_longer(data, cols = -c(ID), names_to = names_to, ...) %>%
  left_join(dftypes)
if (tripplenames) {names(res) <- c("subject", "predicate", "object", "type")}
res %>% dplyr::mutate(type = case_when(
  type == "character" ~ "string",
  type ==  "numeric" ~ "double",
  type == "logical" ~ "boolean",
  type == "factor" ~ "string",
  type ==  "Date" ~ "date",
  type == "POSIXct" ~ "date",
  type == "POSIXt" ~ "date",
  type ==  "integer" ~ "integer",
  type ==  "list" ~ "array",
  TRUE ~ "string"))

}

pivot_wider_by_type <- function(res, subject = "subject", predicate = "predicate", object = "object") {

  dftypes <- res %>% select(predicate, type) %>%
    distinct() %>%
    dplyr::mutate(type = case_when(
    type == "string" ~ "character",
    type == "double" ~ "numeric",
    type == "boolean" ~ "logical",
    type == "date" ~ "Date",
    type == "date" ~ "POSIXct",
    type == "integer" ~ "integer",
    type ==  "array" ~ "list",
    TRUE ~ "string"))

  res %>% distinct() %>% select(-type) %>%
    pivot_wider(id_cols = all_of(subject), names_from = predicate, values_from = object) %>%
    mutate(across(all_of(dftypes$predicate),
                  ~ map2(.x, dftypes$type[dftypes$predicate == cur_column()], function(col, type) {
                    switch(type,
                           "character" = as.character(col),
                           "integer" = as.integer(col),
                           "numeric" = as.numeric(col),
                           "double" = as.double(col),
                           col) # Return as is if type doesn't match any cases
                  }) %>% unlist()))
}
