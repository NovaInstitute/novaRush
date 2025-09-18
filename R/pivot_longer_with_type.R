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
#' @import tibble
#' @examples
#' df1 = pivot_longer_with_type(iris)
#' df1 %>% dplyr::select(predicate, type) %>% dplyr::distinct()

pivot_longer_with_type <- function(data, names_to = "name", tripplenames = TRUE, ...) {

# Get original data types
types <- sapply(data, class)
dftypes <- dplyr::tibble(name = names(types), type = types) %>% mutate(type = purrr::map_chr(type, ~.[[1]]))
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

#' pivot_wider_by_type
#'
#' @param res data.frame
#' @param subject Character. Name of column in res with subject. Default "subject"
#' @param predicate Character. Name of column in res with predicate. Default "predicate"
#' @param object Character. Name of column in res with object Default "object"
#'
#' @returns
#' @export
#'
#' @examples

#' df1 = pivot_longer_with_type(iris)
#' df1 %>% dplyr::select(predicate, type) %>% dplyr::distinct()
#' df2 = pivot_wider_by_type(df1)

pivot_wider_by_type <- function(res, subject = "subject", predicate = "predicate", object = "object") {

  dftypes <- res %>% dplyr::select(predicate, type) %>%
    dplyr::distinct() %>%
    dplyr::mutate(type = dplyr::case_when(
    type == "string" ~ "character",
    type == "double" ~ "numeric",
    type == "boolean" ~ "logical",
    type == "date" ~ "Date",
    type == "date" ~ "POSIXct",
    type == "integer" ~ "integer",
    type ==  "array" ~ "list",
    TRUE ~ "string"))

  res %>% dplyr::distinct() %>% dplyr::select(-type) %>%
    tidyr::pivot_wider(id_cols = all_of(subject), names_from = predicate, values_from = object) %>%
    dplyr::mutate(across(all_of(dftypes$predicate),
                  ~ purrr::map2(.x, dftypes$type[dftypes$predicate == dplyr::cur_column()], function(col, type) {
                    switch(type,
                           "character" = as.character(col),
                           "integer" = as.integer(col),
                           "numeric" = as.numeric(col),
                           "double" = as.double(col),
                           col) # Return as is if type doesn't match any cases
                  }) %>% unlist()))
}
