
#' schema_from_tripples
#' @title Create a schema body from tripples
#' @description This function creates a schema body from tripples.
#' It is used to create a schema body from tripples resulting from pivot_longer_with_type.
#' @param df A data frame containing the tripples.
#' @param name Character. The name of the schema. This will be used to populate the name field in the first list item
#' e.g. list(`_id` = "_collection", `name` = "iris")
#'
#' @return A schema body in the form of a list ready to be transacted
#' @examples
#' df1 <- pivot_longer_with_type(iris)
#' df_schema <- schema_from_tripples(df = df1, name = "iris")
#' jsonlite::toJSON(df_schema, auto_unbox = TRUE, pretty = TRUE)
#' @export

schema_from_tripples <- function(df = NULL, name = NULL, typename = "object_type", predicate_name = "predicate") {

  if (is.null(df) || is.null(name)) {
    stop("Both df and name must be provided.")
  }

  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  if (typename %in% names(df)) {
    df <- df %>% rename(type = all_of(typename))
  }

  if (predicate_name %in% names(df)) {
    df <- df %>% rename(predicate = all_of(predicate_name))
  }

  # Check if the data frame has the required columns
  required_columns <- c("predicate", "type" )

  if (!all(required_columns %in% colnames(df))) {
    stop(paste("Data frame must contain the following columns:", paste(required_columns, collapse = ", ")))
  }

  df <- df %>% select(predicate, type) %>% distinct()

  # Create the schema body
  schema_body <- list(
    list(
      `_id` = "_collection",
      `name` = name
    )
  )

  # Populate the schema body with tripples
  schema_body2  <- purrr::pmap(df %>% mutate(name = name) %>% select(name, predicate, type),
                                   ~{list( `_id` = "_predicate",
                                           `name` = paste0(..1, "/", ..2),
                                           type = ..3)})

  return(c(schema_body, schema_body2))
}
