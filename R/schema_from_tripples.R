
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
#' @export

schema_from_tripples <- function(df = NULL, name = NULL) {

  if (is.null(df) || is.null(name)) {
    stop("Both df and name must be provided.")
  }

  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Check if the data frame has the required columns
  required_columns <- c("predicate", "type" )
  df <- df %>% select(predicate, type) %>% distinct()
  missing_columns <- setdiff(required_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop(paste("Data frame is missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Create the schema body
  schema_body <- list(
    list(
      `_id` = "_collection",
      `name` = name
    )
  )

  # Populate the schema body with tripples
  schema_body2  <- pmap(df %>% mutate(name = name) %>% select(name, predicate, type),
                                   ~{list( `_id` = "_predicate",
                                           `name` = paste0(..1, "/", ..2),
                                           type = ..3)})

  return(c(schema_body, schema_body2))
}
