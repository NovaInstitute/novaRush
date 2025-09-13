# create_uri_safe
#' @title
#' create_uri_safe
#' @description
#' Function to create URI-safe identifiers from text strings.
#' @param text
#'
#' @returns
#' @export
#'
#' @examples
#' create_uri_safe("Example Text with Spaces & Special Characters!")

create_uri_safe <- function(text) {
  text %>%
    tolower() %>%
    gsub("[^a-z0-9_]", "_", .) %>%
    gsub("_{2,}", "_", .) %>%
    gsub("^_|_$", "", .)
}
