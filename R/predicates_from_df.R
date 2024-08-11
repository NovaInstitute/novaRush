
#' predicates_from_df
#'
#' @param data Data frame or tibble
#'
#' @return list
#' @export
#'
#' @examples
#' predicates_from_df(iris, returnJSON = T)
#' predicates_from_df(iris)
predicates_from_df <- function(data, returnJSON = FALSE){
  df1 <- pivot_longer_with_type(data) %>%
    select(predicate, type) %>%
    distinct()

  l <- map(1:nrow(df1), ~list(
    "_id" = '_predicate',
    name = df1[., "predicate"] %>% pull(),
    type = df1[.,"type"] %>% pull()
  ))

  if (returnJSON) return(jsonlite::toJSON(l))
  l

}
