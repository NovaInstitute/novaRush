#' unnest_all
#' A recursive unnesting function that unnests all list-columns in a data frame.
#' @param .df
#'
#' @returns A data frame with all list-columns unnested.
#' @export
#'
#' @examples

unnest_all <- function( .df )
{
  lc <- purrr::keep(.df, is.list) %>% names
  if( length(lc) == 0 ) return(.df)
  tidyr::unnest( .df, all_of(lc)) %>% unnest_all()
}
