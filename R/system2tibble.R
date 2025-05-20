#' system2tibble
#' @description
#' Turn the results of a call to system into a tibble
#'
#' @param chr Character. The output of a call to system
#'
#' @return tibble
#' @export
#'
#' @examples
#' system2tibble(system("docker ps -a", intern = TRUE))

 system2tibble <- function(chr = system("docker ps -a", intern = TRUE) ) {
  nms <- chr %>% tibble() %>% `[`(1,) %>% pull() %>% strsplit("[[:space:]]{2,20}") %>% unlist() %>% discard(~.x == "")
  out <- chr %>% as.tibble() %>% `[`(-1,) %>% tidyr::separate(col = value, sep = "[[:space:]]{2,20}", into = nms)
  out
   }
