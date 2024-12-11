#' Create Fluree transaction body from dataframe and context tibble
#'
#' Currently only supports insertion of data. 
#' TODO: expand to accommodate queries
#'
#' @param ledger [string] The name of the ledger into which to transact the data
#' @param data [data.frame] Containing columns `subject`, `predicate`, `object`
#' @param context [data.frame] Containing columns `prefix`, `IRI`
#'
#' @return A JSON-LD object with keys `@context`, `ledger`, `insert`
#' @export
createBody <- function(ledger,
                       data, 
                       context) {
  
  # @context
  context_list <- setNames(as.list(context$IRI), context$prefix)
  context_json <- list("@context" = context_list)
  
  # ledger
  ledger_json <- list("ledger" = ledger)
  
  # content of transaction
  content_json <- data %>%
    pivot_wider(
      names_from = predicate, 
      values_from = object
    ) %>%
    mutate(`@id` = subject) %>%
    select(-subject) %>% 
    { list("insert" = .) }

  combined_json <- c(context_json, ledger_json, content_json)
  json_output <- toJSON(combined_json, pretty = TRUE, auto_unbox = TRUE)
  return(json_output)
  
}
