#' Create Fluree transaction body from dataframe and context tibble
#'
#' Create the body for a Fluree transaction containing fields `@context`, `ledger`, and `insert`.
#' Currently only supports insertion of data. 
#' TODO: expand to accommodate queries
#'
#' @param ledger [string] The name of the ledger into which to transact the data
#' @param data [data.frame] Containing columns `subject`, `predicate`, `object`
#' @param context [data.frame] Containing columns `prefix`, `IRI`
#'
#' @return A JSON-LD object with keys `@context`, `ledger`, `insert`
#' @export
createBody <- function(ledger = NULL,
                       data = NULL, 
                       context = NULL) {
  
  # @context field
  if (!is.null(context)) {
    context_list <- setNames(as.list(context$IRI), context$prefix)
    context_json <- list("@context" = context_list)
  } else {
    context_json <- NULL
  }
  
  # ledger field
  if (is.null(ledger)) {
    stop("Please provide a ledger name for this transaction.")
  } else {
    ledger_json <- list("ledger" = ledger)
  }
  
  # content field
  if (!is.null(data)) {
    # replace rdf:type predicates with @type according to JSON-LD conventions
    data <- data %>% 
      mutate(predicate = if_else(predicate == "rdf:type", "@type", predicate))
    
    # helper function to construct key-value pairs
    spo_json <- function(subject, data) {
      sub_json <- list("@id" = subject)
      
      kv <- data %>% 
        distinct() %>% 
        group_by(predicate) %>% 
        nest() %>% 
        mutate(x = map(data, ~deframe(unname(.x))))
      
      l <- kv$x
      names(l) <- kv$predicate
      
      return(c(sub_json, l))
    }
    
    content <- data %>% 
      group_by(subject) %>% 
      nest() %>% 
      mutate(json = map2(subject, data, ~spo_json(.x, .y))) %>% 
      select(json)
    
    content_json <- list("insert" = content$json)
  } else {
    content_json <- NULL
  }
  
  # combine key-value pairs
  combined_json <- c(context_json, ledger_json, content_json)
  json_output <- toJSON(combined_json, pretty = TRUE, auto_unbox = TRUE)
  
  return(json_output)
}


