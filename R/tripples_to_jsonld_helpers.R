
#' shorten_predicate
#' @description
#' Function to convert predicate to short form
#'
#' @param pred Character string of the predicate URI
#'
#' @returns
#' @export
#'
#' @examples

shorten_predicate <- function(pred, context_df = make_surveycto_centext()) {
  for (i in 1:nrow(context_df)) {
    full_ns <- context_df$namespace[i]
    prefix <- context_df$prefix[i]
    if (startsWith(pred, full_ns)) {
      return(paste0(prefix, ":", gsub(full_ns, "", pred)))
    }
  }

  # Handle common RDF predicates
  if (pred == "rdf:type" || pred == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") {
    return("@type")
  }
  if (startsWith(pred, "http://www.w3.org/1999/02/22-rdf-syntax-ns#")) {
    return(paste0("rdf:", gsub("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "", pred)))
  }

  return(pred)
}


#' format_object
#' @description
#' Function to convert object based on type. If the type is "uri", the object should be a list with @id as the name so the the json can be "@id": <object>.
#' @param obj
#' @param obj_type
#'
#' @returns
#' @export
#'
#' @examples

format_object <- function(obj, obj_type, context_df = NULL) {

  if (is.null(context_df)) {
    context_df <- make_surveycto_centext()
  }

  if (obj_type == "uri") {
    # Try to shorten URI using context
    for (i in 1:nrow(context_df)) {
      full_ns <- context_df$namespace[i]
      prefix <- context_df$prefix[i]
      if (startsWith(obj, full_ns)) {
        return(paste0(prefix, ":", gsub(full_ns, "", obj)))
      }
    }
    # If not shortened, return as @id
    return(list("@id" = obj))
  } else {
    return(obj)
  }
}
