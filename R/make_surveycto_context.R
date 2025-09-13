#' make_surveycto_context
#'
#' @returns tibble with columns `prefix` and `namespace`
#' @export
#'
#' @examples
#' surveycto_context <- make_surveycto_context()

make_surveycto_context <- function(){
  tribble(
    ~prefix, ~namespace,
    "xsd", "http://www.w3.org/2001/XMLSchema#",
    "rdfs", "http://www.w3.org/2000/01/rdf-schema#",
    "owl", "http://www.w3.org/2002/07/owl#",
    "survey", "https://w3id.org/survey-ontology#",
    "dcat", "http://www.w3.org/ns/dcat#",
    "dcterms", "http://purl.org/dc/terms/",
    "prov", "http://www.w3.org/ns/prov#",
    "foaf", "http://xmlns.com/foaf/0.1/",
    "vcard", "http://www.w3.org/2006/vcard/ns#",
    "geo", "http://www.w3.org/2003/01/geo/wgs84_pos#",
    "gsp", "http://www.opengis.net/ont/geosparql#",
    "time", "http://www.w3.org/2006/time#",
    "ma", "http://www.w3.org/ns/ma-ont#",
    "schema", "http://schema.org/",
    "skos", "http://www.w3.org/2004/02/skos/core#",
    "sioc", "http://rdfs.org/sioc/ns#",
    "gs1", "http://gs1.org/voc/"
  )

}

#' make_surveycto_centext
#' @description Alternatiove duie to spelling error in original function name
#' @returns tibble with columns `prefix` and `namespace`
#' @export
#'
#' @examples
#' surveycto_centext <- make_surveycto_centext()

make_surveycto_centext <- function(){
  make_surveycto_context()
}

#' make_surveycto_context_list
#' @description Create a named list from the surveycto context tibble
#' @returns named list with prefixes as names and namespaces as values
#' @export
#'
#' @examples
make_surveycto_context_list <- function(){
  ctx <- make_surveycto_context()
  setNames(as.list(ctx$namespace), ctx$prefix)
  list("@context"=ctx)
}
