
#' rdf_from_df3
#'
#' @param df Dataframe or tibble
#' @param subject. Character. Default "subject"
#' @param predicate Character. Default "predicate"
#' @param object Character. Default "object"
#' @import rdflib
#' @return rdf object
#' @export
#'
#' @examples
#' iristriples <- pivot_longer_with_type(iris)
#' rdfiris <- rdf_from_df3(iristriples, base = "http://example.com/iris/", vocab = "http://example.com/irisvocab/#")
#' rdflib::rdf_serialize(rdfiris, "rdfiris.json", format = "jsonld")

rdf_from_df3 <- function(df,
                         subject = "subject",
                         predicate = "predicate",
                         object = "object",
                         base = NULL,
                         vocab = NULL) {
  df <- as.data.frame(df)

  if (!is.null(base)) {
    df[[subject]] <- paste0(base, df[[subject]])
  }

  if (!is.null(vocab)) {
    df[[predicate]] <- paste0(vocab, df[[predicate]])
  }

  rdf1 <- rdflib::rdf()
  for(i in 1:nrow(df)) rdf1 %>% rdflib::rdf_add(object = df[i, object],
                                                predicate = df[i, predicate],
                                                subject = df[i, subject])
  rdf1
}

# combine with pivot_longer_with_type
#' rdf_from_df
#'
#' @param df Dataframe or tibble
#' @param subject Character. Default "subject"
#' @param predicate Character. Default "predicate"
#' @param object Character. Default "object"
#'
#' @return rdf object
#' @export
#'
#' @examples
#' irisrdf <- rdf_from_df(iris)

rdf_from_df <- function(df, subject = "subject", predicate = "predicate", object = "object", ...) {
  df <- pivot_longer_with_type(df, ...)
  rdf_from_df3(df, subject = subject, predicate = predicate, object = object)
}

# function to create Json-LD from an rdf object. Make the rdf character first
#' jsonld_from_rdf
#' @param rdf rdf object
#' @param format Character. Default "application/ld+json"
#' @return jsonld object
#' @export
#' @examples
#' jsonld_from_rdf(rdf1)
#' jsonld_from_rdf(rdf1, format = "application/n-quads")

jsonld_from_rdf <- function(rdf) {
  # Make the rdf character first by wriring it for file
  tp <- tempfile(fileext = ".nq")
  rdf_serialize(rdf, tp, format = "nquads")

  jsonld::jsonld_from_rdf(rdf = tp)
}
