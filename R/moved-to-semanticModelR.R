# The semantic modelling functions now live in semanticModelR. These wrappers keep
# novaRush::<name>() working so existing scripts and vignettes are unaffected, while
# there is only one copy of the code to maintain. Each warns once per session.

warned_moved <- new.env(parent = emptyenv())

warn_moved <- function(name) {
  if (is.null(warned_moved[[name]])) {
    assign(name, TRUE, envir = warned_moved)
    warning(name, "() has moved to semanticModelR and will be removed from novaRush ",
            "in a future version. Use semanticModelR::", name, "() instead.",
            call. = FALSE)
  }
  invisible(NULL)
}

#' Functions that have moved to semanticModelR
#'
#' @description
#' Turning tables and SurveyCTO form definitions into RDF is modelling, not client
#' work, so it now lives in \pkg{semanticModelR}. These names remain exported from
#' novaRush and forward to it unchanged, warning once per session. They will be
#' removed in a future version, so call the semanticModelR function directly:
#'
#' ```r
#' semanticModelR::pivot_longer_with_type(head(iris))
#' semanticModelR::map_cto_to_rdf(formdef, instrument = "my_form")
#' ```
#'
#' semanticModelR also carries fixes that were never applied here, and adds a
#' \pkg{tidygraph} bridge: [semanticModelR::triples_to_tbl_graph()] and
#' [semanticModelR::as_triples()].
#'
#' @param ... Passed to the semanticModelR function of the same name.
#'
#' @returns Whatever the corresponding semanticModelR function returns.
#' @seealso [fluree_graph()], which returns a tidygraph graph directly.
#' @name novaRush-moved
NULL

#' @rdname novaRush-moved
#' @export
addObject <- function(...) {
  warn_moved("addObject")
  semanticModelR::addObject(...)
}

#' @rdname novaRush-moved
#' @export
addSubject <- function(...) {
  warn_moved("addSubject")
  semanticModelR::addSubject(...)
}

#' @rdname novaRush-moved
#' @export
create_uri_safe <- function(...) {
  warn_moved("create_uri_safe")
  semanticModelR::create_uri_safe(...)
}

#' @rdname novaRush-moved
#' @export
cto_to_jsonld <- function(...) {
  warn_moved("cto_to_jsonld")
  semanticModelR::cto_to_jsonld(...)
}

#' @rdname novaRush-moved
#' @export
expandIRIs <- function(...) {
  warn_moved("expandIRIs")
  semanticModelR::expandIRIs(...)
}

#' @rdname novaRush-moved
#' @export
export_turtle <- function(...) {
  warn_moved("export_turtle")
  semanticModelR::export_turtle(...)
}

#' @rdname novaRush-moved
#' @export
format_object <- function(...) {
  warn_moved("format_object")
  semanticModelR::format_object(...)
}

#' @rdname novaRush-moved
#' @export
identify_nodes <- function(...) {
  warn_moved("identify_nodes")
  semanticModelR::identify_nodes(...)
}

#' @rdname novaRush-moved
#' @export
jsonld_from_rdf <- function(...) {
  warn_moved("jsonld_from_rdf")
  semanticModelR::jsonld_from_rdf(...)
}

#' @rdname novaRush-moved
#' @export
make_cto_semantic_mapping <- function(...) {
  warn_moved("make_cto_semantic_mapping")
  semanticModelR::make_cto_semantic_mapping(...)
}

#' @rdname novaRush-moved
#' @export
make_extended_cto_semantic_mapping <- function(...) {
  warn_moved("make_extended_cto_semantic_mapping")
  semanticModelR::make_extended_cto_semantic_mapping(...)
}

#' @rdname novaRush-moved
#' @export
make_surveycto_centext <- function(...) {
  warn_moved("make_surveycto_centext")
  semanticModelR::make_surveycto_centext(...)
}

#' @rdname novaRush-moved
#' @export
make_surveycto_context <- function(...) {
  warn_moved("make_surveycto_context")
  semanticModelR::make_surveycto_context(...)
}

#' @rdname novaRush-moved
#' @export
make_surveycto_context_list <- function(...) {
  warn_moved("make_surveycto_context_list")
  semanticModelR::make_surveycto_context_list(...)
}

#' @rdname novaRush-moved
#' @export
map_cto_to_rdf <- function(...) {
  warn_moved("map_cto_to_rdf")
  semanticModelR::map_cto_to_rdf(...)
}

#' @rdname novaRush-moved
#' @export
mapPredicates <- function(...) {
  warn_moved("mapPredicates")
  semanticModelR::mapPredicates(...)
}

#' @rdname novaRush-moved
#' @export
parseCTO <- function(...) {
  warn_moved("parseCTO")
  semanticModelR::parseCTO(...)
}

#' @rdname novaRush-moved
#' @export
pivot_longer_with_type <- function(...) {
  warn_moved("pivot_longer_with_type")
  semanticModelR::pivot_longer_with_type(...)
}

#' @rdname novaRush-moved
#' @export
pivot_wider_by_type <- function(...) {
  warn_moved("pivot_wider_by_type")
  semanticModelR::pivot_wider_by_type(...)
}

#' @rdname novaRush-moved
#' @export
pivotLongerSPO <- function(...) {
  warn_moved("pivotLongerSPO")
  semanticModelR::pivotLongerSPO(...)
}

#' @rdname novaRush-moved
#' @export
plot_rdf_triples_generic <- function(...) {
  warn_moved("plot_rdf_triples_generic")
  semanticModelR::plot_rdf_triples_generic(...)
}

#' @rdname novaRush-moved
#' @export
plot_rdf_triples_interactive <- function(...) {
  warn_moved("plot_rdf_triples_interactive")
  semanticModelR::plot_rdf_triples_interactive(...)
}

#' @rdname novaRush-moved
#' @export
prefixIRIs <- function(...) {
  warn_moved("prefixIRIs")
  semanticModelR::prefixIRIs(...)
}

#' @rdname novaRush-moved
#' @export
properties2kv <- function(...) {
  warn_moved("properties2kv")
  semanticModelR::properties2kv(...)
}

#' @rdname novaRush-moved
#' @export
rdf_from_df <- function(...) {
  warn_moved("rdf_from_df")
  semanticModelR::rdf_from_df(...)
}

#' @rdname novaRush-moved
#' @export
rdf_from_df3 <- function(...) {
  warn_moved("rdf_from_df3")
  semanticModelR::rdf_from_df3(...)
}

#' @rdname novaRush-moved
#' @export
schema_from_tripples <- function(...) {
  warn_moved("schema_from_tripples")
  semanticModelR::schema_from_tripples(...)
}

#' @rdname novaRush-moved
#' @export
shorten_predicate <- function(...) {
  warn_moved("shorten_predicate")
  semanticModelR::shorten_predicate(...)
}

#' @rdname novaRush-moved
#' @export
specIDPredicates <- function(...) {
  warn_moved("specIDPredicates")
  semanticModelR::specIDPredicates(...)
}

#' @rdname novaRush-moved
#' @export
triples_to_jsonld <- function(...) {
  warn_moved("triples_to_jsonld")
  semanticModelR::triples_to_jsonld(...)
}

#' @rdname novaRush-moved
#' @export
unnest_all <- function(...) {
  warn_moved("unnest_all")
  semanticModelR::unnest_all(...)
}
