# The semantic functions now live in semanticModelR. novaRush keeps the names
# working, forwarding to it and warning once per session.

test_that("a moved function still works through novaRush", {
  expect_warning(out <- create_uri_safe("Hello World & Co"), "has moved to semanticModelR")
  expect_equal(out, "hello_world_co")
  expect_equal(out, semanticModelR::create_uri_safe("Hello World & Co"))
})

test_that("the warning names the replacement", {
  # each function warns only once per session, so use a fresh one here
  expect_warning(make_surveycto_context(),
                 "semanticModelR::make_surveycto_context\\(\\)")
})

test_that("a moved function warns only once per session", {
  suppressWarnings(unnest_all(tibble::tibble(a = 1L, b = list(1))))
  expect_silent(unnest_all(tibble::tibble(a = 1L, b = list(1))))
})

test_that("arguments forward correctly, named and positional", {
  triples <- suppressWarnings(pivot_longer_with_type(head(iris, 2)))
  expect_named(triples, c("subject", "predicate", "object", "type"))

  schema <- suppressWarnings(
    schema_from_tripples(df = triples, name = "iris", typename = "type"))
  expect_equal(schema[[1]], list(`_id` = "_collection", name = "iris"))
})

test_that("every documented moved function is exported and forwards", {
  moved <- c("addObject", "addSubject", "create_uri_safe", "cto_to_jsonld",
             "expandIRIs", "export_turtle", "format_object", "identify_nodes",
             "jsonld_from_rdf", "make_cto_semantic_mapping",
             "make_extended_cto_semantic_mapping", "make_surveycto_centext",
             "make_surveycto_context", "make_surveycto_context_list",
             "map_cto_to_rdf", "mapPredicates", "parseCTO", "pivot_longer_with_type",
             "pivot_wider_by_type", "pivotLongerSPO", "plot_rdf_triples_generic",
             "plot_rdf_triples_interactive", "prefixIRIs", "properties2kv",
             "rdf_from_df", "rdf_from_df3", "schema_from_tripples",
             "shorten_predicate", "specIDPredicates", "triples_to_jsonld",
             "unnest_all")

  exported <- getNamespaceExports("novaRush")
  expect_true(all(moved %in% exported))
  expect_true(all(moved %in% getNamespaceExports("semanticModelR")))
})

test_that("novaRush no longer defines the semantic functions itself", {
  # the wrapper body must be a forward, not a copy
  body_text <- paste(deparse(body(novaRush::create_uri_safe)), collapse = " ")
  expect_match(body_text, "semanticModelR::create_uri_safe")
  expect_no_match(body_text, "tolower")
})
