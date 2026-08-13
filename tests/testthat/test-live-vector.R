test_that("live Fluree exact vector search ranks and isolates embeddings", {
  skip_if_no_live_fluree()
  config <- live_fluree_config()
  createLedger(config)
  db <- FlureeInstance$new(config)$connect()

  graph <- live_fluree_graph_iri("vectors")
  other_graph <- live_fluree_graph_iri("vectors-other")
  base <- paste0(graph, "/entity/")
  vector_property <- "https://data.nova.org/test/schema/embedding"
  model_property <- "https://data.nova.org/test/schema/embeddingModel"
  dimension_property <- "https://data.nova.org/test/schema/embeddingDimension"
  records <- list(
    list("@id" = paste0(base, "exact"),
         "https://data.nova.org/test/schema/embedding" = c(1, 0, 0)),
    list("@id" = paste0(base, "near"),
         "https://data.nova.org/test/schema/embedding" = c(.9, .1, 0)),
    list("@id" = paste0(base, "far"),
         "https://data.nova.org/test/schema/embedding" = c(0, 0, 1))
  )
  db$upsertVectors(
    records, graph, vector_property,
    model = "manual-live-v1", model_property = model_property,
    dimension_property = dimension_property, dimension = 3L
  )
  db$upsertVectors(
    list(list(
      "@id" = paste0(other_graph, "/hidden"),
      "https://data.nova.org/test/schema/embedding" = c(1, 0, 0)
    )),
    other_graph, vector_property, dimension = 3L
  )

  results <- db$searchVectors(
    graph, vector_property, c(1, 0, 0), metric = "cosine", limit = 3L
  )
  ids <- vapply(results, function(row) as.character(row[[1L]]), character(1))
  scores <- vapply(results, function(row) as.numeric(row[[2L]]), numeric(1))
  expect_equal(ids[[1L]], paste0(base, "exact"))
  expect_equal(ids[[2L]], paste0(base, "near"))
  expect_equal(ids[[3L]], paste0(base, "far"))
  expect_equal(scores[[1L]], 1, tolerance = 1e-6)
  expect_true(scores[[1L]] >= scores[[2L]])
  expect_true(scores[[2L]] >= scores[[3L]])
  expect_false(paste0(other_graph, "/hidden") %in% ids)

  expect_error(db$upsertVectors(
    list(list("@id" = paste0(base, "invalid"),
              "https://data.nova.org/test/schema/embedding" = c(1, 0))),
    graph, vector_property, dimension = 3L
  ), "dimension mismatch")
})
