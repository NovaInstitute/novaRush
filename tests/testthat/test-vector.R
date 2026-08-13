vector_test_property <- "https://example.org/vector"

test_that("vectors are validated strictly", {
  checked <- validateVectors(list(c(1, 2), c(3, 4)), dimension = 2L)
  expect_equal(checked$count, 2L)
  expect_equal(checked$dimension, 2L)
  expect_error(validateVectors(list(c(1, NA_real_))), "finite")
  expect_error(validateVectors(list(c(1, 2), c(1))), "consistent")
  expect_error(validateVectors(c(1, 2), dimension = 3L), "mismatch")
  expect_error(validateVectors(list()), "at least one")
})

test_that("Fluree vector literals use transaction and query datatypes", {
  transaction <- flureeVector(c(.1, .2))
  query <- flureeVector(c(.1, .2), query = TRUE)
  expect_equal(transaction[["@type"]], "@vector")
  expect_equal(query[["@type"]],
               "https://ns.flur.ee/db#embeddingVector")
  expect_equal(transaction[["@value"]], c(.1, .2))
})

test_that("vector records retain caller-defined metadata predicates", {
  prepared <- novaRush:::.prepare_vector_records(
    list(list("@id" = "https://example.org/a",
              "https://example.org/vector" = c(.1, .2))),
    vector_property = vector_test_property,
    model = "embedding-model-v1",
    model_property = "https://example.org/model",
    dimension_property = "https://example.org/dimension"
  )
  node <- prepared$records[[1L]]
  expect_equal(node[[vector_test_property]][["@type"]], "@vector")
  expect_equal(node[["https://example.org/model"]], "embedding-model-v1")
  expect_equal(node[["https://example.org/dimension"]][["@value"]], 2L)
  expect_error(novaRush:::.prepare_vector_records(
    list(list("@id" = "https://example.org/a",
              "https://example.org/vector" = c(.1, .2))),
    vector_test_property, model = "model-without-property"
  ), "supplied together")
})

test_that("vector query construction supports all exact metrics", {
  cosine <- vectorSimilarityQuery(vector_test_property, c(1, 0), "cosine")
  dot <- vectorSimilarityQuery(vector_test_property, c(1, 0), "dot")
  euclidean <- vectorSimilarityQuery(
    vector_test_property, c(1, 0), "euclidean", threshold = 0.5
  )
  expect_match(cosine$where[[2L]][[3L]], "cosineSimilarity")
  expect_match(dot$where[[2L]][[3L]], "dotProduct")
  expect_match(euclidean$where[[2L]][[3L]], "euclideanDistance")
  expect_equal(cosine$orderBy, list(list("desc", "?score")))
  expect_equal(euclidean$orderBy, list(list("asc", "?score")))
  expect_equal(cosine$values[[2L]][[1L]][["@type"]],
               "https://ns.flur.ee/db#embeddingVector")
  expect_match(euclidean$where[[3L]][[2L]], "<=", fixed = TRUE)
})

test_that("upsertVectors delegates to named graph storage", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  result <- testthat::with_mocked_bindings(
    upsertVectors(
      list(list("@id" = "https://example.org/a",
                "https://example.org/vector" = c(1, 0))),
      "https://example.org/graphs/vectors", vector_test_property, config,
      branch = "candidate"
    ),
    upsertNamedGraph = function(document, graph, config, branch) {
      captured <<- list(document = document, graph = graph, branch = branch)
      list(t = 4L)
    },
    .package = "novaRush"
  )
  expect_equal(result$t, 4L)
  expect_equal(captured$branch, "candidate")
  expect_equal(captured$document[[1L]][[vector_test_property]][["@type"]],
               "@vector")
})

test_that("searchVectors scopes the exact query to graph and branch", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  result <- testthat::with_mocked_bindings(
    searchVectors(
      "https://example.org/graphs/vectors", vector_test_property,
      c(1, 0), config, branch = "candidate", limit = 3L
    ),
    queryNamedGraph = function(query, graph, config, branch) {
      captured <<- list(query = query, graph = graph, branch = branch)
      list(list("https://example.org/a", 1))
    },
    .package = "novaRush"
  )
  expect_length(result, 1L)
  expect_equal(captured$graph, "https://example.org/graphs/vectors")
  expect_equal(captured$branch, "candidate")
  expect_equal(captured$query$limit, 3L)
})

test_that("diagnostic traces redact embedding-sized vectors", {
  traced <- novaRush:::fluree_trace_payload(flureeVector(seq_len(20)))
  expect_true(traced[["@value"]][["__vector__"]])
  expect_equal(traced[["@value"]]$dimension, 20L)
})
