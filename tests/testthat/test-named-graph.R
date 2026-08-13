test_that("namedGraphDocument preserves context and places every resource", {
  document <- list(
    "@context" = list(ex = "https://example.org/"),
    "@graph" = list(
      list("@id" = "ex:a", "ex:name" = "A"),
      list("@id" = "ex:b", "ex:name" = "B")
    )
  )
  placed <- namedGraphDocument(document, "https://example.org/graphs/one")
  expect_equal(placed[["@context"]], document[["@context"]])
  expect_equal(length(placed[["@graph"]]), 2L)
  expect_true(all(vapply(placed[["@graph"]], function(node) {
    identical(node[["@graph"]], "https://example.org/graphs/one")
  }, logical(1))))
})

test_that("namedGraphDocument handles one resource and resource lists", {
  one <- namedGraphDocument(
    list("@id" = "https://example.org/a", "https://example.org/name" = "A"),
    "https://example.org/graphs/one"
  )
  many <- namedGraphDocument(
    list(list("@id" = "https://example.org/a"),
         list("@id" = "https://example.org/b")),
    "https://example.org/graphs/one"
  )
  expect_length(one[["@graph"]], 1L)
  expect_length(many[["@graph"]], 2L)
})

test_that("invalid or conflicting named-graph placement is rejected", {
  expect_error(namedGraphDocument(
    list("@id" = "https://example.org/a"), "not an iri"
  ), "absolute IRI")
  expect_error(namedGraphDocument(
    list("@graph" = list(list(
      "@id" = "https://example.org/a",
      "@graph" = "https://example.org/graphs/old"
    ))),
    "https://example.org/graphs/new"
  ), "already assigned")
  expect_error(namedGraphDocument(
    list(list("https://example.org/name" = "missing id")),
    "https://example.org/graphs/one"
  ), "must contain `@id`")
})

test_that("upsertNamedGraph targets the selected branch", {
  captured <- NULL
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", branch = "main"
  )
  result <- testthat::with_mocked_bindings(
    upsertNamedGraph(
      list("@id" = "https://example.org/a"),
      "https://example.org/graphs/one", config, branch = "candidate"
    ),
    fluree_request = function(config, endpoint, method, body, query,
                              operation, write) {
      captured <<- list(endpoint = endpoint, method = method, body = body,
                        query = query, operation = operation, write = write)
      list(t = 3L)
    },
    .package = "novaRush"
  )
  expect_equal(result$t, 3L)
  expect_equal(captured$endpoint, "upsert")
  expect_equal(captured$query$ledger, "demo:candidate")
  expect_equal(captured$body[["@graph"]][[1]][["@graph"]],
               "https://example.org/graphs/one")
  expect_true(captured$write)
})

test_that("queryNamedGraph constructs a structured graph source", {
  captured <- NULL
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", branch = "main",
    defaultContext = list(schema = "https://schema.org/")
  )
  result <- testthat::with_mocked_bindings(
    queryNamedGraph(
      list(
        "@context" = list(ex = "https://example.org/"),
        select = list("?name"),
        where = list(list("@id" = "?s", "ex:name" = "?name"))
      ),
      "https://example.org/graphs/one", config, branch = "candidate"
    ),
    fluree_request = function(config, endpoint, method, body, operation,
                              write) {
      captured <<- list(endpoint = endpoint, method = method, body = body,
                        operation = operation, write = write)
      list(list("A"))
    },
    .package = "novaRush"
  )
  expect_length(result, 1L)
  expect_equal(captured$body$from, list(
    "@id" = "demo:candidate", graph = "https://example.org/graphs/one"
  ))
  expect_equal(names(captured$body[["@context"]]), c("schema", "ex"))
  expect_false(captured$write)
})

test_that("queryNamedGraph rejects a conflicting source", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  expect_error(queryNamedGraph(
    list(from = "demo:main", select = list("?s"),
         where = list(list("@id" = "?s"))),
    "https://example.org/graphs/one", config
  ), "conflicts")
})

test_that("R6 named-graph methods forward configuration", {
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", branch = "candidate"
  )
  db <- FlureeInstance$new(config)
  result <- testthat::with_mocked_bindings(
    db$upsertNamedGraph(
      list("@id" = "https://example.org/a"),
      "https://example.org/graphs/one"
    ),
    upsertNamedGraph = function(document, graph, config, branch) {
      list(graph = graph, branch = branch, ledger = config$ledger)
    },
    .package = "novaRush"
  )
  expect_equal(result$branch, "candidate")
  expect_equal(result$ledger, "demo")
})
