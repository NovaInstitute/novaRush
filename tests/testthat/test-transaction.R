test_that("functional transactions retain endpoint and branch", {
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "survey-data", branch = "review"
  )
  configured <- transact(
    config = config,
    transaction = list("@graph" = list(list("@id" = "https://example.org/a")))
  )
  expect_equal(configured$endpoint, "insert")
  expect_equal(configured$ledger, "survey-data:review")
  expect_null(configured$transaction$txn$ledger)
})

test_that("sendTransaction uses a ledger query parameter", {
  captured <- NULL
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "survey-data", branch = "review"
  )
  configured <- transact(
    config = config,
    transaction = list("@graph" = list(list("@id" = "https://example.org/a")))
  )
  result <- testthat::with_mocked_bindings(
    sendTransaction(configured),
    fluree_request = function(config, endpoint, method, body, query,
                              contentType, operation) {
      captured <<- list(endpoint = endpoint, method = method, body = body,
                        query = query, contentType = contentType)
      list(t = 1L, commit_id = "commit-1")
    },
    .package = "novaRush"
  )
  expect_equal(captured$endpoint, "insert")
  expect_equal(captured$query$ledger, "survey-data:review")
  expect_equal(captured$contentType, "application/json")
  expect_true(jsonlite::validate(result))
})

test_that("upsert uses its dedicated V4 endpoint", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  document <- list("@graph" = list(list("@id" = "https://example.org/a")))
  result <- testthat::with_mocked_bindings(
    upsert(config, document),
    fluree_request = function(config, endpoint, method, body, query,
                              operation) {
      captured <<- list(endpoint = endpoint, body = body, query = query)
      list(t = 2L)
    },
    .package = "novaRush"
  )
  expect_equal(result$t, 2L)
  expect_equal(captured$endpoint, "upsert")
  expect_equal(captured$query$ledger, "demo:main")
})

test_that("functional delete is configured as an update", {
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo",
    defaultContext = list(id = "@id")
  )
  configured <- delete(config, "https://example.org/a")
  expect_equal(configured$endpoint, "update")
  expect_equal(configured$ledger, "demo:main")
})
