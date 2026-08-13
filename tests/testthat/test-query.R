test_that("JSON-LD queries target the configured branch", {
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "survey-data", branch = "review"
  )
  configured <- query(
    config = config,
    query = list(select = list("?s"), where = list(list("@id" = "?s")))
  )
  expect_equal(configured$query$qry$from, "survey-data:review")
  expect_equal(configured$ledger, "survey-data:review")
})

test_that("sendQuery uses the shared request executor", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  configured <- query(
    config = config,
    query = list(select = list("?s"), where = list(list("@id" = "?s")))
  )
  result <- testthat::with_mocked_bindings(
    sendQuery(configured),
    fluree_request = function(config, endpoint, method, body, contentType,
                              operation) {
      captured <<- list(endpoint = endpoint, method = method, body = body,
                        contentType = contentType)
      list(list("https://example.org/one"))
    },
    .package = "novaRush"
  )
  expect_equal(captured$endpoint, "query")
  expect_equal(captured$method, "POST")
  expect_equal(captured$contentType, "application/json")
  expect_true(jsonlite::validate(captured$body))
  expect_true(jsonlite::validate(result))
})
