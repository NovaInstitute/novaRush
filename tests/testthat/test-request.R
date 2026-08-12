test_that("request parameters use the V4 API and hide no required headers", {
  config <- setConfig(
    baseUrl = "https://fluree.test", ledger = "demo", branch = "review",
    apiKey = "token-value", timeout = 9
  )
  params <- novaRush:::generateFetchParams(
    config, "info/demo%3Areview", method = "GET"
  )
  expect_equal(params$url,
               "https://fluree.test/v1/fluree/info/demo%3Areview")
  expect_equal(params$config$method, "GET")
  expect_equal(params$config$timeout, 9)
  expect_equal(unname(params$config$headers[["Authorization"]]),
               "Bearer token-value")
})

test_that("health requests target the server root", {
  config <- setConfig(baseUrl = "http://localhost:8090", ledger = "demo")
  params <- novaRush:::generateFetchParams(config, "health", method = "GET")
  expect_equal(params$url, "http://localhost:8090/health")
})

test_that("the request executor encodes and decodes JSON", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  testthat::local_mocked_bindings(
    .fluree_perform_request = function(method, url, headers, timeout, body) {
      captured <<- list(method = method, url = url, headers = headers,
                        timeout = timeout, body = body)
      list(status = 200L, text = '{"status":"ok","version":"4.1.2"}')
    },
    .package = "novaRush"
  )
  result <- novaRush:::fluree_request(
    config, "query", method = "POST", body = list(select = "?s")
  )
  expect_equal(result$status, "ok")
  expect_equal(captured$method, "POST")
  expect_equal(captured$url, "http://fluree.test/v1/fluree/query")
  expect_true(jsonlite::validate(captured$body))
})

test_that("request query parameters are URL encoded", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  testthat::local_mocked_bindings(
    .fluree_perform_request = function(method, url, headers, timeout, body) {
      captured <<- url
      list(status = 200L, text = '{"t":1}')
    },
    .package = "novaRush"
  )
  novaRush:::fluree_request(
    config, "insert", method = "POST", body = list("@id" = "ex:a"),
    query = list(ledger = "demo:review")
  )
  expect_match(captured, "ledger=demo%3Areview", fixed = TRUE)
})

test_that("POST query timeouts are not classified as uncertain writes", {
  condition <- novaRush:::.fluree_transport_error(
    simpleError("Timeout was reached"), "query",
    "http://localhost/query", "POST"
  )
  expect_s3_class(condition, "fluree_request_error")
  expect_false(inherits(condition, "fluree_uncertain_write"))
})

test_that("HTTP failures return structured Fluree errors", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  testthat::local_mocked_bindings(
    .fluree_perform_request = function(...) {
      list(status = 400L, text = '{"error":"invalid query"}')
    },
    .package = "novaRush"
  )
  error <- tryCatch(
    novaRush:::fluree_request(config, "query", method = "POST"),
    error = identity
  )
  expect_s3_class(error, "fluree_request_error")
  expect_equal(error$status, 400L)
  expect_match(conditionMessage(error), "invalid query")
})

test_that("large numeric values are summarized for diagnostics", {
  traced <- novaRush:::fluree_trace_payload(list(vector = as.numeric(1:20)))
  expect_true(traced$vector$`__vector__`)
  expect_equal(traced$vector$dimension, 20L)
})

test_that("write timeouts have an explicit uncertain outcome", {
  condition <- novaRush:::.fluree_transport_error(
    simpleError("Timeout was reached after 60000 milliseconds"),
    "upsert", "http://localhost/upsert", "POST"
  )
  expect_s3_class(condition, "fluree_uncertain_write")
  expect_match(conditionMessage(condition), "verify remote state")
})

test_that("read timeouts are ordinary request errors", {
  condition <- novaRush:::.fluree_transport_error(
    simpleError("Timeout was reached"), "query",
    "http://localhost/query", "GET"
  )
  expect_s3_class(condition, "fluree_request_error")
  expect_false(inherits(condition, "fluree_uncertain_write"))
})
