test_that("ledger creation uses the V4 create endpoint", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  testthat::local_mocked_bindings(
    fluree_request = function(config, endpoint, method, body, operation,
                              allowStatus, returnResponse) {
      captured <<- list(endpoint = endpoint, method = method, body = body,
                        operation = operation, allowStatus = allowStatus)
      list(status = 200L, body = list(ledger_id = "demo:main", t = 0L))
    },
    .package = "novaRush"
  )
  result <- createLedger(config)
  expect_equal(result$ledger_id, "demo:main")
  expect_equal(captured$endpoint, "create")
  expect_equal(captured$method, "POST")
  expect_equal(captured$body$ledger, "demo")
  expect_equal(captured$allowStatus, 409L)
})

test_that("ledger creation is idempotent on HTTP 409", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  testthat::local_mocked_bindings(
    fluree_request = function(...) {
      list(status = 409L, body = list(error = "ledger already exists"))
    },
    .package = "novaRush"
  )
  result <- createLedger(config)
  expect_false(result$created)
  expect_true(result$exists)
  expect_equal(result$ledger, "demo")
})
