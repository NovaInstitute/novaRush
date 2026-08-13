test_that("setConfig creates a Fluree v4 configuration", {
  config <- setConfig(
    baseUrl = "http://localhost:8090/", ledger = "survey-data",
    branch = "review", apiKey = "secret", timeout = 12
  )
  expect_s3_class(config, "nova_fluree_config")
  expect_equal(config$baseUrl, "http://localhost:8090")
  expect_equal(config$host, "localhost")
  expect_equal(config$port, "8090")
  expect_equal(config$ledger, "survey-data")
  expect_equal(config$branch, "review")
  expect_false(config$signMessages)
  expect_equal(config$timeout, 12)
})

test_that("legacy host and port configuration remains supported", {
  config <- setConfig(host = "fluree.test", port = 8090, ledger = "demo")
  expect_equal(config$baseUrl, "http://fluree.test:8090")
  expect_equal(config$host, "fluree.test")
  expect_equal(config$port, 8090)
})

test_that("configuration rejects incomplete connection details", {
  expect_error(setConfig(), "ledger")
  expect_error(setConfig(ledger = "demo", branch = ""), "branch")
  expect_error(setConfig(ledger = "demo", timeout = 0), "timeout")
  expect_error(setConfig(ledger = "demo", baseUrl = "localhost:8090"),
               "http")
})

test_that("ledger references include the selected branch", {
  expect_equal(novaRush:::flureeLedgerRef("demo", "review"), "demo:review")
  expect_equal(novaRush:::flureeLedgerRef("demo:existing", "review"),
               "demo:existing")
})
