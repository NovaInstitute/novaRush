test_that("connect checks server health and the configured ledger", {
  calls <- character()
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", branch = "review"
  )
  db <- FlureeInstance$new(config)
  testthat::local_mocked_bindings(
    fluree_health_check = function(config) {
      calls <<- c(calls, "health")
      list(status = "ok")
    },
    fluree_ledger_info = function(config, branch = config$branch) {
      calls <<- c(calls, paste0("ledger:", branch))
      list(ledger_id = novaRush:::flureeLedgerRef(config$ledger, branch))
    },
    .package = "novaRush"
  )
  expect_identical(db$connect(), db)
  expect_true(db$connected)
  expect_equal(calls, c("health", "ledger:review"))
})

test_that("a failed connection does not leave the instance connected", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  db <- FlureeInstance$new(config)
  testthat::local_mocked_bindings(
    fluree_health_check = function(config) stop("not healthy"),
    .package = "novaRush"
  )
  expect_error(db$connect(), "not healthy")
  expect_false(db$connected)
})
