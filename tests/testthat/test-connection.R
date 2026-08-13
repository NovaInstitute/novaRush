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

# base_url is an alternative to host/port, not an addition. checkConfig() only knew
# about host, so a base_url-only connection could never connect - which the live
# suite caught and the offline suite did not, because check = FALSE skips connect().
test_that("a base_url-only config passes connection validation", {
  instance <- FlureeInstance$new(list(baseUrl = "http://fluree.test", ledger = "demo"))

  expect_silent(instance$checkConfig(instance$config, TRUE))
  expect_error(
    FlureeInstance$new(list(ledger = "demo"))$checkConfig(list(ledger = "demo"), TRUE),
    "Either `host` or `baseUrl` is required")
})

test_that("a base_url-only connection prints its address rather than ?", {
  con <- fluree_connect(base_url = "http://fluree.test:8090", ledger = "demo",
                        check = FALSE)

  expect_output(print(con), "http://fluree.test:8090", fixed = TRUE)
  expect_no_match(format(con), "?", fixed = TRUE)
})
