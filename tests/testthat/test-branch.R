test_that("branch names are validated before requests", {
  expect_silent(novaRush:::validateBranchName("ai-run_1.2"))
  expect_error(novaRush:::validateBranchName(""), "must start")
  expect_error(novaRush:::validateBranchName("run one"), "must start")
  expect_error(novaRush:::validateBranchName(":main"), "must start")
})

test_that("listBranches targets the ledger branch endpoint", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  result <- testthat::with_mocked_bindings(
    listBranches(config),
    fluree_request = function(config, endpoint, method, operation) {
      captured <<- list(endpoint = endpoint, method = method,
                        operation = operation)
      list(list(branch = "main", ledger_id = "demo:main", t = 2L))
    },
    .package = "novaRush"
  )
  expect_equal(captured$endpoint, "branch/demo")
  expect_equal(captured$method, "GET")
  expect_equal(result[[1]]$branch, "main")
})

test_that("branchExists interprets branch records", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  expect_true(testthat::with_mocked_bindings(
    branchExists(config, "review"),
    listBranches = function(...) list(
      list(branch = "main", ledger_id = "demo:main"),
      list(branch = "review", ledger_id = "demo:review")
    ),
    .package = "novaRush"
  ))
  expect_false(testthat::with_mocked_bindings(
    branchExists(config, "missing"),
    listBranches = function(...) list(list(branch = "main")),
    .package = "novaRush"
  ))
})

test_that("createBranch sends source and destination branches", {
  captured <- NULL
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  result <- testthat::with_mocked_bindings(
    createBranch(config, "candidate", from = "main"),
    branchExists = function(...) FALSE,
    fluree_request = function(config, endpoint, method, body, operation,
                              allowStatus, returnResponse, write) {
      captured <<- list(endpoint = endpoint, method = method, body = body,
                        allowStatus = allowStatus, write = write)
      list(status = 200L, body = list(
        ledger_id = "demo:candidate", branch = "candidate", t = 0L
      ))
    },
    .package = "novaRush"
  )
  expect_equal(captured$endpoint, "branch")
  expect_equal(captured$method, "POST")
  expect_equal(captured$body,
               list(ledger = "demo", branch = "candidate", from = "main"))
  expect_equal(captured$allowStatus, 409L)
  expect_true(captured$write)
  expect_equal(result$ledger_id, "demo:candidate")
})

test_that("createBranch is idempotent", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  result <- testthat::with_mocked_bindings(
    createBranch(config, "candidate"),
    branchExists = function(...) TRUE,
    .package = "novaRush"
  )
  expect_false(result$created)
  expect_true(result$exists)
})

test_that("R6 branch methods use the functional API", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  db <- FlureeInstance$new(config)
  branches <- testthat::with_mocked_bindings(
    db$listBranches(),
    listBranches = function(config) list(list(branch = "main")),
    .package = "novaRush"
  )
  expect_equal(branches[[1]]$branch, "main")
  expect_true(testthat::with_mocked_bindings(
    db$branchExists("main"),
    branchExists = function(config, branch) identical(branch, "main"),
    .package = "novaRush"
  ))
})
