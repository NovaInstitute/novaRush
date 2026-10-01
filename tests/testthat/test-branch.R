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

test_that("branchInfo and branchHead expose the selected commit", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  captured <- NULL
  info <- testthat::with_mocked_bindings(
    branchInfo(config, "review"),
    fluree_ledger_info = function(config, branch) {
      captured <<- branch
      list(branch = branch, commit = list(id = "fluree:commit:sha256:abc"))
    },
    .package = "novaRush"
  )
  expect_equal(captured, "review")
  expect_equal(info$branch, "review")
  expect_equal(testthat::with_mocked_bindings(
    branchHead(config, "review"),
    branchInfo = function(...) info,
    .package = "novaRush"
  ), "fluree:commit:sha256:abc")
})

test_that("branchHead rejects responses without a commit identity", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  expect_error(testthat::with_mocked_bindings(
    branchHead(config),
    branchInfo = function(...) list(commit = list()),
    .package = "novaRush"
  ), "commit identity")
})

test_that("mergeBranch checks the expected target and sends a merge", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  captured <- NULL
  result <- testthat::with_mocked_bindings(
    mergeBranch(config, "review-a", expected_target_head = "head-1"),
    branchHead = function(config, branch) "head-1",
    fluree_request = function(config, endpoint, method, body, operation,
                              allowStatus, returnResponse, write) {
      captured <<- list(endpoint = endpoint, method = method, body = body,
                        allowStatus = allowStatus, write = write)
      list(status = 200L, body = list(target = "main", commit = "head-2"))
    },
    .package = "novaRush"
  )
  expect_equal(captured$endpoint, "merge")
  expect_equal(captured$method, "POST")
  expect_equal(captured$body,
               list(ledger = "demo", source = "review-a", target = "main"))
  expect_equal(captured$allowStatus, 409L)
  expect_true(captured$write)
  expect_equal(result$commit, "head-2")
})

test_that("mergeBranch rejects stale targets before submitting", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  error <- tryCatch(testthat::with_mocked_bindings(
    mergeBranch(config, "review-a", expected_target_head = "head-1"),
    branchHead = function(...) "head-2",
    .package = "novaRush"
  ), error = identity)
  expect_s3_class(error, "fluree_branch_conflict")
  expect_equal(error$actual_target_head, "head-2")
})

test_that("mergeBranch turns HTTP 409 into a branch conflict", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  error <- tryCatch(testthat::with_mocked_bindings(
    mergeBranch(config, "review-a"),
    fluree_request = function(...) list(status = 409L, body = list(error = "conflict")),
    .package = "novaRush"
  ), error = identity)
  expect_s3_class(error, "fluree_branch_conflict")
  expect_equal(error$response$error, "conflict")
})
