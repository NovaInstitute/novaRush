r6_connected_instance <- function(branch = "review") {
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", branch = branch
  )
  db <- FlureeInstance$new(config)
  db$connected <- TRUE
  db
}

test_that("R6 query uses the shared request executor", {
  captured <- NULL
  db <- r6_connected_instance()
  instance <- db$query(
    list(select = list("?s"), where = list(list("@id" = "?s")))
  )
  result <- testthat::with_mocked_bindings(
    instance$send(),
    fluree_request = function(config, endpoint, method, body, operation) {
      captured <<- body
      list(list("https://example.org/a"))
    },
    .package = "novaRush"
  )
  expect_equal(captured$from, "demo:review")
  expect_length(result, 1L)
})

test_that("R6 transactions retain their selected endpoint", {
  calls <- list()
  db <- r6_connected_instance()
  insert <- db$insert(list("@graph" = list(list("@id" = "ex:a"))))
  update <- db$update(list(
    where = list(list("@id" = "ex:a", "ex:name" = "?name")),
    delete = list(list("@id" = "ex:a", "ex:name" = "?name"))
  ))
  testthat::with_mocked_bindings(
    {
      insert$send()
      update$send()
    },
    fluree_request = function(config, endpoint, method, body, query,
                              contentType, operation) {
      calls[[length(calls) + 1L]] <<- list(endpoint = endpoint, query = query)
      list(t = length(calls))
    },
    .package = "novaRush"
  )
  expect_equal(vapply(calls, `[[`, character(1), "endpoint"),
               c("insert", "update"))
  expect_true(all(vapply(calls, function(x) x$query$ledger == "demo:review",
                         logical(1))))
})
