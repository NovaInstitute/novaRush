test_that("request tracing is disabled by default", {
  config <- setConfig(baseUrl = "http://fluree.test", ledger = "demo")
  local_options(novaRush.trace = "off")
  testthat::local_mocked_bindings(
    .fluree_perform_request = function(...) {
      list(status = 200L, text = '{"status":"ok"}')
    },
    .package = "novaRush"
  )

  expect_silent(novaRush:::fluree_request(config, "health"))
})

test_that("summary tracing reports request and response metadata", {
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", apiKey = "secret-token"
  )
  local_options(novaRush.trace = "summary")
  testthat::local_mocked_bindings(
    .fluree_perform_request = function(...) {
      list(status = 200L, text = '{"status":"ok"}')
    },
    .package = "novaRush"
  )

  messages <- capture.output(
    novaRush:::fluree_request(
      config, "query", method = "POST", body = list(select = "?s"),
      operation = "trace query"
    ),
    type = "message"
  )
  messages <- paste(messages, collapse = "\n")
  expect_match(messages, "trace query.*POST.*request.*bytes")
  expect_match(messages, "trace query.*HTTP 200.*response.*bytes")
})

test_that("payload tracing writes exact bodies without authorization headers", {
  directory <- tempfile("novarush-trace-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  config <- setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", apiKey = "secret-token"
  )
  local_options(novaRush.trace = "payloads", novaRush.trace_dir = directory)
  testthat::local_mocked_bindings(
    .fluree_perform_request = function(...) {
      list(status = 200L, text = '{"status":"ok","rows":2}')
    },
    .package = "novaRush"
  )

  invisible(capture.output(
    novaRush:::fluree_request(
      config, "upsert", method = "POST",
      body = list("@id" = "https://example.org/a", value = 42),
      operation = "trace payload"
    ),
    type = "message"
  ))
  requests <- list.files(directory, "[.]request[.]json$", full.names = TRUE)
  responses <- list.files(directory, "[.]response[.]json$", full.names = TRUE)
  metadata <- list.files(directory, "[.]meta[.]json$", full.names = TRUE)
  expect_length(requests, 1L)
  expect_length(responses, 1L)
  expect_length(metadata, 1L)
  expect_identical(readLines(responses), '{"status":"ok","rows":2}')
  request <- paste(readLines(requests), collapse = "\n")
  all_files <- paste(unlist(lapply(
    c(requests, responses, metadata), readLines, warn = FALSE
  )), collapse = "\n")
  expect_true(jsonlite::validate(request))
  expect_match(request, "https://example.org/a", fixed = TRUE)
  expect_no_match(all_files, "secret-token", fixed = TRUE)
  expect_no_match(all_files, "Authorization", fixed = TRUE)
})
