test_that("live Fluree branch inherits and isolates knowledge", {
  skip_if_no_live_fluree()
  config <- live_fluree_config()
  createLedger(config)

  branch <- live_fluree_branch_name("candidate")
  marker <- paste0("https://data.nova.org/test/branch/", branch)
  candidate_only <- paste0(marker, "/candidate-only")
  context <- list(test = "https://data.nova.org/test/schema/")

  main <- FlureeInstance$new(updateConfig(config, list(branch = "main")))
  main$connect()
  main$upsert(list(
    "@context" = context,
    "@graph" = list(list("@id" = marker, "test:value" = "inherited"))
  ))$send()

  created <- main$createBranch(branch, from = "main")
  expect_true(is.list(created))
  expect_true(main$branchExists(branch))

  candidate <- FlureeInstance$new(updateConfig(config, list(branch = branch)))
  candidate$connect()
  inherited <- candidate$query(list(
    "@context" = context,
    select = list("?value"),
    where = list(list("@id" = marker, "test:value" = "?value"))
  ))$send()
  expect_true("inherited" %in% live_query_values(inherited))

  candidate$insert(list(
    "@context" = context,
    "@graph" = list(list(
      "@id" = candidate_only,
      "test:value" = "candidate-only"
    ))
  ))$send()

  candidate_result <- candidate$query(list(
    "@context" = context,
    select = list("?value"),
    where = list(list("@id" = candidate_only, "test:value" = "?value"))
  ))$send()
  main_result <- main$query(list(
    "@context" = context,
    select = list("?value"),
    where = list(list("@id" = candidate_only, "test:value" = "?value"))
  ))$send()
  expect_true("candidate-only" %in% live_query_values(candidate_result))
  expect_false("candidate-only" %in% live_query_values(main_result))
})
