test_that("live Fluree named graphs isolate resources", {
  skip_if_no_live_fluree()
  config <- live_fluree_config()
  createLedger(config)
  db <- FlureeInstance$new(config)$connect()

  graph_a <- live_fluree_graph_iri("graph-a")
  graph_b <- live_fluree_graph_iri("graph-b")
  entity_a <- paste0(graph_a, "/entity")
  entity_b <- paste0(graph_b, "/entity")
  context <- list(test = "https://data.nova.org/test/schema/")

  db$upsertNamedGraph(list(
    "@context" = context,
    "@graph" = list(list("@id" = entity_a, "test:value" = "graph-a"))
  ), graph_a)

  db$upsertNamedGraph(list(
    "@context" = context,
    "@graph" = list(list("@id" = entity_b, "test:value" = "graph-b"))
  ), graph_b)

  scoped_query <- list(
    "@context" = context,
    select = list("?entity", "?value"),
    where = list(list("@id" = "?entity", "test:value" = "?value"))
  )
  result_a <- db$queryNamedGraph(scoped_query, graph_a)
  result_b <- db$queryNamedGraph(scoped_query, graph_b)
  values_a <- live_query_values(result_a)
  values_b <- live_query_values(result_b)

  expect_true(entity_a %in% values_a)
  expect_true("graph-a" %in% values_a)
  expect_false(entity_b %in% values_a)
  expect_false("graph-b" %in% values_a)

  expect_true(entity_b %in% values_b)
  expect_true("graph-b" %in% values_b)
  expect_false(entity_a %in% values_b)
  expect_false("graph-a" %in% values_b)
})

test_that("live Fluree named graph exact update retracts obsolete values", {
  skip_if_no_live_fluree()
  config <- live_fluree_config()
  createLedger(config)
  graph <- live_fluree_graph_iri("graph-update")
  entity <- paste0(graph, "/entity")
  property <- "https://data.nova.org/test/schema/value"
  old <- list("@id" = entity); old[[property]] <- "old"
  new <- list("@id" = entity); new[[property]] <- "new"
  upsertNamedGraph(old, graph, config)
  updateNamedGraph(delete = old, insert = new, graph = graph, config = config)
  result <- queryNamedGraph(list(
    select = list("?value"),
    where = list(setNames(list(entity, "?value"), c("@id", property)))
  ), graph, config)
  values <- live_query_values(result)
  expect_true("new" %in% values)
  expect_false("old" %in% values)
})
