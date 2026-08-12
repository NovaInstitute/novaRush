test_that("live Fluree core workflow completes", {
  skip_if_no_live_fluree()
  config <- live_fluree_config()

  health <- novaRush:::fluree_health_check(config)
  expect_identical(health$status, "ok")

  created <- createLedger(config)
  expect_true(is.list(created))

  db <- FlureeInstance$new(config)$connect()
  expect_true(db$connected)

  run_id <- paste0(
    "run-", format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), "-",
    Sys.getpid()
  )
  run_id <- gsub("[^A-Za-z0-9-]", "", run_id)
  entity_iri <- paste0("https://data.nova.org/test/novarush/", run_id)
  context <- list(test = "https://data.nova.org/test/schema/")

  inserted <- db$insert(list(
    "@context" = context,
    "@graph" = list(list(
      "@id" = entity_iri,
      "@type" = "test:LiveCoreEntity",
      "test:name" = "inserted",
      "test:stage" = 1L
    ))
  ))$send()
  expect_true(is.list(inserted))

  query_property <- function(predicate, variable) {
    pattern <- list("@id" = entity_iri)
    pattern[[predicate]] <- variable
    db$query(list(
      "@context" = context,
      select = list(variable),
      where = list(pattern)
    ))$send()
  }

  expect_true("inserted" %in%
                live_query_values(query_property("test:name", "?name")))

  upserted <- db$upsert(list(
    "@context" = context,
    "@graph" = list(list(
      "@id" = entity_iri,
      "test:name" = "upserted"
    ))
  ))$send()
  expect_true(is.list(upserted))
  name_values <- live_query_values(query_property("test:name", "?name"))
  expect_true("upserted" %in% name_values)
  expect_false("inserted" %in% name_values)

  updated <- db$update(list(
    "@context" = context,
    where = list(list(
      "@id" = entity_iri,
      "test:stage" = "?oldStage"
    )),
    delete = list(list(
      "@id" = entity_iri,
      "test:stage" = "?oldStage"
    )),
    insert = list(list(
      "@id" = entity_iri,
      "test:stage" = 2L
    ))
  ))$send()
  expect_true(is.list(updated))
  expect_true("2" %in%
                live_query_values(query_property("test:stage", "?stage")))

  functional_query <- Query(
    config = config,
    query = list(
      "@context" = context,
      select = list("?name"),
      where = list(list("@id" = entity_iri, "test:name" = "?name"))
    )
  )
  expect_true(jsonlite::validate(functional_query))
  expect_true("upserted" %in%
                live_query_values(jsonlite::fromJSON(
                  functional_query, simplifyVector = FALSE
                )))
})
