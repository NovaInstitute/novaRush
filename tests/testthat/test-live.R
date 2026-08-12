# End-to-end tests against a real ledger. Skipped unless FLUREE_TEST_HOST is set,
# so the suite stays runnable with no server.
#
#   cd $(mktemp -d) && fluree init && fluree create scratch
#   fluree server start --listen-addr 127.0.0.1:8099
#   FLUREE_TEST_HOST=127.0.0.1:8099 FLUREE_TEST_LEDGER=scratch Rscript -e 'devtools::test()'
#   fluree server stop

live_connection <- function() {
  host <- Sys.getenv("FLUREE_TEST_HOST")
  skip_if(host == "", "set FLUREE_TEST_HOST to run the live tests")

  parts <- strsplit(host, ":", fixed = TRUE)[[1]]
  fluree_connect(
    parts[1],
    ledger  = Sys.getenv("FLUREE_TEST_LEDGER", "scratch"),
    port    = if (length(parts) > 1) as.integer(parts[2]) else NULL,
    context = c(ex = "http://example.org/", schema = "http://schema.org/"))
}

# A subject unique to every call. A time-based id collides between tests in the same
# run, which makes later assertions count triples from earlier ones.
live_subject <- function() {
  paste0("ex:test", gsub("[^A-Za-z0-9]", "", basename(tempfile(""))))
}

test_that("a query returns a tibble with values typed", {
  con <- live_connection()
  id <- live_subject()

  # datatype drives the JSON type, so age must arrive back as a number not a string
  fluree_insert(con, tibble::tribble(
    ~subject, ~predicate, ~object, ~object_type, ~datatype,
    id, "rdf:type",    "schema:Person", "uri",     NA,
    id, "schema:name", "Zoe",           "literal", "string",
    id, "schema:age",  "44",            "literal", "integer"))

  out <- fluree_query(con) |>
    fq_where(`@id` = id, `schema:name` = "?name", `schema:age` = "?age") |>
    fq_select(name, age) |>
    collect()

  expect_s3_class(out, "tbl_df")
  expect_equal(out$name, "Zoe")
  expect_false(is.character(out$age))
  expect_equal(as.numeric(out$age), 44)
})

test_that("a data frame pivoted to triples keeps its column types in the ledger", {
  con <- live_connection()
  df <- data.frame(id = live_subject(), n = 7L, x = 1.5, label = "seven")

  triples <- suppressMessages(semanticModelR::pivot_longer_with_type(df))
  fluree_insert(con, triples)

  out <- fluree_query(con) |>
    fq_where(`@id` = "?s", n = "?n", x = "?x") |>
    fq_select(n, x) |>
    collect()

  expect_gt(nrow(out), 0)
  expect_false(is.character(out$n))
  expect_false(is.character(out$x))
})

test_that("filters are applied by the server, not silently dropped", {
  con <- live_connection()
  id <- live_subject()

  fluree_insert(con, tibble::tribble(
    ~subject, ~predicate, ~object, ~object_type,
    paste0(id, "a"), "schema:name", "Alpha", "literal",
    paste0(id, "a"), "schema:age",  "20",    "literal",
    paste0(id, "b"), "schema:name", "Beta",  "literal",
    paste0(id, "b"), "schema:age",  "60",    "literal"))

  base <- fluree_query(con) |>
    fq_where(`@id` = "?s", `schema:name` = "?name", `schema:age` = "?age") |>
    fq_select(name, age)

  unfiltered <- collect(base)
  filtered <- collect(fq_filter(base, age > 50, startsWith(name, "B")))

  expect_true(nrow(filtered) < nrow(unfiltered))
  expect_true(all(as.numeric(filtered$age) > 50))
  expect_true(all(startsWith(filtered$name, "B")))
})

test_that("a triple table round-trips through insert and query", {
  con <- live_connection()
  id <- live_subject()

  triples <- tibble::tribble(
    ~subject, ~predicate, ~object, ~object_type,
    id, "rdf:type",     "schema:Person", "uri",
    id, "schema:name", "Round",          "literal",
    id, "schema:knows", "ex:a",          "uri")

  fluree_insert(con, triples)

  # crawl needs a variable to crawl from, so bind ?s and pick our subject out
  out <- fluree_query(con) |>
    fq_where(`@id` = "?s", `schema:name` = "?name") |> fq_crawl("?s") |> collect()

  row <- out[out$`@id` == id, ]
  expect_equal(nrow(row), 1)
  expect_equal(row$`schema:name`[[1]], "Round")
  expect_equal(row$`schema:knows`[[1]][["@id"]], "ex:a")
})

test_that("upsert is idempotent", {
  con <- live_connection()
  id <- live_subject()
  triples <- tibble::tibble(
    subject = id, predicate = "schema:name",
    object = "Once", object_type = "literal")

  fluree_upsert(con, triples)
  fluree_upsert(con, triples)
  fluree_upsert(con, triples)

  out <- fluree_query(con) |>
    fq_where(`@id` = id, `schema:name` = "?name") |> fq_select(name) |> collect()

  expect_equal(nrow(out), 1)
  expect_equal(out$name, "Once")
})

test_that("fluree_graph returns a tidygraph graph with edges and classes", {
  skip_if_not_installed("tidygraph")
  con <- live_connection()
  id <- live_subject()

  fluree_insert(con, tibble::tribble(
    ~subject, ~predicate, ~object, ~object_type,
    id, "rdf:type",     "schema:Person", "uri",
    id, "schema:name", "Graphy",         "literal",
    id, "schema:knows", paste0(id, "x"), "uri"))

  g <- fluree_graph(con, subject_type = "schema:Person")

  expect_s3_class(g, "tbl_graph")
  nodes <- tibble::as_tibble(g, active = "nodes")
  edges <- tibble::as_tibble(g, active = "edges")

  expect_true(id %in% nodes$name)
  expect_true("schema:knows" %in% edges$predicate)
  expect_true("schema:Person" %in% unlist(nodes$rdf_type))
  expect_equal(nrow(semanticModelR::rdf_summary(g)), 1)
})

test_that("SPARQL returns a tibble with datatypes applied", {
  con <- live_connection()

  out <- fluree_sparql(con, "
    PREFIX ex: <http://example.org/>
    SELECT ?name ?age WHERE { ?s ex:name ?name ; ex:age ?age . } ORDER BY ?name")

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("name", "age") %in% names(out)))
  if (nrow(out) > 0) expect_false(is.character(out$age))
})

test_that("fq_at is honoured rather than ignored", {
  con <- live_connection()
  id <- live_subject()

  fluree_insert(con, tibble::tibble(
    subject = id, predicate = "schema:name",
    object = "Later", object_type = "literal"))

  now <- fluree_query(con) |>
    fq_where(`@id` = id, `schema:name` = "?name") |> fq_select(name) |> collect()
  # t=1 predates this subject, so an honoured time filter returns nothing
  before <- fluree_query(con) |>
    fq_where(`@id` = id, `schema:name` = "?name") |> fq_select(name) |>
    fq_at(1) |> collect()

  expect_equal(nrow(now), 1)
  expect_equal(nrow(before), 0)
})

test_that("history returns the commit log as a tibble", {
  con <- live_connection()
  out <- fluree_history(con, limit = 3)

  expect_s3_class(out, "tbl_df")
  expect_gt(nrow(out), 0)
})

test_that("delete retracts a subject", {
  con <- live_connection()
  id <- live_subject()

  fluree_insert(con, tibble::tibble(
    subject = id, predicate = "schema:name",
    object = "Doomed", object_type = "literal"))

  present <- fluree_query(con) |>
    fq_where(`@id` = id, `schema:name` = "?name") |> fq_select(name) |> collect()
  expect_equal(nrow(present), 1)

  fluree_delete(con, id)

  gone <- fluree_query(con) |>
    fq_where(`@id` = id, `schema:name` = "?name") |> fq_select(name) |> collect()
  expect_equal(nrow(gone), 0)
})
