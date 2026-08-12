# as_jsonld_payload() is where every write verb converts its input, so it is tested
# directly rather than through the network.

payload <- novaRush:::as_jsonld_payload

demo_triples <- function() {
  tibble::tribble(
    ~subject, ~predicate, ~object, ~object_type,
    "ex:alice", "rdf:type",     "schema:Person", "uri",
    "ex:alice", "schema:name", "Alice",          "literal",
    "ex:alice", "schema:knows", "ex:bob",        "uri")
}

test_that("a triple table becomes a JSON-LD graph", {
  out <- payload(demo_triples())

  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(out[[1]][["@id"]], "ex:alice")
  expect_equal(out[[1]][["@type"]], "schema:Person")
  expect_equal(out[[1]][["schema:name"]], "Alice")
  expect_equal(out[[1]][["schema:knows"]], list(`@id` = "ex:bob"))
})

test_that("untyped subjects are not given an invented owl:Thing type", {
  # triples_to_jsonld() defaults @type to owl:Thing, which is fine for a document
  # but would write a triple the caller never asked for
  out <- payload(tibble::tibble(
    subject = "ex:alice", predicate = "schema:name",
    object = "Alice", object_type = "literal"))

  expect_null(out[[1]][["@type"]])
})

test_that("an explicit owl:Thing type is kept", {
  out <- payload(tibble::tibble(
    subject = "ex:a", predicate = "rdf:type",
    object = "owl:Thing", object_type = "uri"))

  expect_equal(out[[1]][["@type"]], "owl:Thing")
})

test_that("a tbl_graph is converted through its triples", {
  skip_if_not_installed("tidygraph")

  g <- semanticModelR::triples_to_tbl_graph(demo_triples())
  out <- payload(g)

  alice <- Filter(function(n) identical(n[["@id"]], "ex:alice"), out)[[1]]
  expect_equal(alice[["@type"]], "schema:Person")
  expect_equal(alice[["schema:name"]], "Alice")
})

test_that("JSON-LD is passed through, whether wrapped in @graph or not", {
  nodes <- list(list(`@id` = "ex:a", `schema:name` = "Ann"))

  expect_equal(payload(nodes), nodes)
  expect_equal(payload(list(`@graph` = nodes)), nodes)
})

test_that("a subject/predicate/object table can be classified on the way in", {
  out <- payload(
    tibble::tibble(subject = "ex:a", predicate = "schema:knows", object = "ex:b"),
    object_type = "infer")

  expect_equal(out[[1]][["schema:knows"]], list(`@id` = "ex:b"))
})

test_that("a plain data frame is refused with a pointer to the converters", {
  expect_error(payload(head(iris, 2)), "not a triple table")
  expect_error(payload(head(iris, 2)), "pivot_longer_with_type")
})

test_that("unsupported input is refused", {
  expect_error(payload(42), "must be a triple table")
  expect_error(payload("ex:a"), "must be a triple table")
})

test_that("write verbs reject a non-connection", {
  expect_error(fluree_insert(42, demo_triples()), "must be a fluree_connection")
  expect_error(fluree_upsert(42, demo_triples()), "must be a fluree_connection")
  expect_error(fluree_delete(42, "ex:a"), "must be a fluree_connection")
  expect_error(fluree_update(42, list()), "must be a fluree_connection")
  expect_error(fluree_update(test_connection(), "not a list"), "must be a list")
})

test_that("fluree_sparql validates its query argument", {
  expect_error(fluree_sparql(test_connection(), c("a", "b")), "single SPARQL string")
  expect_error(fluree_sparql(42, "SELECT * WHERE {}"), "must be a fluree_connection")
})
