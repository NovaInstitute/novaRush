# Fixtures use the shapes Fluree 4.1 actually returns. fromJSON is configured with
# no simplification, so every response is a nested list.

test_that("the array form of select becomes a tibble named from select", {
  # what {"select": ["?s","?name","?age"]} returns
  result <- list(list("ex:a", "Ann", 30), list("ex:b", "Bob", 40))

  out <- fluree_result_to_tibble(result, select = c("?s", "?name", "?age"))

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("s", "name", "age"))
  expect_equal(out$name, c("Ann", "Bob"))
  expect_equal(out$age, c(30, 40))
})

test_that("without select, array columns are named positionally", {
  out <- fluree_result_to_tibble(list(list("ex:a", "Ann")))
  expect_named(out, c("V1", "V2"))
})

test_that("the object form of select becomes one row per node", {
  # what {"select": {"?s": ["*"]}} returns
  result <- list(
    list(`@id` = "ex:a", `ex:name` = "Ann", `ex:age` = 30),
    list(`@id` = "ex:b", `ex:name` = "Bob", `ex:age` = 40))

  out <- fluree_result_to_tibble(result)

  expect_named(out, c("@id", "ex:name", "ex:age"))
  expect_equal(out$`@id`, c("ex:a", "ex:b"))
  expect_equal(out$`ex:age`, c(30, 40))
})

test_that("nodes with different predicates are unioned, with NA for absences", {
  result <- list(
    list(`@id` = "ex:a", `ex:name` = "Ann"),
    list(`@id` = "ex:b", `ex:age` = 40))

  out <- fluree_result_to_tibble(result)

  expect_named(out, c("@id", "ex:name", "ex:age"))
  expect_equal(out$`ex:name`, c("Ann", NA))
  expect_equal(out$`ex:age`, c(NA, 40))
})

test_that("non-scalar values become list-columns rather than being flattened", {
  result <- list(
    list(`@id` = "ex:a", `ex:knows` = list(`@id` = "ex:b")),
    list(`@id` = "ex:b", `ex:knows` = list(list(`@id` = "ex:c"), list(`@id` = "ex:d"))))

  out <- fluree_result_to_tibble(result)

  expect_true(is.list(out$`ex:knows`))
  expect_equal(out$`ex:knows`[[1]][["@id"]], "ex:b")
  expect_length(out$`ex:knows`[[2]], 2)
})

test_that("a single node returned unwrapped still gives one row", {
  out <- fluree_result_to_tibble(list(`@id` = "ex:a", `ex:name` = "Ann"))

  expect_equal(nrow(out), 1)
  expect_equal(out$`@id`, "ex:a")
})

test_that("an empty result keeps the selected column names", {
  out <- fluree_result_to_tibble(list(), select = c("?s", "?name"))

  expect_equal(nrow(out), 0)
  expect_named(out, c("s", "name"))
  expect_equal(nrow(fluree_result_to_tibble(NULL)), 0)
})

test_that("SPARQL bindings become a tibble with datatypes applied", {
  result <- list(
    head = list(vars = list("name", "age", "score", "ok", "when")),
    results = list(bindings = list(
      list(
        name  = list(type = "literal", value = "Ann"),
        age   = list(type = "literal", value = "30",
                     datatype = "http://www.w3.org/2001/XMLSchema#integer"),
        score = list(type = "literal", value = "1.5",
                     datatype = "http://www.w3.org/2001/XMLSchema#decimal"),
        ok    = list(type = "literal", value = "true",
                     datatype = "http://www.w3.org/2001/XMLSchema#boolean"),
        when  = list(type = "literal", value = "2024-02-27",
                     datatype = "http://www.w3.org/2001/XMLSchema#date")),
      list(
        name  = list(type = "literal", value = "Bob"),
        age   = list(type = "literal", value = "40",
                     datatype = "http://www.w3.org/2001/XMLSchema#integer"),
        score = list(type = "literal", value = "2.5",
                     datatype = "http://www.w3.org/2001/XMLSchema#decimal"),
        ok    = list(type = "literal", value = "false",
                     datatype = "http://www.w3.org/2001/XMLSchema#boolean"),
        when  = list(type = "literal", value = "2024-03-01",
                     datatype = "http://www.w3.org/2001/XMLSchema#date")))))

  out <- fluree_bindings_to_tibble(result)

  expect_named(out, c("name", "age", "score", "ok", "when"))
  expect_type(out$name, "character")
  expect_type(out$age, "integer")
  expect_type(out$score, "double")
  expect_type(out$ok, "logical")
  expect_s3_class(out$when, "Date")
  expect_equal(out$age, c(30L, 40L))
  expect_equal(out$ok, c(TRUE, FALSE))
})

test_that("SPARQL IRIs and untyped literals stay character", {
  result <- list(
    head = list(vars = list("s")),
    results = list(bindings = list(
      list(s = list(type = "uri", value = "http://example.org/a")))))

  expect_type(fluree_bindings_to_tibble(result)$s, "character")
})

test_that("an unbound SPARQL variable becomes NA", {
  result <- list(
    head = list(vars = list("name", "age")),
    results = list(bindings = list(
      list(name = list(type = "literal", value = "Ann")))))

  out <- fluree_bindings_to_tibble(result)
  expect_equal(out$name, "Ann")
  expect_true(is.na(out$age))
})

test_that("empty SPARQL results keep the projected columns", {
  out <- fluree_bindings_to_tibble(list(
    head = list(vars = list("name", "age")), results = list(bindings = list())))

  expect_equal(nrow(out), 0)
  expect_named(out, c("name", "age"))
})
