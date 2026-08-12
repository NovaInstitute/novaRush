test_that("a connection carries host, ledger and context", {
  con <- test_connection()

  expect_s3_class(con, "fluree_connection")
  expect_true(is_fluree_connection(con))
  expect_equal(fluree_ledger(con), "novaRush/test")
  expect_equal(fluree_context(con)$schema, "http://schema.org/")
  expect_output(print(con), "novaRush/test")
})

test_that("fluree_connect insists on a ledger and on a key when signing", {
  expect_error(fluree_connect("localhost"), "`ledger` is required")
  # must not reach for the keyring, which can prompt and block a script
  expect_error(
    fluree_connect("localhost", ledger = "a", sign = TRUE, private_key = NULL),
    "needs `private_key`")
})

test_that("the verbs build the query Fluree expects", {
  body <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s", `@type` = "schema:Person", `schema:name` = "?name") |>
    fq_where(`@id` = "?s", `schema:age` = "?age") |>
    fq_select(s, name, age) |>
    fq_order_by(desc(age)) |>
    fq_limit(10) |>
    fq_offset(2) |>
    fq_reasoning("rdfs") |>
    fluree_query_body()

  expect_equal(body$from, "novaRush/test")
  expect_equal(body$select, list("?s", "?name", "?age"))
  expect_equal(body$orderBy, "(desc ?age)")
  expect_equal(body$limit, 10L)
  expect_equal(body$offset, 2L)
  expect_equal(body$reasoning, "rdfs")
  expect_length(body$where, 2)
  expect_equal(body$where[[1]][["@type"]], "schema:Person")
})

test_that("where stays a JSON array even with several entries", {
  # empty names on the assembled list would make jsonlite emit an object
  body <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s") |>
    fq_where(`@id` = "?t") |>
    fq_select(s) |>
    fq_filter(age > 1) |>
    fluree_query_body()

  expect_null(names(body$where))

  json <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s") |> fq_where(`@id` = "?t") |>
    fq_select(s) |> fq_filter(age > 1) |>
    fluree_query_json(pretty = FALSE)
  expect_match(json, '"where":\\[\\{')
})

test_that("filters go inside where, not at the top level", {
  # a top-level filter field is silently ignored by Fluree v4.1
  body <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s", `schema:age` = "?age") |>
    fq_select(age) |>
    fq_filter(age > 35) |>
    fluree_query_body()

  expect_null(body$filter)
  expect_length(body$where, 2)
  expect_equal(body$where[[2]], list("filter", "(> ?age 35)"))
})

test_that("comparison, boolean and arithmetic operators translate", {
  expect_equal(filter_expr(age > 35), "(> ?age 35)")
  expect_equal(filter_expr(age >= 40), "(>= ?age 40)")
  expect_equal(filter_expr(age < 45), "(< ?age 45)")
  expect_equal(filter_expr(age <= 30), "(<= ?age 30)")
  expect_equal(filter_expr(age == 40), "(= ?age 40)")
  expect_equal(filter_expr(age != 40), "(!= ?age 40)")
  expect_equal(filter_expr(age >= 40 & age < 45), "(and (>= ?age 40) (< ?age 45))")
  expect_equal(filter_expr(age == 30 | age == 50), "(or (= ?age 30) (= ?age 50))")
  expect_equal(filter_expr(!(age == 40)), "(not (= ?age 40))")
  expect_equal(filter_expr(age + 1 > 41), "(> (+ ?age 1) 41)")
  expect_equal(filter_expr(age - 5 > 40), "(> (- ?age 5) 40)")
  expect_equal(filter_expr(age * 2 > 90), "(> (* ?age 2) 90)")
  expect_equal(filter_expr(age / 2 >= 20), "(>= (/ ?age 2) 20)")
})

test_that("string and membership functions translate, with argument order fixed", {
  expect_equal(filter_expr(startsWith(name, "B")), '(strStarts ?name "B")')
  expect_equal(filter_expr(endsWith(name, "d")), '(strEnds ?name "d")')
  expect_equal(filter_expr(grepl("^A", name)), '(regex ?name "^A")')
  expect_equal(filter_expr(nchar(name) > 2), "(> (strlen ?name) 2)")
  expect_equal(filter_expr(age %in% c(30, 50)), "(in ?age [30 50])")
})

test_that("missingness translates to bound, without a double negation", {
  expect_equal(filter_expr(is.na(age)), "(not (bound ?age))")
  expect_equal(filter_expr(!is.na(age)), "(bound ?age)")
})

test_that("literals are quoted and escaped", {
  expect_equal(filter_expr(name == "Ann"), '(= ?name "Ann")')
  expect_equal(filter_expr(name == 'say "hi"'), '(= ?name "say \\"hi\\"")')
  expect_equal(filter_expr(flag == TRUE), "(= ?flag true)")
  expect_equal(filter_expr(flag == FALSE), "(= ?flag false)")
})

test_that("unknown functions pass through as Fluree functions", {
  expect_equal(filter_expr(strStarts(name, "B")), '(strStarts ?name "B")')
  expect_equal(filter_expr(coalesce(age, 0)), "(coalesce ?age 0)")
})

test_that("values are injected with !! and strings pass through verbatim", {
  threshold <- 35
  expect_equal(filter_expr(age > !!threshold), "(> ?age 35)")
  expect_equal(filter_expr("(> ?age 35)"), "(> ?age 35)")
})

test_that("several filters accumulate as separate where entries", {
  expect_equal(filter_expr(age > 35, startsWith(name, "A")),
               c("(> ?age 35)", '(strStarts ?name "A")'))
})

test_that("fq_crawl builds the object form of select", {
  body <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s", `?p` = "?o") |>
    fq_crawl("?s") |>
    fluree_query_body()

  expect_equal(body$select, list(`?s` = list("*")))
})

test_that("select accepts bare names and strings, adding the question mark", {
  body <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s") |> fq_select(s, "name", "?age") |> fluree_query_body()

  expect_equal(body$select, list("?s", "?name", "?age"))
})

test_that("fq_at puts the time on the ledger name", {
  body <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s") |> fq_select(s) |> fq_at(3) |> fluree_query_body()

  expect_equal(body$from, "novaRush/test@t:3")
})

test_that("fq_context adds to the connection context rather than replacing it", {
  body <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s") |> fq_select(s) |>
    fq_context(ex = "http://example.org/") |>
    fluree_query_body()

  expect_equal(body[["@context"]]$schema, "http://schema.org/")
  expect_equal(body[["@context"]]$ex, "http://example.org/")
})

test_that("verbs copy rather than mutate, so a base query can be reused", {
  base <- fluree_query(test_connection()) |> fq_where(`@id` = "?s") |> fq_select(s)
  limited <- fq_limit(base, 5)

  expect_null(fluree_query_body(base)$limit)
  expect_equal(fluree_query_body(limited)$limit, 5L)
})

test_that("an incomplete query is refused with a useful message", {
  con <- test_connection()
  expect_error(fluree_query_body(fq_select(fluree_query(con), s)), "needs at least one pattern")
  expect_error(fluree_query_body(fq_where(fluree_query(con), `@id` = "?s")), "needs a select")
  expect_error(fq_where(fluree_query(con), "?s"), "must be named")
  expect_error(fluree_query(42), "must be a fluree_connection")
  expect_error(fq_limit(42, 1), "Expected a fluree_query")
})

test_that("show_query prints the assembled query", {
  q <- fluree_query(test_connection()) |>
    fq_where(`@id` = "?s", `schema:age` = "?age") |> fq_select(age) |> fq_filter(age > 35)

  expect_output(show_query(q), '"\\(> \\?age 35\\)"')
  expect_output(print(q), "fluree_query")
  expect_match(format(q), "1 pattern")
})
