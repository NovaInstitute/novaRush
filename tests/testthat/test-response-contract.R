# fluree_result_to_tibble() assumes every response arrives as a predictable nested
# list - see the header of R/response.R. That holds only while every parse in the
# package goes through getDefaultFromJSONargs(). When the request layer was
# centralised into fluree_request(), that assumption became a cross-file contract
# with nothing pinning it, and breaking it would not error: results would simply
# come back the wrong shape.

test_that("getDefaultFromJSONargs disables every form of simplification", {
  args <- getDefaultFromJSONargs()

  expect_false(args$simplifyVector)
  expect_false(args$simplifyDataFrame)
  expect_false(args$simplifyMatrix)
  expect_false(args$flatten)
})

# R/ is only there when the tests run against the source tree, not the installed
# package, so the two source-scanning guards below skip under R CMD check.
source_dir <- function() {
  d <- test_path("../../R")
  if (dir.exists(d)) d else NA_character_
}

test_that("fluree_request parses response bodies with those settings", {
  skip_if(is.na(source_dir()), "source tree not available")
  src <- paste(readLines(file.path(source_dir(), "requestHandling.R"), warn = FALSE),
               collapse = "\n")

  expect_match(src, "getDefaultFromJSONargs\\(\\)", fixed = FALSE)
  # and never with a bare fromJSON() that would take jsonlite's simplifying defaults
  expect_no_match(src, "fromJSON\\(\\s*(txt\\s*=\\s*)?responseText\\s*\\)")
})

test_that("no response parse in the package silently takes jsonlite's defaults", {
  skip_if(is.na(source_dir()), "source tree not available")
  files <- list.files(source_dir(), pattern = "[.]R$", full.names = TRUE)

  # signTransaction() reads a stored config out of an env var rather than a Fluree
  # response, and relies on simplification so privateKey is a string not a list.
  # It is the only exemption; anything else that turns up here is a real defect.
  exempt <- "config <- fromJSON(Sys.getenv(\"config\"))"

  offenders <- Filter(function(f) {
    lines <- readLines(f, warn = FALSE)
    calls <- grep("fromJSON\\(", lines, value = TRUE)
    calls <- calls[!grepl("^\\s*#", calls)]           # comments and roxygen
    calls <- calls[!grepl("getDefaultFromJSONargs", calls)]
    calls <- calls[!grepl(exempt, calls, fixed = TRUE)]
    # a do.call() spreading the defaults puts them on a neighbouring line
    any(vapply(calls, function(c) !grepl("do.call", c, fixed = TRUE), logical(1)))
  }, files)

  expect_equal(basename(offenders), character(0))
})

test_that("an unsimplified node response still becomes one row per node", {
  # what fromJSON() returns for two nodes when simplification is off: a list of
  # named lists. With simplifyDataFrame = TRUE this arrives as a data frame
  # instead, and the columns below come out nested or missing.
  rows <- list(
    list(`@id` = "ex:alice", name = "Alice", age = 30L),
    list(`@id` = "ex:bob",   name = "Bob",   age = 40L))

  out <- fluree_result_to_tibble(rows)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)
  expect_equal(out$name, c("Alice", "Bob"))
  expect_equal(out$age, c(30L, 40L))
  expect_type(out$age, "integer")
})

test_that("an unsimplified array response keeps its positional columns", {
  rows <- list(list("ex:alice", "Alice", 30L), list("ex:bob", "Bob", 40L))

  out <- fluree_result_to_tibble(rows, select = c("?s", "?name", "?age"))

  expect_equal(names(out), c("s", "name", "age"))
  expect_equal(nrow(out), 2L)
  expect_equal(out$age, c(30L, 40L))
})

test_that("a repeated property stays a list-column rather than being flattened", {
  # the case simplification would quietly destroy: one node with two values for
  # the same predicate
  rows <- list(
    list(`@id` = "ex:alice", tag = list("a", "b")),
    list(`@id` = "ex:bob",   tag = "c"))

  out <- fluree_result_to_tibble(rows)

  expect_equal(nrow(out), 2L)
  expect_type(out$tag, "list")
  expect_equal(out$tag[[1]], list("a", "b"))
})
