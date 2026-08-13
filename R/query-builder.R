# A JSON-LD Query assembled step by step. Every verb returns a modified copy, so a
# partly built query can be reused as the base for several others.

#' Start a JSON-LD Query
#'
#' @description
#' Creates an empty query against a connection, to be built up with the `fq_*`
#' verbs and run with [collect()]. Fluree v4 calls its native JSON query language
#' **JSON-LD Query**; "FlureeQL" was the v2/v3 name for it.
#'
#' Nothing is sent until you call [collect()]. Use [show_query()] to see the
#' JSON-LD Query that will be sent.
#'
#' @param con A `fluree_connection` from [fluree_connect()].
#'
#' @returns A `fluree_query`.
#' @seealso [fq_where()], [fq_select()], [fq_filter()], [collect.fluree_query()]
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query(con) |>
#'   fq_where(`@id` = "?s", `schema:name` = "?name") |>
#'   fq_select(s, name) |>
#'   collect()
#' }
fluree_query <- function(con) {
  check_connection(con)
  structure(
    list(con = con, patterns = list(), filters = list(), opts = list()),
    class = "fluree_query"
  )
}

#' Is this a Fluree query?
#'
#' @param x An object.
#' @returns `TRUE` or `FALSE`.
#' @export
#'
#' @examples
#' is_fluree_query(1)
is_fluree_query <- function(x) inherits(x, "fluree_query")

check_query <- function(q) {
  if (!is_fluree_query(q)) {
    stop("Expected a fluree_query from fluree_query(), not ", class(q)[1], ".",
         call. = FALSE)
  }
  invisible(q)
}

#' Add a node pattern to the where clause
#'
#' @description
#' Each call appends one node pattern. Names are predicates (or JSON-LD keywords
#' such as `@id` and `@type`) and values are either variables like `"?name"` or
#' constants. Call it several times to match several nodes.
#'
#' @param q A `fluree_query`.
#' @param ... Named predicate/value pairs forming one pattern. Alternatively a
#'   single unnamed list, used as a pattern verbatim.
#'
#' @returns The query, with the pattern appended.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query(con) |>
#'   fq_where(`@id` = "?s", `@type` = "schema:Person", `schema:name` = "?name") |>
#'   fq_where(`@id` = "?s", `schema:age` = "?age")
#' }
fq_where <- function(q, ...) {
  check_query(q)
  args <- list(...)

  pattern <- if (length(args) == 1 && is.null(names(args)) && is.list(args[[1]])) {
    args[[1]]
  } else {
    nms <- names(args)
    if (length(args) == 0 || is.null(nms) || any(!nzchar(nms)) || anyNA(nms)) {
      stop("Every argument to fq_where() must be named, e.g. `@id` = \"?s\".",
           call. = FALSE)
    }
    args
  }

  q$patterns <- c(q$patterns, list(pattern))
  q
}

#' Choose what to return
#'
#' @description
#' `fq_select()` builds the array form of `select`, one column per variable. Bare
#' names are turned into query variables, so `fq_select(name, age)` selects
#' `?name` and `?age`. Aggregate expressions can be passed as strings, e.g.
#' `fq_select("(count ?age)")`.
#'
#' `fq_crawl()` builds the object form instead, which returns whole nodes rather
#' than columns - the JSON-LD graph crawl.
#'
#' @param q A `fluree_query`.
#' @param ... Variables to select, as bare names or strings.
#' @param var The variable to crawl from, e.g. `"?s"`.
#' @param props Properties to include. `"*"` (the default) takes all of them.
#'
#' @returns The query, with `select` set.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query(con) |> fq_where(`@id` = "?s", `schema:name` = "?name") |>
#'   fq_select(s, name)
#'
#' # whole nodes instead of columns
#' fluree_query(con) |> fq_where(`@id` = "?s", `schema:name` = "?name") |>
#'   fq_crawl("?s")
#' }
fq_select <- function(q, ...) {
  check_query(q)
  vars <- vapply(rlang::enquos(...), as_query_variable, character(1))
  if (length(vars) == 0) stop("fq_select() needs at least one variable.", call. = FALSE)
  q$opts$select <- unname(vars)
  q$opts$crawl <- NULL
  q
}

#' @rdname fq_select
#' @export
fq_crawl <- function(q, var, props = "*") {
  check_query(q)
  q$opts$select <- NULL
  q$opts$crawl <- list(var = ensure_qmark(var), props = as.list(props))
  q
}

#' Filter results
#'
#' @description
#' Filters are written as ordinary R expressions and translated to the prefix
#' s-expressions Fluree expects, then placed inside the `where` clause where Fluree
#' looks for them.
#'
#' This translation is not cosmetic. A top-level `filter` field is **silently
#' ignored** by Fluree v4.1, and an infix expression such as `"?age > 35"` parses but
#' matches nothing - both give you a wrong answer with no error. Writing
#' `fq_filter(age > 35)` avoids both traps.
#'
#' Supported: comparisons `> >= < <= == !=`, boolean `& | !`, arithmetic
#' `+ - * /`, `%in%`, and these translations:
#'
#' | R | Fluree |
#' |---|---|
#' | `startsWith(name, "B")` | `(strStarts ?name "B")` |
#' | `endsWith(name, "d")` | `(strEnds ?name "d")` |
#' | `grepl("^A", name)` | `(regex ?name "^A")` |
#' | `nchar(name)` | `(strlen ?name)` |
#' | `is.na(age)` | `(not (bound ?age))` |
#' | `age %in% c(30, 50)` | `(in ?age [30 50])` |
#'
#' Bare names become query variables. To use a value from the calling environment,
#' unquote it with `!!`. Any other function name is passed through unchanged, so
#' Fluree functions can be called directly. A single character argument is used
#' verbatim as an escape hatch.
#'
#' @param q A `fluree_query`.
#' @param ... Filter expressions. Several are combined with `and`.
#'
#' @returns The query, with the filters appended.
#' @export
#'
#' @examples
#' \dontrun{
#' threshold <- 35
#' fluree_query(con) |>
#'   fq_where(`@id` = "?s", `schema:age` = "?age") |>
#'   fq_filter(age > !!threshold, !is.na(age))
#'
#' # escape hatch: pass the s-expression yourself
#' fluree_query(con) |> fq_filter("(> ?age 35)")
#' }
fq_filter <- function(q, ...) {
  check_query(q)
  quos <- rlang::enquos(...)
  if (length(quos) == 0) return(q)

  exprs <- lapply(quos, function(qu) {
    e <- rlang::quo_get_expr(qu)
    if (is.character(e) && length(e) == 1) e else translate_filter(e)
  })

  # enquos() gives empty-string names; left in place they make `where` serialise as
  # a JSON object instead of an array
  q$filters <- unname(c(q$filters, exprs))
  q
}

#' Order, group, limit and offset results
#'
#' @description
#' `fq_order_by()` accepts bare names, and `desc()` for descending order.
#' `fq_group_by()` groups for the aggregate functions used in [fq_select()].
#'
#' @param q A `fluree_query`.
#' @param ... Variables, as bare names or strings. `desc(x)` sorts descending.
#' @param n Number of rows.
#'
#' @returns The query, modified.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query(con) |>
#'   fq_where(`@id` = "?s", `schema:age` = "?age") |>
#'   fq_select(s, age) |>
#'   fq_order_by(desc(age)) |>
#'   fq_limit(10)
#' }
fq_order_by <- function(q, ...) {
  check_query(q)
  q$opts$orderBy <- unname(vapply(rlang::enquos(...), as_order_term, character(1)))
  q
}

#' @rdname fq_order_by
#' @export
fq_group_by <- function(q, ...) {
  check_query(q)
  q$opts$groupBy <- unname(vapply(rlang::enquos(...), as_query_variable, character(1)))
  q
}

#' @rdname fq_order_by
#' @export
fq_limit <- function(q, n) {
  check_query(q)
  q$opts$limit <- as.integer(n)
  q
}

#' @rdname fq_order_by
#' @export
fq_offset <- function(q, n) {
  check_query(q)
  q$opts$offset <- as.integer(n)
  q
}

#' Add prefixes, reasoning or a point in time
#'
#' @description
#' `fq_context()` adds JSON-LD prefixes for this query on top of the connection's
#' default context. `fq_reasoning()` requests an inference level. `fq_at()` runs the
#' query against an earlier state of the ledger.
#'
#' @param q A `fluree_query`.
#' @param ... Named prefixes, e.g. `ex = "http://example.org/"`.
#' @param mode One of `"rdfs"`, `"owl2ql"`, `"owl2rl"`, `"datalog"`, `"none"`.
#' @param t A transaction number, a commit hash, or an ISO-8601 timestamp.
#'
#' @returns The query, modified.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query(con) |>
#'   fq_context(ex = "http://example.org/") |>
#'   fq_where(`@id` = "?s", `ex:name` = "?name") |>
#'   fq_reasoning("rdfs") |>
#'   fq_at(3)
#' }
fq_context <- function(q, ...) {
  check_query(q)
  ctx <- list(...)
  if (length(ctx) == 1 && is.null(names(ctx)) && is.list(ctx[[1]])) ctx <- ctx[[1]]
  q$opts$context <- utils::modifyList(q$opts$context %||% list(), ctx)
  q
}

#' @rdname fq_context
#' @export
fq_reasoning <- function(q, mode = c("rdfs", "owl2ql", "owl2rl", "datalog", "none")) {
  check_query(q)
  q$opts$reasoning <- match.arg(mode)
  q
}

#' @rdname fq_context
#' @export
fq_at <- function(q, t) {
  check_query(q)
  q$opts$at <- t
  q
}

# ---- assembly ---------------------------------------------------------------

#' The JSON-LD Query a query object will send
#'
#' @description
#' Assembles the query without sending it. Filters are placed inside `where`,
#' because a top-level `filter` field is ignored by Fluree v4.1.
#'
#' @param q A `fluree_query`.
#'
#' @returns A list, ready to be serialised as JSON-LD Query.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_query_body(fq_limit(fluree_query(con), 5))
#' }
fluree_query_body <- function(q) {
  check_query(q)

  where <- unname(c(q$patterns, lapply(q$filters, function(f) list("filter", f))))
  if (length(where) == 0) {
    stop("A query needs at least one pattern. Add one with fq_where().", call. = FALSE)
  }

  body <- list(from = query_from(q), where = where)

  if (!is.null(q$opts$crawl)) {
    body$select <- stats::setNames(list(q$opts$crawl$props), q$opts$crawl$var)
  } else if (!is.null(q$opts$select)) {
    body$select <- as.list(q$opts$select)
  } else {
    stop("A query needs a select. Add one with fq_select() or fq_crawl().",
         call. = FALSE)
  }

  ctx <- utils::modifyList(
    fluree_context(q$con) %||% list(), q$opts$context %||% list())
  if (length(ctx) > 0) body[["@context"]] <- ctx

  for (nm in c("orderBy", "groupBy", "limit", "offset", "reasoning")) {
    if (!is.null(q$opts[[nm]])) body[[nm]] <- q$opts[[nm]]
  }
  body
}

# Time travel rides on the ledger name: `ledger@t:3`. The CLI spells the same thing
# `--at 3`, and accepts a transaction number, a commit hash or an ISO-8601 stamp.
query_from <- function(q) {
  ledger <- fluree_ledger(q$con)
  if (is.null(q$opts$at)) ledger else paste0(ledger, "@t:", q$opts$at)
}

# ---- filter translation -----------------------------------------------------

# R function name -> Fluree function name, with argument order where it differs
filter_functions <- function() {
  list(
    startsWith = list(name = "strStarts"),
    endsWith   = list(name = "strEnds"),
    nchar      = list(name = "strlen"),
    grepl      = list(name = "regex", swap = TRUE),
    str_detect = list(name = "regex"),
    str_starts = list(name = "strStarts"),
    str_ends   = list(name = "strEnds"),
    str_length = list(name = "strlen"),
    tolower    = list(name = "lcase"),
    toupper    = list(name = "ucase"),
    abs        = list(name = "abs"),
    round      = list(name = "round")
  )
}

binary_operators <- function() {
  c("==" = "=", "!=" = "!=", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=",
    "+" = "+", "-" = "-", "*" = "*", "/" = "/",
    "&" = "and", "&&" = "and", "|" = "or", "||" = "or")
}

translate_filter <- function(e) {
  if (is.name(e)) return(paste0("?", as.character(e)))
  if (is.atomic(e) && length(e) == 1) return(filter_literal(e))

  if (!is.call(e)) {
    stop("Cannot translate this filter expression: ", deparse(e), call. = FALSE)
  }

  op <- as.character(e[[1]])

  if (op == "(") return(translate_filter(e[[2]]))

  if (op == "!") {
    inner <- e[[2]]
    # !is.na(x) is just (bound ?x); no need for a double negation
    if (is.call(inner) && identical(as.character(inner[[1]]), "is.na")) {
      return(sprintf("(bound %s)", translate_filter(inner[[2]])))
    }
    return(sprintf("(not %s)", translate_filter(inner)))
  }

  if (op == "-" && length(e) == 2) {
    return(sprintf("(- 0 %s)", translate_filter(e[[2]])))
  }

  ops <- binary_operators()
  if (op %in% names(ops) && length(e) == 3) {
    return(sprintf("(%s %s %s)", ops[[op]],
                   translate_filter(e[[2]]), translate_filter(e[[3]])))
  }

  if (op == "%in%") {
    return(sprintf("(in %s %s)", translate_filter(e[[2]]), filter_list(e[[3]])))
  }

  if (op == "is.na") {
    return(sprintf("(not (bound %s))", translate_filter(e[[2]])))
  }

  known <- filter_functions()
  args <- lapply(as.list(e)[-1], translate_filter)

  if (op %in% names(known)) {
    spec <- known[[op]]
    if (isTRUE(spec$swap)) args <- rev(args)
    return(sprintf("(%s %s)", spec$name, paste(args, collapse = " ")))
  }

  # unknown name: assume it is a Fluree function and pass it through
  sprintf("(%s %s)", op, paste(args, collapse = " "))
}

filter_literal <- function(x) {
  if (is.character(x)) return(paste0("\"", gsub('"', '\\\\"', x), "\""))
  if (is.logical(x))   return(if (isTRUE(x)) "true" else "false")
  format(x, scientific = FALSE)
}

# (in ?age [30 50]) - a bracketed list literal, which is what Fluree requires
filter_list <- function(e) {
  values <- if (is.call(e) && as.character(e[[1]]) %in% c("c", "list")) {
    lapply(as.list(e)[-1], translate_filter)
  } else if (is.atomic(e)) {
    lapply(e, filter_literal)
  } else {
    list(translate_filter(e))
  }
  paste0("[", paste(unlist(values), collapse = " "), "]")
}

# ---- variable helpers -------------------------------------------------------

as_query_variable <- function(quo) {
  e <- rlang::quo_get_expr(quo)
  if (is.character(e) && length(e) == 1) return(ensure_qmark(e))
  if (is.name(e)) return(paste0("?", as.character(e)))
  stop("Expected a variable name or string, not ", deparse(e), ".", call. = FALSE)
}

as_order_term <- function(quo) {
  e <- rlang::quo_get_expr(quo)
  if (is.call(e) && as.character(e[[1]]) %in% c("desc", "asc")) {
    direction <- as.character(e[[1]])
    inner <- e[[2]]
    var <- if (is.name(inner)) paste0("?", as.character(inner)) else ensure_qmark(inner)
    return(sprintf("(%s %s)", direction, var))
  }
  as_query_variable(quo)
}

ensure_qmark <- function(x) {
  x <- as.character(x)
  # leave aggregates and s-expressions alone
  if (grepl("^\\(", x)) return(x)
  if (startsWith(x, "?") || startsWith(x, "*")) x else paste0("?", x)
}
