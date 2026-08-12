# Fluree hands back three different shapes. These turn each of them into a tibble.
# fromJSON is configured with no simplification (see getDefaultFromJSONargs), so
# every response arrives as a predictable nested list.

#' Turn a JSON-LD Query result into a tibble
#'
#' @description
#' Handles both result shapes. The array form of `select` returns positional rows,
#' so column names come from the `select` vector; the object form returns whole
#' nodes, so columns come from the union of their keys.
#'
#' Values that are not scalars - a nested node, or a repeated property - become
#' list-columns rather than being flattened or dropped.
#'
#' @param result The parsed response body.
#' @param select The `select` value that was sent, used for column names in the
#'   array form.
#'
#' @returns A tibble.
#' @export
#'
#' @examples
#' fluree_result_to_tibble(
#'   list(list("ex:a", "Ann", 30), list("ex:b", "Bob", 40)),
#'   select = c("?s", "?name", "?age"))
fluree_result_to_tibble <- function(result, select = NULL) {
  rows <- as_row_list(result)

  if (length(rows) == 0) return(empty_tibble(select))

  if (is_node_shape(rows)) node_rows_to_tibble(rows) else array_rows_to_tibble(rows, select)
}

# A single node object comes back unwrapped rather than in a list of one
as_row_list <- function(result) {
  if (is.null(result)) return(list())
  if (!is.list(result)) return(list(list(result)))
  if (length(result) > 0 && !is.null(names(result))) return(list(result))
  result
}

is_node_shape <- function(rows) {
  first <- rows[[1]]
  is.list(first) && !is.null(names(first)) && any(nzchar(names(first)))
}

empty_tibble <- function(select) {
  if (is.null(select)) return(tibble::tibble())
  cols <- stats::setNames(
    rep(list(character()), length(select)), strip_qmark(unlist(select)))
  tibble::as_tibble(cols)
}

array_rows_to_tibble <- function(rows, select) {
  width <- max(vapply(rows, length, integer(1)))

  names_out <- if (!is.null(select) && length(unlist(select)) == width) {
    strip_qmark(unlist(select))
  } else {
    paste0("V", seq_len(width))
  }

  cols <- lapply(seq_len(width), function(j) {
    simplify_column(lapply(rows, function(r) if (j <= length(r)) r[[j]] else NULL))
  })
  tibble::as_tibble(stats::setNames(cols, make.unique(names_out)))
}

node_rows_to_tibble <- function(rows) {
  keys <- unique(unlist(lapply(rows, names)))
  keys <- keys[nzchar(keys)]

  cols <- lapply(keys, function(k) {
    simplify_column(lapply(rows, function(r) r[[k]]))
  })
  tibble::as_tibble(stats::setNames(cols, keys))
}

# scalar-per-row -> atomic column; anything else -> list-column
simplify_column <- function(values) {
  scalar <- vapply(values, function(v) {
    is.null(v) || (is.atomic(v) && length(v) == 1)
  }, logical(1))

  if (!all(scalar)) {
    return(lapply(values, function(v) if (is.null(v)) NA else v))
  }
  unlist(lapply(values, function(v) if (is.null(v)) NA else v), use.names = FALSE)
}

strip_qmark <- function(x) sub("^\\?", "", as.character(x))

#' Turn a W3C SPARQL JSON response into a tibble
#'
#' @description
#' Reads the standard SPARQL results format - `head$vars` for the column names and
#' `results$bindings` for the rows - and coerces each value using the `datatype` the
#' binding carries, so integers arrive as integers rather than strings.
#'
#' @param result The parsed response body.
#'
#' @returns A tibble, with one column per projected variable.
#' @export
#'
#' @examples
#' fluree_bindings_to_tibble(list(
#'   head = list(vars = list("name", "age")),
#'   results = list(bindings = list(
#'     list(name = list(type = "literal", value = "Ann"),
#'          age  = list(type = "literal", value = "30",
#'                      datatype = "http://www.w3.org/2001/XMLSchema#integer"))))))
fluree_bindings_to_tibble <- function(result) {
  vars <- unlist(result$head$vars)
  bindings <- result$results$bindings %||% list()

  if (length(bindings) == 0) {
    if (is.null(vars)) return(tibble::tibble())
    return(tibble::as_tibble(
      stats::setNames(rep(list(character()), length(vars)), vars)))
  }

  if (is.null(vars)) vars <- unique(unlist(lapply(bindings, names)))

  cols <- lapply(vars, function(v) {
    cells <- lapply(bindings, function(b) b[[v]])
    values <- vapply(cells, function(cell) {
      if (is.null(cell)) NA_character_ else as.character(cell$value)
    }, character(1))

    datatypes <- unique(unlist(lapply(cells, function(cell) cell$datatype)))
    coerce_binding(values, if (length(datatypes) == 1) datatypes else NA_character_)
  })

  tibble::as_tibble(stats::setNames(cols, vars))
}

coerce_binding <- function(values, datatype) {
  if (is.na(datatype) || is.null(datatype)) return(values)

  switch(tolower(sub("^.*[:#]", "", datatype)),
    integer = , int = , long = , short = , byte = ,
    nonnegativeinteger = , positiveinteger = as.integer(values),
    decimal = , double = , float = as.numeric(values),
    boolean = values %in% c("true", "1"),
    date = as.Date(values),
    datetime = as.POSIXct(values, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"),
    values
  )
}
