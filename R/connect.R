# The tidy surface. FlureeInstance and the other R6 classes remain the transport
# and are unchanged; nothing below asks the user to call a `$` method.

#' Connect to a Fluree instance
#'
#' @description
#' Opens a connection and returns an opaque handle to pass to the tidy verbs. The
#' handle wraps a [FlureeInstance], which stays the transport layer - signing,
#' context merging and HTTP all still happen there - but you never need to reach
#' into it.
#'
#' @param host Host name, e.g. `"localhost"` or `"data.flur.ee"`. Omit when
#'   `fluree_hosted = TRUE`.
#' @param ledger Ledger name, e.g. `"novaRush/demo"`. Required.
#' @param port Port number. `NULL` for the default.
#' @param context A named character vector or list of JSON-LD prefixes merged into
#'   every subsequent query and transaction.
#' @param sign Whether to sign messages. Requires `private_key`.
#' @param private_key Private key as a hex string. Pass `getKey()` to read the one
#'   stored in your keyring - this is deliberately not done for you, because reading
#'   the keyring can prompt, which would block any non-interactive script.
#' @param api_key API key, for the Fluree hosted service.
#' @param fluree_hosted Set `TRUE` for the hosted service, which takes no host or
#'   port.
#' @param create Whether to create the ledger if it does not exist.
#' @param branch Ledger branch to read and write. Every query and transaction made
#'   through this connection is scoped to it.
#' @param timeout Request timeout in seconds.
#' @param base_url Full base URL, e.g. `"http://localhost:8090"`. Overrides `host`
#'   and `port` when given.
#' @param api_path API path prefix. Defaults to `"/v1/fluree"`.
#' @param check Whether to verify the server is reachable and the ledger exists.
#'   `FALSE` builds a handle without any request, which is what makes offline query
#'   construction possible - but the handle cannot send until it is connected.
#'
#' @returns A `fluree_connection`.
#' @seealso [fluree_query()] to read, [fluree_insert()] to write,
#'   [fluree_graph()] for a tidygraph view, [fluree_branch()] for the branch.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- fluree_connect("localhost", ledger = "novaRush/demo", port = 8090,
#'                       context = c(schema = "http://schema.org/",
#'                                   ex     = "http://example.org/"))
#' con
#'
#' # a candidate branch, isolated from main
#' fluree_connect("localhost", ledger = "novaRush/demo", port = 8090,
#'                branch = "candidate-01")
#' }
fluree_connect <- function(host = NULL,
                           ledger,
                           port = NULL,
                           context = NULL,
                           sign = FALSE,
                           private_key = NULL,
                           api_key = NULL,
                           fluree_hosted = FALSE,
                           create = FALSE,
                           branch = "main",
                           timeout = 60,
                           base_url = NULL,
                           api_path = NULL,
                           check = TRUE) {
  if (missing(ledger) || is.null(ledger)) {
    stop("`ledger` is required.", call. = FALSE)
  }
  if (length(branch) != 1L || is.na(branch) || !nzchar(branch)) {
    stop("`branch` must be one non-empty string.", call. = FALSE)
  }

  # Deliberately no implicit getKey(): unlocking a keyring can prompt, and a
  # connection helper must never block a script on stdin.
  if (isTRUE(sign) && is.null(private_key)) {
    stop("`sign = TRUE` needs `private_key`. Pass private_key = getKey() to use ",
         "the key in your keyring, or setKey() to store one first.", call. = FALSE)
  }

  config <- compact_list(list(
    host           = host,
    port           = port,
    ledger         = ledger,
    branch         = branch,
    signMessages   = sign,
    privateKey     = private_key,
    apiKey         = api_key,
    baseUrl        = base_url,
    apiPath        = api_path,
    timeout        = timeout,
    isFlureeHosted = if (isTRUE(fluree_hosted)) TRUE else NULL,
    create         = if (isTRUE(create)) TRUE else NULL
  ))

  instance <- FlureeInstance$new(config)
  if (!is.null(context)) instance$setContext(as_context_list(context))
  if (isTRUE(check)) instance$connect()

  new_fluree_connection(instance)
}

new_fluree_connection <- function(instance) {
  structure(list(instance = instance), class = "fluree_connection")
}

#' @export
print.fluree_connection <- function(x, ...) {
  cfg <- x$instance$config
  where <- if (isTRUE(cfg$isFlureeHosted)) {
    "Fluree hosted"
  } else if (!is.null(cfg$baseUrl)) {
    cfg$baseUrl
  } else {
    paste0(cfg$host %||% "?", if (!is.null(cfg$port)) paste0(":", cfg$port) else "")
  }

  cat("<fluree_connection>\n")
  cat("  host   ", where, "\n", sep = "")
  cat("  ledger ", cfg$ledger %||% "?", "\n", sep = "")
  cat("  branch ", cfg$branch %||% "main", "\n", sep = "")
  cat("  signed ", if (isTRUE(cfg$signMessages)) "yes" else "no", "\n", sep = "")

  ctx <- cfg$defaultContext
  if (length(ctx) > 0) {
    cat("  context ", length(ctx), " prefix(es): ",
        paste(utils::head(names(ctx), 5), collapse = ", "),
        if (length(ctx) > 5) ", ..." else "", "\n", sep = "")
  }
  invisible(x)
}

#' @export
format.fluree_connection <- function(x, ...) {
  cfg <- x$instance$config
  paste0("<fluree_connection ", cfg$host %||% cfg$baseUrl %||% "hosted", "/",
         cfg$ledger %||% "?", ":", cfg$branch %||% "main", ">")
}

#' Is this a Fluree connection?
#'
#' @param x An object.
#' @returns `TRUE` or `FALSE`.
#' @export
#'
#' @examples
#' is_fluree_connection(1)
is_fluree_connection <- function(x) inherits(x, "fluree_connection")

check_connection <- function(con, arg = "con") {
  if (!is_fluree_connection(con)) {
    stop("`", arg, "` must be a fluree_connection from fluree_connect(), not ",
         class(con)[1], ".", call. = FALSE)
  }
  invisible(con)
}

#' The ledger a connection points at
#'
#' @param con A `fluree_connection`.
#' @returns The ledger name, as a string.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_ledger(con)
#' }
fluree_ledger <- function(con) {
  check_connection(con)
  con$instance$config$ledger
}

#' The branch a connection points at
#'
#' @param con A `fluree_connection`.
#' @returns The branch name, as a string.
#' @seealso [listBranches()] and [createBranch()] to administer branches.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_branch(con)
#' }
fluree_branch <- function(con) {
  check_connection(con)
  con$instance$config$branch %||% "main"
}

#' The default JSON-LD context of a connection
#'
#' @param con A `fluree_connection`.
#' @returns A named list of prefixes, or `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' fluree_context(con)
#' }
fluree_context <- function(con) {
  check_connection(con)
  con$instance$config$defaultContext
}

compact_list <- function(x) x[!vapply(x, is.null, logical(1))]

as_context_list <- function(context) {
  if (is.list(context)) return(context)
  if (is.null(names(context))) {
    stop("`context` must be named, e.g. c(schema = \"http://schema.org/\").",
         call. = FALSE)
  }
  as.list(context)
}
