#' Validate a Fluree branch name
#'
#' @param branch Branch name.
#' @return `branch`, invisibly, when valid.
#' @keywords internal
validateBranchName <- function(branch) {
  if (length(branch) != 1L || is.na(branch) || !nzchar(branch) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", branch)) {
    stop(paste(
      "`branch` must start with a letter or number and contain only",
      "letters, numbers, periods, underscores, or hyphens."
    ), call. = FALSE)
  }
  invisible(branch)
}

#' List branches of a Fluree ledger
#'
#' @param config Fluree configuration created by [setConfig()].
#' @param ledger Ledger name. Defaults to `config$ledger`.
#'
#' @return A list of branch records returned by Fluree.
#' @export
listBranches <- function(config, ledger = config$ledger) {
  if (length(ledger) != 1L || is.na(ledger) || !nzchar(ledger)) {
    stop("`ledger` must be one non-empty string.", call. = FALSE)
  }
  fluree_request(
    config,
    endpoint = paste0("branch/", utils::URLencode(ledger, reserved = TRUE)),
    method = "GET",
    operation = paste0("list branches for ", ledger)
  )
}

#' Test whether a Fluree branch exists
#'
#' @param config Fluree configuration created by [setConfig()].
#' @param branch Branch name.
#' @param ledger Ledger name. Defaults to `config$ledger`.
#'
#' @return A single logical value.
#' @export
branchExists <- function(config, branch, ledger = config$ledger) {
  validateBranchName(branch)
  branches <- listBranches(config, ledger)
  if (!length(branches)) return(FALSE)
  any(vapply(branches, function(record) {
    identical(record$branch, branch) ||
      identical(record$ledger_id, flureeLedgerRef(ledger, branch))
  }, logical(1)))
}

#' Create a Fluree branch
#'
#' Creates a branch from `from`, which defaults to the configured branch.
#' Existing branches are treated as an idempotent success.
#'
#' @param config Fluree configuration created by [setConfig()].
#' @param branch New branch name.
#' @param from Source branch. Defaults to `config$branch`.
#' @param ledger Ledger name. Defaults to `config$ledger`.
#'
#' @return The Fluree branch record. If the branch already exists, a list with
#'   `created = FALSE` and `exists = TRUE` is returned.
#' @export
createBranch <- function(config, branch, from = config$branch,
                         ledger = config$ledger) {
  validateBranchName(branch)
  validateBranchName(from)
  if (identical(branch, from)) {
    stop("A branch cannot be created from itself.", call. = FALSE)
  }
  if (branchExists(config, branch, ledger)) {
    return(list(created = FALSE, exists = TRUE, ledger = ledger,
                branch = branch, from = from))
  }
  response <- fluree_request(
    config,
    endpoint = "branch",
    method = "POST",
    body = list(ledger = ledger, branch = branch, from = from),
    operation = paste0("create branch ", ledger, ":", branch),
    allowStatus = 409L,
    returnResponse = TRUE,
    write = TRUE
  )
  if (identical(response$status, 409L)) {
    return(list(created = FALSE, exists = TRUE, ledger = ledger,
                branch = branch, from = from, response = response$body))
  }
  response$body
}
