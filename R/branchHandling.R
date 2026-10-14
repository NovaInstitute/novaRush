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

#' Inspect a Fluree branch
#'
#' @param config Fluree configuration created by [setConfig()].
#' @param branch Branch to inspect. Defaults to `config$branch`.
#'
#' @return The parsed Fluree ledger information for the selected branch.
#' @export
branchInfo <- function(config, branch = config$branch) {
  validateBranchName(branch)
  fluree_ledger_info(config, branch = branch)
}

#' Read the current commit identity of a Fluree branch
#'
#' @param config Fluree configuration created by [setConfig()].
#' @param branch Branch to inspect. Defaults to `config$branch`.
#'
#' @return A single commit identity string.
#' @export
branchHead <- function(config, branch = config$branch) {
  info <- branchInfo(config, branch)
  commit <- info$commit %||% list()
  head <- commit$id %||% commit$hash %||% commit$address
  if (is.null(head) || length(head) != 1L || is.na(head) || !nzchar(head)) {
    stop("Fluree did not return a commit identity for branch '", branch,
         "'.", call. = FALSE)
  }
  as.character(head)
}

#' Merge one Fluree branch into another
#'
#' The optional expected target head protects callers from publishing work
#' prepared against an older target. Fluree still performs its own atomic
#' branch conflict check when the merge is submitted.
#'
#' @param config Fluree configuration created by [setConfig()].
#' @param source Source branch containing the prepared changes.
#' @param target Target branch. Defaults to `"main"`.
#' @param expected_target_head Optional commit identity previously read with
#'   [branchHead()].
#' @param strategy Optional server-supported conflict strategy. Omit for the
#'   server default.
#'
#' @return The parsed Fluree merge response.
#' @export
mergeBranch <- function(config, source, target = "main",
                        expected_target_head = NULL, strategy = NULL) {
  validateBranchName(source)
  validateBranchName(target)
  if (identical(source, target)) {
    stop("A branch cannot be merged into itself.", call. = FALSE)
  }
  if (!is.null(expected_target_head)) {
    expected_target_head <- as.character(expected_target_head)
    if (length(expected_target_head) != 1L || is.na(expected_target_head) ||
        !nzchar(expected_target_head)) {
      stop("`expected_target_head` must be one non-empty string.",
           call. = FALSE)
    }
    actual <- branchHead(config, target)
    if (!identical(actual, expected_target_head)) {
      stop(structure(
        list(
          message = paste0(
            "Cannot merge stale branch '", source, "': target branch '",
            target, "' advanced from ", expected_target_head, " to ", actual,
            "."
          ),
          call = NULL,
          source = source,
          target = target,
          expected_target_head = expected_target_head,
          actual_target_head = actual
        ),
        class = c("fluree_branch_conflict", "error", "condition")
      ))
    }
  }
  if (!is.null(strategy) &&
      (length(strategy) != 1L || is.na(strategy) || !nzchar(strategy))) {
    stop("`strategy` must be NULL or one non-empty string.", call. = FALSE)
  }
  body <- list(ledger = config$ledger, source = source, target = target)
  if (!is.null(strategy)) body$strategy <- strategy
  response <- fluree_request(
    config,
    endpoint = "merge",
    method = "POST",
    body = body,
    operation = paste0("merge branch ", config$ledger, ":", source,
                       " into ", target),
    allowStatus = 409L,
    returnResponse = TRUE,
    write = TRUE
  )
  if (identical(response$status, 409L)) {
    stop(structure(
      list(
        message = paste0(
          "Fluree rejected the merge of '", source, "' into '", target,
          "' because the branches conflict."
        ),
        call = NULL,
        source = source,
        target = target,
        response = response$body
      ),
      class = c("fluree_branch_conflict", "error", "condition")
    ))
  }
  response$body
}
