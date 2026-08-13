live_fluree_enabled <- function() {
  identical(tolower(Sys.getenv("FLUREE_LIVE_TEST", "false")), "true")
}

skip_if_no_live_fluree <- function() {
  testthat::skip_if_not(
    live_fluree_enabled(),
    "Set FLUREE_LIVE_TEST=true to run tests against a live Fluree server."
  )
}

live_fluree_config <- function() {
  base_url <- Sys.getenv("FLUREE_BASE_URL", "http://localhost:8090")
  ledger <- Sys.getenv("FLUREE_TEST_LEDGER", "novarush-integration")
  branch <- Sys.getenv("FLUREE_TEST_BRANCH", "main")
  token <- Sys.getenv("FLUREE_API_TOKEN", "")
  timeout <- suppressWarnings(as.numeric(
    Sys.getenv("FLUREE_REQUEST_TIMEOUT", "60")
  ))
  if (!nzchar(token)) token <- NULL
  setConfig(
    baseUrl = base_url,
    ledger = ledger,
    branch = branch,
    apiKey = token,
    timeout = timeout,
    signMessages = FALSE
  )
}

live_query_values <- function(result) {
  values <- unlist(result, recursive = TRUE, use.names = FALSE)
  as.character(values)
}

live_fluree_branch_name <- function(prefix = "integration") {
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC")
  value <- paste(prefix, stamp, Sys.getpid(), sep = "-")
  gsub("[^A-Za-z0-9-]", "", value)
}
