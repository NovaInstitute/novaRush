# Internal Fluree v4 request execution -------------------------------------

fluree_trace_payload <- function(x) {
  if (is.numeric(x) && length(x) > 12L) {
    return(list(`__vector__` = TRUE, dimension = length(x)))
  }
  if (is.list(x)) return(lapply(x, fluree_trace_payload))
  x
}

.fluree_request_condition <- function(message, operation, url, method,
                                      status = NULL, response = NULL,
                                      parent = NULL, uncertain = FALSE) {
  structure(
    list(
      message = message,
      call = NULL,
      operation = operation,
      url = url,
      method = method,
      status = status,
      response = response,
      parent = parent
    ),
    class = c(
      if (uncertain) "fluree_uncertain_write",
      "fluree_request_error", "error", "condition"
    )
  )
}

.is_timeout_error <- function(error) {
  grepl("timed? ?out|timeout was reached", conditionMessage(error),
        ignore.case = TRUE)
}

.fluree_transport_error <- function(error, operation, url, method) {
  uncertain <- method %in% c("POST", "PUT", "PATCH", "DELETE") &&
    .is_timeout_error(error)
  suffix <- if (uncertain) {
    "; the write outcome is uncertain, so verify remote state before retrying"
  } else {
    ""
  }
  .fluree_request_condition(
    paste0("Fluree operation '", operation, "' failed at ", url, ": ",
           conditionMessage(error), suffix),
    operation, url, method, parent = error, uncertain = uncertain
  )
}

.fluree_perform_request <- function(method, url, headers, timeout, body) {
  response <- httr::VERB(
    verb = method,
    url = url,
    httr::add_headers(.headers = headers),
    httr::timeout(timeout),
    body = body,
    encode = "raw"
  )
  list(
    status = httr::status_code(response),
    text = httr::content(response, as = "text", encoding = "UTF-8")
  )
}

fluree_request <- function(config, endpoint, method = "GET", body = NULL,
                           ledger = NULL, contentType = "application/json",
                           operation = endpoint, allowStatus = integer()) {
  params <- generateFetchParams(
    config = config, endpoint = endpoint, contentType = contentType,
    ledger = ledger, method = method
  )
  method <- params$config$method
  requestBody <- NULL
  if (!is.null(body)) {
    requestBody <- if (is.character(body)) body else do.call(
      jsonlite::toJSON,
      c(list(x = body), getDefaultToJSONargs())
    )
  }

  response <- tryCatch(
    .fluree_perform_request(
      method = method,
      url = params$url,
      headers = params$config$headers,
      timeout = params$config$timeout,
      body = requestBody
    ),
    error = function(error) {
      stop(.fluree_transport_error(error, operation, params$url, method))
    }
  )

  status <- response$status
  responseText <- response$text
  if (status >= 400L && !status %in% allowStatus) {
    summary <- if (nzchar(responseText)) substr(responseText, 1L, 2000L) else ""
    stop(.fluree_request_condition(
      paste0("Fluree operation '", operation, "' failed at ", params$url,
             " with HTTP ", status,
             if (nzchar(summary)) paste0("; response: ", summary) else ""),
      operation, params$url, method, status, summary
    ))
  }
  if (!nzchar(responseText)) return(invisible(NULL))
  if (!jsonlite::validate(responseText)) return(responseText)
  do.call(jsonlite::fromJSON,
          c(list(txt = responseText), getDefaultFromJSONargs()))
}

fluree_health_check <- function(config) {
  result <- fluree_request(
    config, endpoint = "health", method = "GET",
    operation = "health check"
  )
  if (!is.list(result) || !identical(result$status, "ok")) {
    stop("Fluree health check did not return status 'ok'.", call. = FALSE)
  }
  result
}

fluree_ledger_info <- function(config, branch = config$branch) {
  ledger <- flureeLedgerRef(config$ledger, branch)
  fluree_request(
    config, endpoint = paste0("info/", utils::URLencode(ledger, reserved = TRUE)),
    method = "GET", operation = paste0("inspect ledger ", ledger)
  )
}
