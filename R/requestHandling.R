# Internal Fluree v4 request execution -------------------------------------

fluree_trace_payload <- function(x) {
  if (is.numeric(x) && length(x) > 12L) {
    return(list(`__vector__` = TRUE, dimension = length(x)))
  }
  if (is.list(x)) return(lapply(x, fluree_trace_payload))
  x
}

.fluree_trace_level <- function() {
  value <- getOption("novaRush.trace", Sys.getenv("NOVARUSH_TRACE", "off"))
  value <- tolower(as.character(value %||% "off")[[1L]])
  aliases <- c("false" = "off", "0" = "off", "true" = "summary",
               "1" = "summary", "full" = "payloads")
  if (value %in% names(aliases)) value <- aliases[[value]]
  if (!value %in% c("off", "summary", "payloads")) {
    warning(
      "Unknown novaRush trace level '", value,
      "'; expected off, summary, or payloads. Tracing is disabled.",
      call. = FALSE
    )
    value <- "off"
  }
  value
}

.fluree_trace_counter <- local({
  counter <- 0L
  function() {
    counter <<- counter + 1L
    counter
  }
})

.fluree_trace_stem <- function(operation) {
  safe <- gsub("[^A-Za-z0-9._-]+", "-", tolower(operation))
  safe <- gsub("(^-+|-+$)", "", safe)
  paste0(
    format(Sys.time(), "%Y%m%dT%H%M%OS3"), "-p", Sys.getpid(), "-",
    sprintf("%04d", .fluree_trace_counter()), "-", safe
  )
}

.fluree_trace_start <- function(level, operation, method, url, requestBody) {
  if (identical(level, "off")) return(NULL)
  bytes <- if (is.null(requestBody)) 0L else nchar(requestBody, type = "bytes")
  message(
    "[novaRush] ", operation, " | ", method, " ", url,
    " | request ", format(bytes, big.mark = ","), " bytes"
  )
  if (!identical(level, "payloads")) return(NULL)
  directory <- getOption(
    "novaRush.trace_dir", Sys.getenv("NOVARUSH_TRACE_DIR", "")
  )
  if (!length(directory) || is.na(directory) || !nzchar(directory)) {
    warning(
      "NOVARUSH_TRACE=payloads requires NOVARUSH_TRACE_DIR; only summary ",
      "output will be produced.", call. = FALSE
    )
    return(NULL)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) {
    warning("Could not create novaRush trace directory: ", directory,
            call. = FALSE)
    return(NULL)
  }
  stem <- file.path(directory, .fluree_trace_stem(operation))
  writeLines(requestBody %||% "", paste0(stem, ".request.json"),
             useBytes = TRUE)
  metadata <- list(
    operation = operation, method = method, url = url,
    request_bytes = bytes, request_file = paste0(basename(stem), ".request.json")
  )
  jsonlite::write_json(metadata, paste0(stem, ".meta.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  message("[novaRush] request payload: ", paste0(stem, ".request.json"))
  stem
}

.fluree_trace_finish <- function(level, stem, operation, status, responseText,
                                 elapsed) {
  if (identical(level, "off")) return(invisible(NULL))
  bytes <- nchar(responseText %||% "", type = "bytes")
  message(
    "[novaRush] ", operation, " | HTTP ", status, " | response ",
    format(bytes, big.mark = ","), " bytes | ",
    format(round(elapsed, 3), nsmall = 3), " s"
  )
  if (identical(level, "payloads") && !is.null(stem)) {
    response_file <- paste0(stem, ".response.json")
    writeLines(responseText %||% "", response_file, useBytes = TRUE)
    message("[novaRush] response payload: ", response_file)
  }
  invisible(NULL)
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

.fluree_transport_error <- function(error, operation, url, method,
                                    write = operation %in%
                                      c("insert", "upsert", "update", "create")) {
  uncertain <- isTRUE(write) &&
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
                           operation = endpoint, allowStatus = integer(),
                           query = NULL, returnResponse = FALSE,
                           write = endpoint %in% c("insert", "upsert", "update",
                                                   "create")) {
  params <- generateFetchParams(
    config = config, endpoint = endpoint, contentType = contentType,
    ledger = ledger, method = method
  )
  if (!is.null(query) && length(query)) {
    params$url <- httr::modify_url(params$url, query = query)
  }
  method <- params$config$method
  requestBody <- NULL
  if (!is.null(body)) {
    requestBody <- if (is.character(body)) body else do.call(
      jsonlite::toJSON,
      c(list(x = body), getDefaultToJSONargs())
    )
  }

  traceLevel <- .fluree_trace_level()
  traceStem <- .fluree_trace_start(
    traceLevel, operation, method, params$url, requestBody
  )
  started <- proc.time()[["elapsed"]]

  response <- tryCatch(
    .fluree_perform_request(
      method = method,
      url = params$url,
      headers = params$config$headers,
      timeout = params$config$timeout,
      body = requestBody
    ),
    error = function(error) {
      stop(.fluree_transport_error(
        error, operation, params$url, method, write = write
      ))
    }
  )

  status <- response$status
  responseText <- response$text
  .fluree_trace_finish(
    traceLevel, traceStem, operation, status, responseText,
    proc.time()[["elapsed"]] - started
  )
  if (status >= 400L && !status %in% allowStatus) {
    summary <- if (nzchar(responseText)) substr(responseText, 1L, 2000L) else ""
    stop(.fluree_request_condition(
      paste0("Fluree operation '", operation, "' failed at ", params$url,
             " with HTTP ", status,
             if (nzchar(summary)) paste0("; response: ", summary) else ""),
      operation, params$url, method, status, summary
    ))
  }
  value <- if (!nzchar(responseText)) {
    NULL
  } else if (!jsonlite::validate(responseText)) {
    responseText
  } else {
    do.call(jsonlite::fromJSON,
            c(list(txt = responseText), getDefaultFromJSONargs()))
  }
  if (isTRUE(returnResponse)) return(list(status = status, body = value))
  value
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
