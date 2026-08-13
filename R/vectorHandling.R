#' Validate embedding vectors
#'
#' @param vectors A numeric vector or list of numeric vectors.
#' @param dimension Optional expected dimension.
#'
#' @return A list containing normalized vectors, their count, and dimension.
#' @export
validateVectors <- function(vectors, dimension = NULL) {
  if (is.numeric(vectors)) vectors <- list(vectors)
  if (!is.list(vectors) || !length(vectors)) {
    stop("`vectors` must contain at least one numeric vector.", call. = FALSE)
  }
  normalized <- lapply(vectors, function(vector) {
    if (!is.numeric(vector) || !length(vector)) {
      stop("Every embedding must be a non-empty numeric vector.", call. = FALSE)
    }
    vector <- as.numeric(vector)
    if (any(!is.finite(vector))) {
      stop("Every embedding value must be finite.", call. = FALSE)
    }
    vector
  })
  dimensions <- lengths(normalized)
  if (length(unique(dimensions)) != 1L) {
    stop("All embeddings must have a consistent dimension.", call. = FALSE)
  }
  actual <- dimensions[[1L]]
  if (!is.null(dimension)) {
    dimension <- suppressWarnings(as.integer(dimension))
    if (length(dimension) != 1L || is.na(dimension) || dimension <= 0L) {
      stop("`dimension` must be one positive integer.", call. = FALSE)
    }
    if (!identical(actual, dimension)) {
      stop(paste0(
        "Embedding dimension mismatch: expected ", dimension,
        " but received ", actual, "."
      ), call. = FALSE)
    }
  }
  list(vectors = normalized, count = length(normalized), dimension = actual)
}

#' Construct a native Fluree vector literal
#'
#' @param vector Numeric embedding vector.
#' @param query Whether the literal will be used in a query. Query literals use
#'   the full `f:embeddingVector` datatype because `@vector` is transaction-only
#'   shorthand.
#'
#' @return A JSON-LD typed-value list.
#' @export
flureeVector <- function(vector, query = FALSE) {
  checked <- validateVectors(vector)
  list(
    "@value" = checked$vectors[[1L]],
    "@type" = if (isTRUE(query)) {
      "https://ns.flur.ee/db#embeddingVector"
    } else {
      "@vector"
    }
  )
}

.validate_property_iri <- function(value, name) {
  if (length(value) != 1L || is.na(value) || !nzchar(value) ||
      !grepl("^[A-Za-z][A-Za-z0-9+.-]*:[^[:space:]]+$", value)) {
    stop(paste0("`", name, "` must be one absolute property IRI."),
         call. = FALSE)
  }
  invisible(value)
}

.prepare_vector_records <- function(records, vector_property, model = NULL,
                                    model_property = NULL,
                                    dimension_property = NULL,
                                    dimension = NULL) {
  .validate_property_iri(vector_property, "vector_property")
  if (!is.list(records) || !length(records) ||
      !all(vapply(records, is.list, logical(1)))) {
    stop("`records` must be a non-empty list of JSON-LD resources.",
         call. = FALSE)
  }
  if (!all(vapply(records, function(record) {
    !is.null(record[["@id"]]) && !is.null(record[[vector_property]])
  }, logical(1)))) {
    stop("Every vector record must have `@id` and the vector property.",
         call. = FALSE)
  }
  if (xor(is.null(model), is.null(model_property))) {
    stop("`model` and `model_property` must be supplied together.",
         call. = FALSE)
  }
  if (!is.null(model_property)) {
    .validate_property_iri(model_property, "model_property")
    if (length(model) != 1L || is.na(model) || !nzchar(model)) {
      stop("`model` must be one non-empty string.", call. = FALSE)
    }
  }
  if (!is.null(dimension_property)) {
    .validate_property_iri(dimension_property, "dimension_property")
  }
  raw_vectors <- lapply(records, `[[`, vector_property)
  checked <- validateVectors(raw_vectors, dimension = dimension)
  prepared <- Map(function(record, vector) {
    record[[vector_property]] <- flureeVector(vector)
    if (!is.null(model_property)) record[[model_property]] <- model
    if (!is.null(dimension_property)) {
      record[[dimension_property]] <- list(
        "@value" = checked$dimension,
        "@type" = "http://www.w3.org/2001/XMLSchema#integer"
      )
    }
    record
  }, records, checked$vectors)
  list(records = prepared, dimension = checked$dimension,
       count = checked$count)
}

#' Store vector-bearing JSON-LD resources in a named graph
#'
#' @param records List of JSON-LD resources. Each must contain `@id` and a raw
#'   numeric vector under `vector_property`.
#' @param graph Absolute named-graph IRI.
#' @param vector_property Absolute property IRI holding the embedding.
#' @param config Fluree configuration created by [setConfig()].
#' @param branch Target branch. Defaults to `config$branch`.
#' @param model Optional embedding model identifier.
#' @param model_property Absolute property IRI used to store `model`. Required
#'   when `model` is supplied.
#' @param dimension_property Optional property IRI used to store vector
#'   dimensions.
#' @param dimension Optional expected dimension.
#'
#' @return The parsed Fluree transaction receipt.
#' @export
upsertVectors <- function(records, graph, vector_property, config,
                          branch = config$branch, model = NULL,
                          model_property = NULL, dimension_property = NULL,
                          dimension = NULL) {
  prepared <- .prepare_vector_records(
    records, vector_property, model, model_property, dimension_property,
    dimension
  )
  upsertNamedGraph(
    prepared$records, graph, config = config, branch = branch
  )
}

.vector_metric <- function(metric) {
  if (length(metric) != 1L || is.na(metric)) {
    stop("`metric` must be one of cosine, dot, or euclidean.", call. = FALSE)
  }
  match <- match(tolower(metric), c("cosine", "dot", "euclidean"))
  if (is.na(match)) {
    stop("`metric` must be one of cosine, dot, or euclidean.", call. = FALSE)
  }
  c("cosineSimilarity", "dotProduct", "euclideanDistance")[[match]]
}

#' Construct an exact Fluree vector-similarity query
#'
#' @param vector_property Absolute property IRI holding stored vectors.
#' @param query_vector Numeric query vector.
#' @param metric Similarity metric: `cosine`, `dot`, or `euclidean`.
#' @param limit Maximum results.
#' @param where Optional additional JSON-LD query patterns applied before
#'   similarity scoring.
#' @param select Additional variables to return alongside `?entity` and
#'   `?score`.
#' @param threshold Optional score threshold. For Euclidean distance this is a
#'   maximum; for cosine and dot product it is a minimum.
#' @param context Optional JSON-LD context.
#'
#' @return A JSON-LD query list. The graph and branch are added by
#'   [searchVectors()].
#' @export
vectorSimilarityQuery <- function(vector_property, query_vector,
                                  metric = "cosine", limit = 10L,
                                  where = list(), select = list(),
                                  threshold = NULL, context = NULL) {
  .validate_property_iri(vector_property, "vector_property")
  checked <- validateVectors(query_vector)
  function_name <- .vector_metric(metric)
  limit <- suppressWarnings(as.integer(limit))
  if (length(limit) != 1L || is.na(limit) || limit <= 0L) {
    stop("`limit` must be one positive integer.", call. = FALSE)
  }
  if (!is.list(where) || !is.list(select)) {
    stop("`where` and `select` must be lists.", call. = FALSE)
  }
  patterns <- c(
    where,
    list(
      setNames(list("?entity", "?storedVector"),
               c("@id", vector_property)),
      list("bind", "?score",
           paste0("(", function_name, " ?storedVector ?queryVector)"))
    )
  )
  if (!is.null(threshold)) {
    threshold <- suppressWarnings(as.numeric(threshold))
    if (length(threshold) != 1L || is.na(threshold) || !is.finite(threshold)) {
      stop("`threshold` must be one finite number.", call. = FALSE)
    }
    operator <- if (identical(function_name, "euclideanDistance")) "<=" else ">="
    patterns <- c(patterns, list(list(
      "filter", paste0("(", operator, " ?score ", threshold, ")")
    )))
  }
  result <- list(
    select = unique(c(list("?entity", "?score"), select)),
    values = list(
      list("?queryVector"),
      list(flureeVector(checked$vectors[[1L]], query = TRUE))
    ),
    where = patterns,
    orderBy = if (identical(function_name, "euclideanDistance")) {
      list(list("asc", "?score"))
    } else {
      list(list("desc", "?score"))
    },
    limit = limit
  )
  if (!is.null(context)) result[["@context"]] <- context
  result
}

#' Search vectors in a named graph
#'
#' Performs exact inline similarity scoring in Fluree.
#'
#' @inheritParams vectorSimilarityQuery
#' @param graph Absolute named-graph IRI.
#' @param config Fluree configuration created by [setConfig()].
#' @param branch Source branch. Defaults to `config$branch`.
#'
#' @return Parsed query results ordered by similarity.
#' @export
searchVectors <- function(graph, vector_property, query_vector, config,
                          branch = config$branch, metric = "cosine",
                          limit = 10L, where = list(), select = list(),
                          threshold = NULL, context = NULL) {
  query <- vectorSimilarityQuery(
    vector_property = vector_property,
    query_vector = query_vector,
    metric = metric,
    limit = limit,
    where = where,
    select = select,
    threshold = threshold,
    context = context
  )
  queryNamedGraph(query, graph, config = config, branch = branch)
}
