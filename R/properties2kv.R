
#' properties2kv
#' @description Convert a properties tibble to a key-value map suitable for JSON-LD serialization.
#' @param dfprops A data frame with columns `predicate`, `object`, and `object_type` (either "uri" or "literal").
#'
#' @returns A named list representing the key-value map.
#' @export
#'
#' @examples
#' dfprops <- structure(list(predicate = c("rdf:type", "rdfs:label", "survey:hasQuestion", "survey:hasQuestion", "survey:hasQuestion"), object = c("survey:Survey", "SurveyCTO Form", "https://novapc.surveycto.com/KiA_adaptation_ACTIVE/question/starttime", "https://novapc.surveycto.com/KiA_adaptation_ACTIVE/question/endtime", "https://novapc.surveycto.com/KiA_adaptation_ACTIVE/question/deviceid"), object_type = c("uri", "literal", "uri", "uri", "uri")), row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))
#' res <- properties2kv(dfprops, id = "https://novapc.surveycto.com/KiA_adaptation_ACTIVE_partial")
#' cat(toJSON(res, auto_unbox = TRUE, pretty = TRUE))


properties2kv <- function(dfprops, id, typename = "object_type", to_type = NULL) {

  if (typename != "object_type") {
    dfprops <- dfprops %>%
      dplyr::rename(object_type = all_of(typename))
  }

  if (!all(c("predicate", "object", "object_type") %in% names(dfprops))) {
    stop("Input data frame must contain columns: predicate, object, object_type")
  }

  if (missing(id) || !is.character(id) || length(id) != 1L) {
    stop("An 'id' parameter must be provided as a single string.")
  }


ll <- dfprops %>% group_by(predicate, object_type) %>% group_split() %>% map(~{
  l <- .x
  if (l$object_type[1] == "uri") {
    l <- dplyr::rename(l, "@id" = "object") %>% select( -object_type)
  } else {
    new_name <- unique(l$predicate)
    if (length(new_name) > 1) stop("More than one predicate in group")
    l <- dplyr::rename(l, !!new_name := "object")  %>% select( -object_type)
  }
  l
})

# If any tibble contains multiple predicates, normalize first:
ll_norm <- ll %>%
  purrr::map(~ if (dplyr::n_distinct(.x$predicate) > 1) dplyr::group_split(.x, predicate) else list(.x)) %>%
  flatten()

# list-of-tibbles -> predicate-keyed map
pred_map <- ll_norm %>%
  map(predicate_entry) %>%
  reduce(
    .init = list(),
    .f = function(acc, item) {
      k <- names(item)
      acc[[k]] <- merge_vals(acc[[k]], item[[1]])
      acc
    }
  )

# if pred_map has an item named "`rdf:type`" , change it to "@type"
if ("rdf:type" %in% names(pred_map)) {
  names(pred_map)[names(pred_map) == "rdf:type"] <- "@type"
  pred_map[names(pred_map) == "@type"] <- pred_map[names(pred_map) == "@type"]  %>% flatten() %>% `[[`(1)  #

}

if (!is.null(to_type)) {
  pred_map[["@type"]] <- to_type
}

if (is.null(pred_map[["@type"]])) {
  pred_map[["@type"]] <- "owl:Thing"
}

# add @id
pred_map <- c(list(`@id` = id), `@type` = pred_map[["@type"]], pred_map[!grepl("@type", names(pred_map))])

pred_map
}

# Helper Functions _________________________________________________________________
# predicate_entry: convert a tibble with a single predicate to a named list
# merge two values for the same predicate (vectors or small objects)
merge_vals <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)

  # atomic vectors -> concat + unique
  if (is.atomic(a) && is.atomic(b)) return(unique(c(a, b)))

  # both are lists of @id objects -> dedupe by @id
  is_obj_list <- function(x) is.list(x) && all(map_lgl(x, ~ is.list(.x) && !is.null(.x[["@id"]])))
  if (is_obj_list(a) && is_obj_list(b)) {
    all_objs <- c(a, b)
    # keep first occurrence of each @id
    ids <- map_chr(all_objs, ~ .x[["@id"]])
    return(all_objs[!duplicated(ids)])
  }

  # named lists -> deep-merge by key
  if (is.list(a) && is.list(b) && !is.null(names(a)) && !is.null(names(b))) {
    keys <- union(names(a), names(b))
    return(set_names(map(keys, ~ merge_vals(a[[.x]], b[[.x]])), keys))
  }

  list(a, b)
}

# ONE tibble -> named list { <predicate>: <value> }
predicate_entry <- function(df) {
  stopifnot("predicate" %in% names(df))
  p <- df %>% distinct(predicate) %>% pull()
  if (length(p) != 1L) stop("Each tibble must have a single predicate.")
  p <- p[[1]]

  rest <- df %>% select(-predicate)

  # 1) predicate-named column present (e.g. rdfs:label)
  if (p %in% names(rest)) {
    vals <- rest %>% pull(all_of(p)) %>% unique()
    return(set_names(list(vals), p))
  }

  # 2) @id present
  if ("@id" %in% names(rest)) {
    ids <- rest %>% pull(`@id`) %>% unique()
    if (length(ids) == 1L) {
      return(set_names(list(list(`@id` = ids[[1]])), p))         # single object
    } else {
      return(set_names(list(map(ids, ~ list(`@id` = .x))), p))   # list of objects
    }
  }

  # 3) fallback: pack all remaining columns as vectors
  packed <- rest %>% as.list() %>% map(unique)
  set_names(list(packed), p)
}

