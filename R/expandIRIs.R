#' expandIRIs
#' 
#'@note Largely written by ChatGPT. Does not work yet.
#'@description Expands the IRIs in a JSON-LD object list according to its 
#'  "@context" values.
#'@param x List A list representation of a JSON-LD object. If the list contains an 
#'  "@context" item, this function will expand all IRIs in the remainder of the
#'  list to full IRIs, and optionally return the list without the "@context" item.
#'@param keepCtx Logical Should the "@context" item be included in the result or not? 
#'  Defaults to FALSE.
#'@return If x contained an "@context" item, a list with fully expanded IRIs and 
#'  (optionally) no "@context" item; otherwise, x unchanged.
#'@export
#'
expandIRIs <- function(x, keepCtx = FALSE) {
  
  # ---- helpers ---------------------------------------------------------------
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  is_absolute_iri <- function(s) {
    is.character(s) && length(s) == 1L && grepl("^[A-Za-z][A-Za-z0-9+.-]*:", s, perl = TRUE)
  }
  
  # very light-weight base resolver (does not handle ../ or ./)
  join_base <- function(base, rel) {
    
    if (is.null(base) || 
        is.na(base) || 
        base == "" || 
        is_absolute_iri(rel) || 
        startsWith(rel, "_:")) { 
      return(rel) 
    }
    
    # strip leading slash on rel to avoid losing path when base ends with slash
    if (startsWith(rel, "/")) rel <- substring(rel, 2L)
    sep <- if (endsWith(base, "/")) "" else "/"
    paste0(base, sep, rel)
    
  }
  
  # extract and normalize context: prefixes, term mappings, base, vocab
  normalize_context <- function(ctx) {
    
    # ctx may be a single map or a list (array) of maps/strings; 
    # strings (remote contexts) ignored
    
    if (is.null(ctx)) {
      return(list(prefixes = list(), 
                  terms = list(), 
                  base = NULL, 
                  vocab = NULL)) 
    }
    
    if (!is.list(ctx) || is.null(names(ctx))) {
      # if it's an array-like list, merge items
      if (is.list(ctx) && is.null(names(ctx))) {
        merged <- Reduce(
          f = function(a, b) { merge_contexts(a, b) },
          x = ctx, 
          init = list(prefixes = list(), 
                      terms = list(), 
                      base = NULL, 
                      vocab = NULL))
        return(merged)
      }
    }
    
    # single map case
    prefixes <- list()
    terms    <- list()
    base     <- NULL
    vocab    <- NULL
    
    for (k in names(ctx)) {
      v <- ctx[[k]]
      if (k == "@base")  { base  <- as.character(v)[1]; next }
      if (k == "@vocab") { vocab <- as.character(v)[1]; next }
      if (is.character(v)) {
        # Either a prefix def (e.g. "schema": "http://schema.org/") or a term -> IRI mapping
        if (grepl("/$|#$", v)) {
          prefixes[[k]] <- v
        } else {
          terms[[k]] <- v
        }
      } else if (is.list(v)) {
        # term definition object; support the common "@id" pattern
        if (!is.null(v[["@id"]])) {
          terms[[k]] <- as.character(v[["@id"]])[1]
        }
        # (we could add support for @type, @container etc., but keep this minimal)
      }
    }
    
    list(prefixes = prefixes, terms = terms, base = base, vocab = vocab)
  }
  
  merge_contexts <- function(a, b_raw) {
    
    b <- normalize_context(b_raw)
    
    return(list(
      prefixes = modifyList(a$prefixes %||% list(), b$prefixes %||% list()),
      terms    = modifyList(a$terms    %||% list(), b$terms    %||% list()),
      base     = b$base  %||% a$base,
      vocab    = b$vocab %||% a$vocab))
    
  }
  
  expand_compact_iri <- function(s, ctx) {
    
    # s like "schema:Person" -> expand via prefix
    parts <- strsplit(x = s, split = ":", fixed = TRUE)[[1]]
    if(length(parts) < 2) { return(s) }
    
    prefix <- parts[1]
    rest   <- paste(parts[-1], collapse = ":")
    
    if (!is.null(ctx$prefixes[[prefix]])) {
      base <- ctx$prefixes[[prefix]]
      # If base ends with # or /, just concatenate, else add separator
      sep <- if (endsWith(base, "/") || endsWith(base, "#")) "" else "/"
      return(paste0(base, sep, rest))
    }
    
    return(s)
  }
  
  expand_term_or_iri <- function(
    s, 
    ctx, 
    as_predicate = FALSE, 
    allow_vocab = TRUE) {
    
    if (!is.character(s) || length(s) != 1L) return(s)
    if (s %in% c("@id", "@type", "@context")) return(s)
    if (is_absolute_iri(s) || startsWith(s, "_:")) return(s)
    
    # compact IRI?
    if (grepl(pattern = "^[A-Za-z][A-Za-z0-9+.-]*:", x = s)) {
      return(expand_compact_iri(s, ctx))
    }
    
    # direct term mapping?
    if (!is.null(ctx$terms[[s]])) {
      v <- ctx$terms[[s]]
      # mapped target may itself be compact or relative – expand again
      return(expand_term_or_iri(
        s = v, 
        ctx = ctx, 
        as_predicate = as_predicate, 
        allow_vocab = FALSE))
    }
    
    # predicate fallback to @vocab
    if (as_predicate && allow_vocab && !is.null(ctx$vocab) && nzchar(ctx$vocab)) {
      base <- ctx$vocab
      sep  <- if (endsWith(base, "/") || endsWith(base, "#")) "" else "/"
      return(paste0(base, sep, s))
    }
    
    # last chance: @base for relative IRIs (only when not a predicate term)
    if (!as_predicate && !is.null(ctx$base) && nzchar(s)) {
      return(join_base(ctx$base, s))
    }
    
    return(s)
  }
  
  expand_key <- function(k, ctx) {
    # Keys are predicates (except JSON-LD keywords)
    if (k %in% c("@id", "@type", "@context", "@value", "@language", "@list", "@set", "@reverse")) return(k)
    expand_term_or_iri(k, ctx, as_predicate = TRUE)
  }
  
  expand_value <- function(key, v, ctx) {
    # Expand values only where JSON-LD expects IRIs: @id, @type, or objects with @id/@type.
    
    if (is.null(v)) return(v)
    
    if (is.list(v)) {
      if (is.null(names(v))) { # array
        return(lapply(X = v, FUN = expand_value, key = key, ctx = ctx))
      } else { # object
        return(expand_object(v, ctx))
      }
    }
    
    # scalar
    if (identical(key, "@id")) {
      return(expand_term_or_iri(as.character(v)[1], ctx, as_predicate = FALSE))
    }
    if (identical(key, "@type")) {
      if (is.character(v) && length(v) == 1) {
        return(expand_term_or_iri(v, ctx, as_predicate = TRUE))
      } else if (is.vector(v)) {
        return(
          vapply(X = v, FUN = function(s) { 
            expand_term_or_iri(
              as.character(s), 
              ctx, 
              as_predicate = TRUE) 
          }, 
          FUN.VALUE = "", 
          USE.NAMES = FALSE))
      }
    }
    
    # For typical predicate values, leave literals alone.
    return(v)
  }
  
  expand_object <- function(obj, ctx) {
    # If the object carries its own @context, merge (child overrides / extends)
    
    local_ctx_raw <- obj[["@context"]]
    eff_ctx <- if (!is.null(local_ctx_raw)) merge_contexts(ctx, local_ctx_raw) else ctx
    
    # First pass: expand keys
    out <- list()
    for (k in names(obj)) {
      if (k == "@context" && !keepCtx) next
      newk <- expand_key(k, eff_ctx)
      out[[newk]] <- expand_value(newk, obj[[k]], eff_ctx)
    }
    
    return(out)
  }
  
  # ---- drive ---------------------------------------------------------------
  ctx <- normalize_context(x[["@context"]])
  out <- expand_object(x, ctx)
  return(out)
}

# expandIRIs <- function(x, keepCtx = FALSE) {
#   
#   if (!is.list(x)) {
#     stop("x must be a list.")
#   }
#   if ("@context" %in% names(x)) {
#     return(x)
#   }
#   
#   ctx <- x[["@context"]]
#   x[["@context"]] <- NULL
#   ctx[c("@base", "")]
#   ctx <- ctx[order(nchar(names(ctx)), decreasing = FALSE)]
#   
#   for (nsa in names(ctx)) {
#     names(x) <- gsub(
#       pattern = paste0("^", nsa, ":"), 
#       replacement = ctx[[nsa]],
#       x = names(x))
#     x[["@type"]] <- gsub(
#       pattern = paste0("^", nsa, ":"), 
#       replacement = ctx[[nsa]],
#       x = x[["@type"]])
#   }
#   
#   return(x)
# }