shorten_id <- function(id) {
  if (str_detect(id, "/")) {
    # If ID contains a slash, take the part after the last slash
    return(basename(id))
  } else {
    # Truncate to first 8 characters and add ellipsis if longer
    if (nchar(id) > 8) {
      return(paste0(str_sub(id, 1, 8), "..."))
    }
    return(id)
  }
}
