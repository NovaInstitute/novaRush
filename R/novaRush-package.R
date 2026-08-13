#' @keywords internal
"_PACKAGE"

# tidyr, dplyr and magrittr are in Depends, so they are attached for every user and
# the older code calls them unqualified. They still have to be imported for the case
# where the namespace is loaded but not attached. Moving them to Imports outright
# means qualifying every bare call in R/ - tracked separately.
#' @importFrom dplyr distinct group_by mutate if_else select pull
#' @importFrom tidyr nest
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom utils modifyList
NULL

# Column names used in non-standard evaluation by createBody() and the transaction
# helpers. Declared so R CMD check does not read them as undefined globals.
utils::globalVariables(c("predicate", "subject", "value", "json"))
