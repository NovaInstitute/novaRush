#' createLedger
#'
#'@description Helper function to create a ledger in Fluree
#'@param ledgerName Character. The name of the ledger to be created.
#'@param fluree_link Character. The link to the Fluree database.
#'
#' @return character
#' @export
#' @import httr
createLedger <- function(ledgerName = "rdataset/mtcars",
                         createEndpoint = "create",
                         fluree_link = Sys.getenv("fluree_link")) {
  require(httr)
  flureeFetch(path = paste0(fluree_link, createEndpoint), method = "POST",
              body = list(`ledger/id` = ledgerName))
}
