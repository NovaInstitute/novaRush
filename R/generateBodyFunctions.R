#' generateBodyFunctions
#' @description Helper function to generate the body of a function
#' @param ledgerName Character. The name of the ledger.
#' @param signQuery Logical. Should the query be signed?
#' @return character
#'
#' @export
generateBodyFunctions <- function(ledgerName =  "authority/test", signQuery = FALSE){
  dfCollections <- getAllEntityRecords(ledgerName = ledgerName, entityName = "_collection", signQuery = FALSE)
  names(dfCollections) <- novaUtils::fixname(names(dfCollections))

  #clear the file
  cat("", file = "R/flureeObjectFunctions.R")
  # Generate the body of the function
  for (j in 1:nrow(dfCollections)) {
    collectionName <- dfCollections$collection_name[j]
    dfData <- getAllEntityRecords(ledgerName, collectionName, signQuery = signQuery)
    if(nrow(dfData) == 0){
      next
    }
    originalNames <- gsub(paste(collectionName,"/", sep = ""), "", names(dfData))
    argumentNames <- novaUtils::fixname(originalNames)[-1]
    functionName <- paste0("create", Hmisc::capitalize(gsub("_", "", collectionName)), "Object" ,sep = "")
    originalNames[1] <- "'_id'"
    cat(novaGPT::generateDocumentationText(functionName, functionArgs = argumentNames), paste0(functionName, " <- function(", paste(argumentNames, collapse = ", "), "){\n\nentityObject <- tibble(", paste0("\n  ", originalNames,
                                                                                                                                                                                                             " = ", c(paste0("'", collectionName, "'"),
                                                                                                                                                                                                                      argumentNames), sep = "",
                                                                                                                                                                                                             collapse = ","),")\n\n return(entityObject)\n}\n\n"),
        append = TRUE, file = "R/flureeObjectFunctions.R")
  }
}
