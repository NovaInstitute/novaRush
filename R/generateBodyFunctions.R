#' generateBodyFunctions
#' @description Helper function to generate the body of a function
#' @param ledgerName Character. The name of the ledger.
#' @param signQuery Logical. Should the query be signed?
#' @return character
#' @import dplyr
#'
#' @export

generateBodyFunctions <- function(ledgerName = "authority/test", signQuery = FALSE) {

  dfCollections <- getAllEntityRecords(ledgerName = ledgerName, entityName = "_collection", signQuery = signQuery)
  names(dfCollections) <- novaUtils::fixname(names(dfCollections))

  # Clear the file
  cat("\n\n### Do not manually edit this file, modify generateBodyFunctions() instead. ###\n\n\n",
      file = "R/flureeObjectFunctions.R")

  dfAllPredicates <- getAllEntityRecords(ledgerName = ledgerName, entityName = "_predicate", signQuery = signQuery) %>%
    mutate(`_predicate/multi` = ifelse(is.na(`_predicate/multi`), FALSE, `_predicate/multi`))

  # Generate the body of the function
  for (j in 1:nrow(dfCollections)) {
    collectionName <- dfCollections$collection_name[j]
    dfData <- getAllEntityRecords(ledgerName, collectionName, signQuery = signQuery)
    inData <- dfAllPredicates[["_predicate/name"]] %in% names(dfData)

    if (!all(inData)) {
      inData <- TRUE
    }

    dfEntityPredicates <- dfAllPredicates[which(grepl(paste0("^", collectionName), dfAllPredicates[["_predicate/name"]]) & inData), ]
    isMulti <- dfEntityPredicates[["_predicate/multi"]]

    if (nrow(dfEntityPredicates) == 0) {
      next
    }

    originalNames <- gsub(paste(collectionName, "/", sep = ""), "", dfEntityPredicates[["_predicate/name"]])
    argumentNames <- novaUtils::fixname(originalNames)
    functionName <- paste0("create", Hmisc::capitalize(gsub("_", "", collectionName)), "Object")
    idxx <- length(originalNames) + 1
    originalNames[idxx] <- "'_id'"

    cat(novaGPT::generateDocumentationText(functionName, functionArgs = argumentNames),
        paste0(functionName, " <- function(", paste(argumentNames, "= NULL", collapse = ", "), ", entityId = NULL, deleteObject = FALSE) {\n",
               "  arguments <- as.list(match.call())[-1]\n",
               "  arguments <- setdiff(names(arguments), 'deleteObject')\n",
               "  if (length(arguments) < 1) {\n",
               "    stop('Please provide at least one argument')\n",
               "  }\n",
               "  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {\n",
               "    stop('All arguments are NULL or FALSE')\n",
               "  }\n\n",
               "  entityObject <- list(", paste0("\n    ", originalNames,
                                                           " = ", c(ifelse(isMulti, paste0("if(!is.null(",argumentNames,")) list(", argumentNames, ") else NULL"), argumentNames),
                                                                    paste0("ifelse(is.null(entityId), paste0('", collectionName, "'), entityId)")), sep = "", collapse = ", "), ")\n\n",
               "  if (deleteObject) {\n",
               "    entityObject$action <- 'delete'\n",
               "  }\n",
               "  entityObject <- Filter(Negate(is.null), entityObject) \n",
               "  return(entityObject)\n",
               "}\n\n"),
        append = TRUE, file = "R/flureeObjectFunctions.R")
  }
}

