#' flureeFetch
#' @description Helper function to fetch data from Fluree
#' @param path Character. The path to the Fluree database.
#' @param body List. The body of the request.
#' @param fluree_link Character. The link to the Fluree database.
#'
#' @return character
#' @export
#' @import httr


# Helper function to fetch data from Fluree
flureeFetch <- function(path, method, body) {
  res <- VERB(
    method,
    paste0(path),
    body = body,
    encode = "json",
    content_type_json()
  )
  delay(1000)
  response <- list()
  response[["code"]] <- res$status_code
  response[["content"]] <- content(res, "text")
  json_reponse <- jsonlite::toJSON(response, auto_unbox = TRUE)
  print(json_reponse)
  return(json_reponse)
}

# Helper function to delay
delay <- function(ms) {
  Sys.sleep(ms / 1000)
}

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
                                 fluree_link = Sys.getenv("fluree_link")) {
  require(httr)
  flureeFetch(path = paste0(fluree_link, "new-ledger"), method = "POST",
               body = list(`ledger/id` = ledgerName))
}

#' prepareSchema
#' @description Helper function to prepare a schema in Fluree
#' @param predicateNames List. The names of the predicates.
#' @param dfData Dataframe. The data to be added.
#' @return character
#' @export
#' @import httr
prepareSchema <- function(predicateNames = NULL, dfData = NULL){
  if(is.null(predicateNames) & is.null(dfData)){
    stop("Please provide a list of predicate names or a data frame")
  }

  if(is.null(predicateNames)){
    predicateNames <- sapply(dfData, function(x){
      switch(class(x),
             "character" = "string",
             "numeric" = "double",
             "logical" = "boolean",
             "factor" = "string",
             "Date" = "date",
             "POSIXct" = "date",
             "POSIXt" = "date",
             "integer" = "integer",
             "list" = "array",
             "default" = "string")
    })
  }else{
    if(is.null(names(predicateNames))){
      stop("predicateNames must be a named list, containing the predicate names and their types")
    }
  }
  schema_list <- list()
  for(i in 1:length(predicateNames)){
    schema_list[[i]] <- list(
      "_id" = "_predicate",
      "name" = paste0(basename(path), "/", names(predicateNames[i])),
      "type" = predicateNames[[i]]
    )
  }
  return(schema_list)
}

#' createSchema
#' @description Helper function to create a schema in Fluree
#' @param path Character. The path to the Fluree database.
#' @param schema_list List. The schema to be created (predicates).
#' @param fluree_link Character. The link to the Fluree database.
#'
#' @return character
#' @export
#' @import httr
createSchema <- function(path,
                         schema_list,
                         fluree_link = Sys.getenv("fluree_link")) {
  link <- paste0(fluree_link, path, "transact")
  flureeFetch(path = link,
               method = "POST",
               body = jsonlite::toJSON(schema_list, auto_unbox = TRUE))
}


#' prepareData
#'
#' @description Helper function to add data in Fluree
#' @param dfData Dataframe or tibble with the data to be added.
#' @param collectionName Character. The name of the collection.
#' @return character
#' @export
#' @import httr

prepareData <- function(dfData = NULL, collectionName = NULL){
  if(is.null(dfData)){
    stop("Please provide a data frame or a tibble")
  }
  if(is.null(collectionName)){
    stop("Please provide a collection name")
  }

  data_list <- list()
  for(i in 1:length(dfData)){
    dfRow <- dfData[i,]
    dfRow[["_id"]] <- collectionName
    data_list[[i]] <- dfRow
  }
  return(data_list)
}



#' insertData
#'
#' @description Helper function to create a schema in Fluree
#' @param path Character. The path to the Fluree database.
#' @param data_list List. The schema to be created (predicates).
#' @param fluree_link Character. The link to the Fluree database.
#' @return character
#' @export
#' @import httr
insertData <- function(path,
                       data_list,
                       fluree_link = Sys.getenv("fluree_link")) {
  require(rjson)
  flureeFetch(path = paste0(fluree_link, path, "transact"),
               method = "POST",
               body = toJSON(data_list))
}


#' makeQuerySignature
#' @description Helper function to sign a query in Fluree
#' @param ledgerName Character. The name of the ledger.
#' @param privateKey Character. The private key.
#' @param queryString Character. The query string.
#' @param authId Character. The authId.
#' @return character
#' @export
makeQuerySignature <- function(ledgerName,
                              privateKey = Sys.getenv("privateKey") ,
                              queryString,
                              authId = Sys.getenv("authId")){
  # Construct the inline Node.js code
  jsCode <- paste0("
    const { signQuery } = require('@fluree/crypto-utils');
    console.log(signQuery(
      '", privateKey, "',
      '", queryString, "',
      'query',
      '", ledgerName, "',
      '", authId, "'
    ))")

  # Escape double quotes for inline execution
  jsCode <- gsub("\"", "\\\"", jsCode)

  # Execute the JavaScript code inline with node
  signedQuery <- system(paste('node -e "', jsCode, '"'), intern = TRUE)

  if(any(grepl("using Node.js", signedQuery))){
    signedQuery <- signedQuery[-1]
  }
  signedQuery <- paste(signedQuery, collapse = "")
  return(signedQuery)
}

#' generateKeyPair
#' @description Helper function to generate a key pair in Fluree
#' @return character
#' @export
generateKeyPair <- function(){
  # Construct the inline Node.js code
  jsCode <- paste0("
   const { generateKeyPair, getSinFromPublicKey } = require('@fluree/crypto-utils');
  const { publicKey: authorityPubKey, privateKey: authorityPrivKey } = generateKeyPair();
  const authorityAuthId = getSinFromPublicKey(authorityPubKey);
  authority = {
      authId: authorityAuthId,
      pubKey: authorityPubKey,
      privKey: authorityPrivKey,
    };
  console.log(JSON.stringify(authority))")

  # Escape double quotes for inline execution
  jsCode <- gsub("\"", "\\\"", jsCode)

  # Execute the JavaScript code inline with node
  keyPair <- system(paste('node -e "', jsCode, '"'), intern = TRUE)

  if(any(grepl("using Node.js", keyPair))){
    keyPair <- keyPair[-1]
  }
  keyPair <- paste(keyPair, collapse = "")
  return(keyPair)
}

createAuthObject <- function(ledgerName, authId, authDoc){
  authObj <- data.frame(
    "_id" = "_auth",
    "id" = authId,
    "doc" = authDoc,
    stringsAsFactors = FALSE,
    check.names = FALSE)
  # Create the user
  # response <- flureeFetch(path = paste0(Sys.getenv("fluree_link"), ledgerName, "/transact/"),
  #                           method = "POST",
  #                           body = userObj)
  return(authObj)
}

flureeTransact <- function(ledgerName, transactObj, signQuery = TRUE){
  require(httr)
  require(jsonlite)

  # Define the URL
  response <- flureeFetch(path = paste0(Sys.getenv("fluree_link"), ledgerName, "/transact/"),
                            method = "POST",
                            body = transactObj)
  return(response)
}


#' getAllEntityRecords
#' @description Helper function to fetch all records of an entity from Fluree
#' @param ledgerName Character. The name of the ledger.
#' @param entityName Character. The name of the entity.
#' @return character
#' @export
#' @examples :
#' dfCollections <- getAllEntityRecords("authority/test", "_collection")
#' dfUser <- getAllEntityRecords("authority/test", "_user")
#' dfAuth <- getAllEntityRecords("authority/test", "_auth")
#' dfPredicate <- getAllEntityRecords("authority/test", "_predicate")

getAllEntityRecords <- function(ledgerName, entityName, signQuery = TRUE){
  require(httr)
  require(tibble)
  # Define the URL
  url <- paste(Sys.getenv("fluree_link"), ledgerName,"/query", sep = "")
  # Define the query object
  queryObj <- list(
    select = list("*"),
    from = entityName
  )
    # Convert the query object to JSON
  body <- jsonlite::toJSON(queryObj, auto_unbox = TRUE)
  if(signQuery){
    # Sign the query
    body <- makeQuerySignature(ledgerName = ledgerName, queryString =  body)
  }
  # Make the POST request
  response <- POST(
    url,
    add_headers("Content-Type" = "application/json"),
    body = body
  )
  # Parse JSON data
  data_list <- fromJSON(content(response, "text"))
  # Convert list to tibble
  data_tibble <- as_tibble(data_list)
  return(data_tibble)
}

#' generateBodyFunctions
#' @description Helper function to generate the body of a function
#' @param ledgerName Character. The name of the ledger.
generateBodyFunctions <- function(ledgerName =  "authority/test", signQuery = FALSE){
  dfCollections <- getAllEntityRecords(ledgerName = ledgerName, entityName = "_collection", signQuery = FALSE)
  names(dfCollections) <- novaUtils::fixname(names(dfCollections))

  #clear the file
  cat("", file = "R/FlureeObjectFunctions.R")
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
        append = TRUE, file = "R/FlureeObjectFunctions.R")
}


}







