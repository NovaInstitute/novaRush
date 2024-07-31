

###Do don't manually edit this file, modify generateBodyFunctions() instead.###



#' @title createArtistObject
#' @description function to create artist object
#' @param name character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createArtistObject <- function(name = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  name = name,
  '_id' = 'artist')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createCommentObject
#' @description function to create comment object
#' @param message character
#' @param person character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createCommentObject <- function(message = NULL, person = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  message = message,
  person = person,
  '_id' = 'comment')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createChatObject
#' @description function to create chat object
#' @param comments character
#' @param instant character
#' @param person character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createChatObject <- function(comments = NULL, instant = NULL, person = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  comments = list(comments),
  instant = instant,
  person = person,
  '_id' = 'chat')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createPersonObject
#' @description function to create person object
#' @param fullname character
#' @param favartists character
#' @param favnums character
#' @param follows character
#' @param handle character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createPersonObject <- function(fullname = NULL, favartists = NULL, favnums = NULL, follows = NULL, handle = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  fullName = list(fullname),
  favArtists = list(favartists),
  favNums = list(favnums),
  follows = list(follows),
  handle = handle,
  '_id' = 'person')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createCtxObject
#' @description function to create ctx object
#' @param doc character
#' @param fn character
#' @param key character
#' @param name character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createCtxObject <- function(doc = NULL, fn = NULL, key = NULL, name = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  doc = doc,
  fn = fn,
  key = key,
  name = name,
  '_id' = '_ctx')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createSettingObject
#' @description function to create setting object
#' @param language character
#' @param id character
#' @param txmax character
#' @param passwords character
#' @param doc character
#' @param consensus character
#' @param ledgers character
#' @param anonymous character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createSettingObject <- function(language = NULL, id = NULL, txmax = NULL, passwords = NULL, doc = NULL, consensus = NULL, ledgers = NULL, anonymous = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  language = language,
  id = id,
  txMax = txmax,
  passwords = passwords,
  doc = doc,
  consensus = consensus,
  ledgers = list(ledgers),
  anonymous = anonymous,
  '_id' = '_setting')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createRuleObject
#' @description function to create rule object
#' @param errormessage character
#' @param collectiondefault character
#' @param ops character
#' @param fns character
#' @param predicates character
#' @param collection character
#' @param doc character
#' @param id character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createRuleObject <- function(errormessage = NULL, collectiondefault = NULL, ops = NULL, fns = NULL, predicates = NULL, collection = NULL, doc = NULL, id = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  errorMessage = errormessage,
  collectionDefault = collectiondefault,
  ops = list(ops),
  fns = list(fns),
  predicates = list(predicates),
  collection = collection,
  doc = doc,
  id = id,
  '_id' = '_rule')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createRoleObject
#' @description function to create role object
#' @param ctx character
#' @param rules character
#' @param doc character
#' @param id character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createRoleObject <- function(ctx = NULL, rules = NULL, doc = NULL, id = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  ctx = list(ctx),
  rules = list(rules),
  doc = doc,
  id = id,
  '_id' = '_role')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createAuthObject
#' @description function to create auth object
#' @param fuel character
#' @param authority character
#' @param type character
#' @param doc character
#' @param roles character
#' @param salt character
#' @param password character
#' @param id character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createAuthObject <- function(fuel = NULL, authority = NULL, type = NULL, doc = NULL, roles = NULL, salt = NULL, password = NULL, id = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  fuel = fuel,
  authority = list(authority),
  type = type,
  doc = doc,
  roles = list(roles),
  salt = salt,
  password = password,
  id = id,
  '_id' = '_auth')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createUserObject
#' @description function to create user object
#' @param doc character
#' @param roles character
#' @param auth character
#' @param username character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createUserObject <- function(doc = NULL, roles = NULL, auth = NULL, username = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  doc = doc,
  roles = list(roles),
  auth = list(auth),
  username = username,
  '_id' = '_user')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createFnObject
#' @description function to create fn object
#' @param language character
#' @param spec character
#' @param doc character
#' @param code character
#' @param params character
#' @param name character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createFnObject <- function(language = NULL, spec = NULL, doc = NULL, code = NULL, params = NULL, name = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  language = language,
  spec = spec,
  doc = doc,
  code = code,
  params = params,
  name = name,
  '_id' = '_fn')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createTagObject
#' @description function to create tag object
#' @param doc character
#' @param id character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createTagObject <- function(doc = NULL, id = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  doc = doc,
  id = id,
  '_id' = '_tag')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createShardObject
#' @description function to create shard object
#' @param mutable character
#' @param miners character
#' @param name character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createShardObject <- function(mutable = NULL, miners = NULL, name = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  mutable = mutable,
  miners = list(miners),
  name = name,
  '_id' = '_shard')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createCollectionObject
#' @description function to create collection object
#' @param partition character
#' @param shard character
#' @param specdoc character
#' @param spec character
#' @param version character
#' @param doc character
#' @param name character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createCollectionObject <- function(partition = NULL, shard = NULL, specdoc = NULL, spec = NULL, version = NULL, doc = NULL, name = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  partition = partition,
  shard = shard,
  specDoc = specdoc,
  spec = list(spec),
  version = version,
  doc = doc,
  name = name,
  '_id' = '_collection')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}


#' @title createPredicateObject
#' @description function to create predicate object
#' @param retractduplicates character
#' @param fulltext character
#' @param restricttag character
#' @param txspecdoc character
#' @param txspec character
#' @param specdoc character
#' @param deprecated character
#' @param encrypted character
#' @param spec character
#' @param restrictcollection character
#' @param nohistory character
#' @param component character
#' @param upsert character
#' @param index character
#' @param multi character
#' @param unique character
#' @param type character
#' @param doc character
#' @param name character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createPredicateObject <- function(retractduplicates = NULL, fulltext = NULL, restricttag = NULL, txspecdoc = NULL, txspec = NULL, specdoc = NULL, deprecated = NULL, encrypted = NULL, spec = NULL, restrictcollection = NULL, nohistory = NULL, component = NULL, upsert = NULL, index = NULL, multi = NULL, unique = NULL, type = NULL, doc = NULL, name = NULL, deleteObject = FALSE){

   arguments <- as.list(match.call())[-1]

   arguments <- setdiff(names(arguments), 'deleteObject')

   if(length(arguments)<1){
     stop('Please provide at least one argument')
   }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

entityObject <- tibble::tibble(
  retractDuplicates = retractduplicates,
  fullText = fulltext,
  restrictTag = restricttag,
  txSpecDoc = txspecdoc,
  txSpec = list(txspec),
  specDoc = specdoc,
  deprecated = deprecated,
  encrypted = encrypted,
  spec = list(spec),
  restrictCollection = restrictcollection,
  noHistory = nohistory,
  component = component,
  upsert = upsert,
  index = index,
  multi = multi,
  unique = unique,
  type = type,
  doc = doc,
  name = name,
  '_id' = '_predicate')

 if(deleteObject){

  entityObject$action <- 'delete'
 }
 return(entityObject)
}

