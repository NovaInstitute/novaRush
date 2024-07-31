

### Do not manually edit this file, modify generateBodyFunctions() instead. ###



#' @title createArtistObject
#' @description function to create artist object
#' @param name character
#' @return data.frame
#' @import tidyverse
#' @import openssl
#' @export

 createArtistObject <- function(name = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    name = name, 
    '_id' = ifelse(is.null(entityId), paste0('artist'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createCommentObject <- function(message = NULL, person = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    message = message, 
    person = person, 
    '_id' = ifelse(is.null(entityId), paste0('comment'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createChatObject <- function(comments = NULL, instant = NULL, person = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    comments = if(!is.null(comments)) list(comments) else NULL, 
    instant = instant, 
    person = person, 
    '_id' = ifelse(is.null(entityId), paste0('chat'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createPersonObject <- function(fullname = NULL, favartists = NULL, favnums = NULL, follows = NULL, handle = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    fullName = fullname, 
    favArtists = if(!is.null(favartists)) list(favartists) else NULL, 
    favNums = if(!is.null(favnums)) list(favnums) else NULL, 
    follows = if(!is.null(follows)) list(follows) else NULL, 
    handle = handle, 
    '_id' = ifelse(is.null(entityId), paste0('person'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createCtxObject <- function(doc = NULL, fn = NULL, key = NULL, name = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    doc = doc, 
    fn = fn, 
    key = key, 
    name = name, 
    '_id' = ifelse(is.null(entityId), paste0('_ctx'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createSettingObject <- function(language = NULL, id = NULL, txmax = NULL, passwords = NULL, doc = NULL, consensus = NULL, ledgers = NULL, anonymous = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    language = language, 
    id = id, 
    txMax = txmax, 
    passwords = passwords, 
    doc = doc, 
    consensus = consensus, 
    ledgers = if(!is.null(ledgers)) list(ledgers) else NULL, 
    anonymous = anonymous, 
    '_id' = ifelse(is.null(entityId), paste0('_setting'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createRuleObject <- function(errormessage = NULL, collectiondefault = NULL, ops = NULL, fns = NULL, predicates = NULL, collection = NULL, doc = NULL, id = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    errorMessage = errormessage, 
    collectionDefault = collectiondefault, 
    ops = if(!is.null(ops)) list(ops) else NULL, 
    fns = if(!is.null(fns)) list(fns) else NULL, 
    predicates = if(!is.null(predicates)) list(predicates) else NULL, 
    collection = collection, 
    doc = doc, 
    id = id, 
    '_id' = ifelse(is.null(entityId), paste0('_rule'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createRoleObject <- function(ctx = NULL, rules = NULL, doc = NULL, id = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    ctx = if(!is.null(ctx)) list(ctx) else NULL, 
    rules = if(!is.null(rules)) list(rules) else NULL, 
    doc = doc, 
    id = id, 
    '_id' = ifelse(is.null(entityId), paste0('_role'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createAuthObject <- function(fuel = NULL, authority = NULL, type = NULL, doc = NULL, roles = NULL, salt = NULL, password = NULL, id = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    fuel = fuel, 
    authority = if(!is.null(authority)) list(authority) else NULL, 
    type = type, 
    doc = doc, 
    roles = if(!is.null(roles)) list(roles) else NULL, 
    salt = salt, 
    password = password, 
    id = id, 
    '_id' = ifelse(is.null(entityId), paste0('_auth'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createUserObject <- function(doc = NULL, roles = NULL, auth = NULL, username = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    doc = doc, 
    roles = if(!is.null(roles)) list(roles) else NULL, 
    auth = if(!is.null(auth)) list(auth) else NULL, 
    username = username, 
    '_id' = ifelse(is.null(entityId), paste0('_user'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createFnObject <- function(language = NULL, spec = NULL, doc = NULL, code = NULL, params = NULL, name = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    language = language, 
    spec = spec, 
    doc = doc, 
    code = code, 
    params = params, 
    name = name, 
    '_id' = ifelse(is.null(entityId), paste0('_fn'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createTagObject <- function(doc = NULL, id = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    doc = doc, 
    id = id, 
    '_id' = ifelse(is.null(entityId), paste0('_tag'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createShardObject <- function(mutable = NULL, miners = NULL, name = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    mutable = mutable, 
    miners = if(!is.null(miners)) list(miners) else NULL, 
    name = name, 
    '_id' = ifelse(is.null(entityId), paste0('_shard'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createCollectionObject <- function(partition = NULL, shard = NULL, specdoc = NULL, spec = NULL, version = NULL, doc = NULL, name = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    partition = partition, 
    shard = shard, 
    specDoc = specdoc, 
    spec = if(!is.null(spec)) list(spec) else NULL, 
    version = version, 
    doc = doc, 
    name = name, 
    '_id' = ifelse(is.null(entityId), paste0('_collection'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
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

 createPredicateObject <- function(retractduplicates = NULL, fulltext = NULL, restricttag = NULL, txspecdoc = NULL, txspec = NULL, specdoc = NULL, deprecated = NULL, encrypted = NULL, spec = NULL, restrictcollection = NULL, nohistory = NULL, component = NULL, upsert = NULL, index = NULL, multi = NULL, unique = NULL, type = NULL, doc = NULL, name = NULL, entityId = NULL, deleteObject = FALSE) {
  arguments <- as.list(match.call())[-1]
  arguments <- setdiff(names(arguments), 'deleteObject')
  if (length(arguments) < 1) {
    stop('Please provide at least one argument')
  }
  if (all(sapply(arguments, function(arg) is.null(arg) || identical(arg, FALSE)))) {
    stop('All arguments are NULL or FALSE')
  }

  entityObject <- list(
    retractDuplicates = retractduplicates, 
    fullText = fulltext, 
    restrictTag = restricttag, 
    txSpecDoc = txspecdoc, 
    txSpec = if(!is.null(txspec)) list(txspec) else NULL, 
    specDoc = specdoc, 
    deprecated = deprecated, 
    encrypted = encrypted, 
    spec = if(!is.null(spec)) list(spec) else NULL, 
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
    '_id' = ifelse(is.null(entityId), paste0('_predicate'), entityId))

  if (deleteObject) {
    entityObject$action <- 'delete'
  }
  entityObject <- Filter(Negate(is.null), entityObject) 
  return(entityObject)
}

