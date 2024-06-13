
#' @title createSettingObject
#' @description function to create setting object
#' @param ledgers character 
#' @param id character 
#' @param language character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createSettingObject <- function(ledgers, id, language){

entityObject <- tibble(
  '_id' = '_setting',
  ledgers = ledgers,
  id = id,
  language = language)

 return(entityObject)
}


#' @title createRuleObject
#' @description function to create rule object
#' @param id character 
#' @param doc character 
#' @param collection character 
#' @param predicates character 
#' @param fns character 
#' @param ops character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createRuleObject <- function(id, doc, collection, predicates, fns, ops){

entityObject <- tibble(
  '_id' = '_rule',
  id = id,
  doc = doc,
  collection = collection,
  predicates = predicates,
  fns = fns,
  ops = ops)

 return(entityObject)
}


#' @title createRoleObject
#' @description function to create role object
#' @param id character 
#' @param doc character 
#' @param rules character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createRoleObject <- function(id, doc, rules){

entityObject <- tibble(
  '_id' = '_role',
  id = id,
  doc = doc,
  rules = rules)

 return(entityObject)
}


#' @title createAuthObject
#' @description function to create auth object
#' @param id character 
#' @param doc character 
#' @param roles character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createAuthObject <- function(id, doc, roles){

entityObject <- tibble(
  '_id' = '_auth',
  id = id,
  doc = doc,
  roles = roles)

 return(entityObject)
}


#' @title createUserObject
#' @description function to create user object
#' @param username character 
#' @param auth character 
#' @param roles character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createUserObject <- function(username, auth, roles){

entityObject <- tibble(
  '_id' = '_user',
  username = username,
  auth = auth,
  roles = roles)

 return(entityObject)
}


#' @title createFnObject
#' @description function to create fn object
#' @param name character 
#' @param code character 
#' @param doc character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createFnObject <- function(name, code, doc){

entityObject <- tibble(
  '_id' = '_fn',
  name = name,
  code = code,
  doc = doc)

 return(entityObject)
}


#' @title createTagObject
#' @description function to create tag object
#' @param id character 
#' @param doc character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createTagObject <- function(id, doc){

entityObject <- tibble(
  '_id' = '_tag',
  id = id,
  doc = doc)

 return(entityObject)
}


#' @title createCollectionObject
#' @description function to create collection object
#' @param name character 
#' @param doc character 
#' @param version character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createCollectionObject <- function(name, doc, version){

entityObject <- tibble(
  '_id' = '_collection',
  name = name,
  doc = doc,
  version = version)

 return(entityObject)
}


#' @title createPredicateObject
#' @description function to create predicate object
#' @param name character 
#' @param doc character 
#' @param type character 
#' @param restrictcollection character 
#' @param unique character 
#' @param multi character 
#' @param restricttag character 
#' @param index character 
#' @param upsert character 
#' @return data.frame
#' @import tidyverse
#' @import openssl 
#' @export

 createPredicateObject <- function(name, doc, type, restrictcollection, unique, multi, restricttag, index, upsert){

entityObject <- tibble(
  '_id' = '_predicate',
  name = name,
  doc = doc,
  type = type,
  restrictCollection = restrictcollection,
  unique = unique,
  multi = multi,
  restrictTag = restricttag,
  index = index,
  upsert = upsert)

 return(entityObject)
}

