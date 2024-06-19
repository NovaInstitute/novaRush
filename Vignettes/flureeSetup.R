library(rjson)
library(httr)
library(jsonlite)
library(tibble)
library(plyr)
library(dplyr)

Sys.setenv(fluree_link = "http://localhost:8090/fdb/")
Sys.setenv(privateKey = "0c7ebd0dcbdb5796ff0175757724cffeaa948794dc0cb649b2eb525e9e70e6cb")
Sys.setenv(authId = "Tf8yFnwVxvekbvwXwsEzgwUVz4YBZMgjHEL")

#Step 1: Start docker container with: docker run -d --name fluree-ledger --network fluree-network  -e fdb-mode=dev -e fdb-open-api=false -p 8090:8090 fluree/ledger
#Step 2: To enable authentication, set the environment variable fdb-auth=true: sudo docker exec -it containerID bash, apt-get update && apt-get install vim nano, vim /opt/fluree/fdb/config/fdb.properties, fdb-auth=true
#Step 3: Install nodejs and npm in the root directory of this project
#Step 4: Install required packages @fluree/crypto-utils,

# create auth account
# transactObj <- createAuthObject(ledgerName = "authority/test",
#                                 authId = Sys.getenv("authId"),
#                                 authDoc = "Test 123")
# flureeTransact("authority/test", transactObj)

#creating collections 'tables' in fluree
flureeTransact(ledgerName = "authority/test",
               transactObject = createCollectionObject(name  = "person",
                                                       doc = "Collection to for all persons",
                                                       version = 1),
               signQuery = FALSE)

flureeTransact(ledgerName = "authority/test",
               transactObject = createCollectionObject(name  = "chat",
                                                       doc = "Collection to for all chats",
                                                       version = 1),
               signQuery = FALSE)

flureeTransact(ledgerName = "authority/test",
               transactObject = createCollectionObject(name  = "comment",
                                                       doc = "Collection to for all comments",
                                                       version = 1),
               signQuery = FALSE)

flureeTransact(ledgerName = "authority/test",
               transactObject = createCollectionObject(name  = "artist",
                                                       doc = "Collection to for all artists",
                                                       version = 1),
               signQuery = FALSE)

#create predicates 'columns' in fluree for person
 lsPredicates <- list(createPredicateObject(name = "person/handle",
                        type = "string",
                        doc = "Handle of the person",
                        unique = TRUE),

  createPredicateObject(name = "person/follows",
                        restrictcollection = "artist",
                        type = "ref", multi = TRUE,
                        doc = "follows of the person"),

  createPredicateObject(name = "person/favNums",
                        type = "int", multi = TRUE,
                        doc = "favNums of the person"),

  createPredicateObject(name = "person/favArtists",
                        type = "ref", multi = TRUE,
                        restrictcollection = "artist",
                        doc = "favourite artists of the person"))
dfPredicates <- do.call("rbind.fill", lsPredicates) %>% jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfPredicates,
               signQuery = FALSE)

#create predicates 'columns' in fluree for chat
 lsPredicates <- list(createPredicateObject(name = "chat/person",
                        type = "ref",
                        restrictcollection = "person",
                        doc = "the person chatting"),

  createPredicateObject(name = "chat/instant",
                        type = "instant",
                        doc = "instant of the chat"),

  createPredicateObject(name = "chat/comments",
                        type = "string", multi = TRUE,
                        restrictcollection = "comment",
                        doc = "comments of the chat"))

dfPredicates <- do.call("rbind.fill", lsPredicates) %>% jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfPredicates,
               signQuery = FALSE)

# create predicates 'columns' in fluree for comment
lsPredicates <- list(createPredicateObject(name = "comment/person",
                                             type = "ref",
                                             restrictcollection = "person",
                                             doc = "person commenting"),

                        createPredicateObject(name = "comment/message",
                        type = "string",
                        doc = "message of the comment"))
dfPredicates <- do.call("rbind.fill", lsPredicates) %>% jsonlite::toJSON(auto_unbox = TRUE)
flureeTransact(ledgerName = "authority/test",
                 transactObject = dfPredicates,
                 signQuery = FALSE)

  # create predicates 'columns' in fluree for artist
lsPredicates <- createPredicateObject(name = "artist/name",
                                             type = "string", unique = TRUE,
                                             doc = "name of the artist") %>% jsonlite::toJSON(auto_unbox = TRUE)


flureeTransact(ledgerName = "authority/test",
                transactObject = lsPredicates,
                signQuery = FALSE)

getAllEntityRecords(ledgerName = "authority/test", entityName = "_collection", signQuery = TRUE)
ds <- getAllEntityRecords("authority/test", "_predicate", signQuery = TRUE)


lsPeople <- list(createPersonObject(fullname = "Jane Doe", handle = "jdoe"),
createPersonObject(fullname = "Zach Smith", handle = "zsmith"))

dfPeople <- do.call("rbind.fill", lsPeople) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfPeople,
               signQuery = FALSE)


#Adding Sample Data - fullName, handle
lsPredicates  <- createPredicateObject(name = "person/fullName",
                      type = "string", multi = TRUE,
                      doc = "favNums of the person")
dfPredicates <- lsPredicates %>% jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfPredicates,
               signQuery = FALSE)

#Write a transaction to create the 3 new people with the following information
# A person with the full name, Alton Brown, handle, aBrown, and favorite numbers: 89 and 7
# A person with the full name, Oprah Winfrey, handle, oWinfrey, and favorite numbers: 2, 6 and 908
# A person with the full name, Roger Goodell, handle, rGood, and favorite numbers: 2
lsPeople <- list(createPersonObject(fullname = "Alton Brown", handle = "aBrown", favnums = c(89, 7)) ,
                 createPersonObject(fullname = "Oprah Winfrey", handle = "oWinfrey", favnums = c(2, 6, 908)),
                 createPersonObject(fullname = "Roger Goodell", handle = "rGood", favnums = c(2)))
dfPeople <- do.call("rbind.fill", lsPeople) %>%  jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfPeople,
               signQuery = FALSE)

#getAllEntityRecords("authority/test", "person", signQuery = FALSE)
# Create 1 New Person and 1 New Artist
# Write a transaction to create the 1 new person and 1 new artist
# Person with the full name, Connie Seur, handle, cSuer, favorite numbers: 13, and favorite artists: Gustav Klimt
# Artist with the name, Gustav Klimt
lsData <- list(createPersonObject(fullname = "Connie Seur", handle = "cSuer",
                                         favnums = 13, favartists = "artist$Gustav"),
                            createArtistObject(name = "Gustav Klimt", entityId = "artist$Gustav"))

dfData <- lsData %>% jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfData,
               signQuery = FALSE)

# Create 1 New Chat and 1 New Comment
# Write a transaction to create a new person with the full name, Joy Tan, and handle, jTan
# Write a transaction to create a new chat with the message, This is a message from Joy!, instant, now, and person, Joy Tan
lsData <- list(createPersonObject(fullname = "Joy Tan", handle = "jTan", entityId = "person$joy"),
               createChatObject(comments = "This is a message from Joy!",
                                instant = "#(now)",
                                person = "person$joy"))
dfData <- lsData %>% jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfData,
               signQuery = FALSE)



lsData <- list(createPersonObject(fullname = "John Cena", handle = "jCena"))
dfData <- lsData %>% jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfData,
               signQuery = FALSE)


lsData <- list(createAuthObject(id = Sys.getenv("authId"),
                                doc = "Test 123"))
dfData <- lsData %>% jsonlite::toJSON(auto_unbox = TRUE)
flureeTransact(ledgerName = "authority/test",
               transactObject = dfData,
               signQuery = FALSE)





lsData <- list(createPersonObject(fullname = "Veli Tshepo", handle = "vSotiya1"))
dfData <- lsData %>% jsonlite::toJSON(auto_unbox = TRUE)
flureeTransact(ledgerName = "authority/test",
               transactObject = dfData,
               signQuery = FALSE)





