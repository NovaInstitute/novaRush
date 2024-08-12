library(rjson)
library(httr)
library(jsonlite)
library(tibble)
library(plyr)
library(novaRush)

# Generate Keypair ----------------------

# # kp <- novaRush::generateKeyPair()
# # writeLines(kp, "kp.txt")
# kp <- readLines("kp.txt")
# authId <- jsonlite::fromJSON(kp)$authId
# privKey <- jsonlite::fromJSON(kp)$privKey
# pubKey <- jsonlite::fromJSON(kp)$pubKey

# Setup ----------------------

Sys.setenv(fluree_link = "http://localhost:8090/fdb/") # "http://localhost:8091/fluree"

# Sys.setenv(authId = "Tf8yFnwVxvekbvwXwsEzgwUVz4YBZMgjHEL")

# Step 1: Start docker container with: docker run -d --name fluree-ledger --network fluree-network  -e fdb-mode=dev -e fdb-open-api=false -p 8090:8090 fluree/ledger
system("docker network ls") # check if network exists
system("docker network create fluree-network") # create network
system("docker run -d --name fluree-ledger --network fluree-network  -e fdb-mode=dev -e fdb-open-api=false -p 8090:8090 fluree/ledger")
system("docker run -d --name fluree-server --network fluree-network
       -e fdb-mode=dev
       -e fdb-open-api=false
       -e fdb-api-port=8090
       -p 8095:8090 fluree/server:latest ")
system("docker ps -a")
system("docker start fb2c5f749966")
k <- system("docker exec -t --user root fb2c5f749966 cat /var/lib/fluree/default-private-key.txt", intern = TRUE) # /var/lib/fluree/
# Go into the UI and retrieve the AithId
authId <- "Texfia91G7PfrL7g3Dc6RK3enUBviVGrDvx"

Sys.setenv(authId = authId)
Sys.setenv(privateKey = k)

# Step 2: To enable authentication, set the environment variable fdb-auth=true:
# sudo docker exec -it --user root fb2c5f749966 bash,  # f555564d120b ?
#   apt-get update && apt-get install nano,
#   nano /opt/fluree/fdb/config/fdb.properties,  fdb-auth=false"
# docker stop fb2c5f749966
# docker start fb2c5f749966
# Step 3: Install nodejs and npm in the root directory of this project
# Step 4: Install npm init -y,

# Find root id
dfRole <- getAllEntityRecords(ledgerName = "cjp/een", entityName = "_role", signQuery = TRUE) %>%
  rename_with(~ gsub("_", "", .x)) %>%
  unnest(cols = c(`role/rules`)) %>%
  mutate(across(where(is.numeric), as.character))

dfAuth <- getAllEntityRecords(ledgerName = "cjp/een", entityName = "_auth", signQuery = TRUE) %>%
  rename_with(~ gsub("_", "", .x)) %>%
  mutate(across(where(is.numeric), as.character))

# create auth object locally
transactObj <- createAuthObject(id = authId, doc = "Test 123", roles = dfRole$`id`[[1]])

# Send to fluree
flureeTransact(ledgerName = "cjp/een", transactObject = transactObj, signQuery = TRUE)

#creating collections 'tables' in fluree
flureeTransact(ledgerName = "cjp/een",
               transactObject = createCollectionObject(name  = "persons",
                                                       doc = "Collection to for all persons",
                                                       version = 1),
               signQuery = TRUE)

flureeTransact(ledgerName = "cjp/test",
               transactObject = createCollectionObject(name  = "chat",
                                                       doc = "Collection to for all chats",
                                                       version = 1),
               signQuery = TRUE)

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

getAllEntityRecords("authority/test", "_collection", signQuery = FALSE)
getAllEntityRecords("authority/test", "_predicate", signQuery = FALSE)



lsPeople <- list(createPersonObject(fullname = "Jane Doe", handle = "jdoe"),
createPersonObject(fullname = "Zach Smith", handle = "zsmith"))

dfPeople <- do.call("rbind.fill", lsPeople) %>%  jsonlite::toJSON(auto_unbox = TRUE)

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
lsPeople <- list(createPersonObject(fullname = "Alton Brown", handle = "aBrown", favnums = list(89, 7)) ,
                 createPersonObject(fullname = "Oprah Winfrey", handle = "oWinfrey", favnums = jsonlite::toJSON(c(2, 6, 908))),
                 createPersonObject(fullname = "Roger Goodell", handle = "rGood", favnums = jsonlite::toJSON(c(2))))
dfPeople <- do.call("rbind.fill", lsPeople) %>%  jsonlite::toJSON(auto_unbox = TRUE)

flureeTransact(ledgerName = "authority/test",
               transactObject = dfPeople,
               signQuery = FALSE)

getAllEntityRecords("authority/test", "person", signQuery = FALSE)





