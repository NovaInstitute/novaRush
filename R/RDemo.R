# This file is used for testing 

config <- list(
  host = 'localhost', 
  port = 58090, 
  ledger = 'policy-view-age', 
  create = TRUE, 
  privateKey = key_get("privateKey", keyring = "Fluree"))

initializeEnvironmentVariables(config = config)

Sys.sleep(1)

connect()

Sys.sleep(1)

c <- list(
  "f" = "https://ns.flur.ee/ledger#", 
  "ex" = "http://example.org/",
  "schema" = "http://schema.org/")

setContext(c)
Sys.sleep(1)

data <- '{
      "insert": [
          {
              "@id": "ex:andrew",
              "@type": [
                  "ex:Yeti",
                  "schema:Person"
              ],
              "schema:age": 35,
              "schema:follows": [
                  {
                      "@id": "ex:freddy"
                  },
                  {
                      "@id": "ex:letty"
                  },
                  {
                      "@id": "ex:betty"
                  }
              ],
              "schema:givenName": "Andrew",
              "schema:name": [
                  "Andrew Johnson",
                  "Andy the Yeti"
              ]
          },
          {
              "@id": "ex:betty",
              "@type": "ex:Yeti",
              "ex:firstName": "Betty",
              "schema:age": 82,
              "schema:follows": {
                  "@id": "ex:freddy"
              },
              "schema:name": "Betty"
          },
          {
              "@id": "ex:freddy",
              "@type": "ex:Yeti",
              "ex:verified": true,
              "schema:name": "Freddy",
              "schema:age": 4
          },
          {
              "@id": "ex:letty",
              "@type": "ex:Yeti",
              "ex:firstName": "Leticia",
              "ex:nickname": "Letty",
              "schema:age": 2,
              "schema:follows": {
                  "@id": "ex:freddy"
              },
              "schema:name": "Leticia"
          }
      ]
}'

policy <- '{
        "insert": {
            "@id": "ex:rootPolicy",
            "@type": ["f:Policy"],
            "f:targetNode": {"@id": "f:allNodes"},
            "f:allow": [
                {
                    "@id": "ex:rootAccessAllow",
                    "f:targetRole": {"@id": "ex:rootRole"},
                    "f:action": [
                        {"@id": "f:view"},
                        {"@id": "f:modify"}
                    ]
                }
            ]
        }
}'

identity <- '{
          "insert": {
              "@id": "did:fluree:TexCb7aQXKkAVAFpfjheQiGK7j2gHSjVAzJ",
              "ex:user": {"@id": "ex:andrew"},
              "f:role": {"@id": "ex:rootRole"}
          }
}'

print("Transacting data")
data_as_list <- fromJSON(data, simplifyDataFrame = F, simplifyMatrix = F, simplifyVector = F)
print(data_as_list)
transact(data_as_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendTransaction(configuration)
Sys.sleep(1)

print("Transacting policy")
policy_as_list <- fromJSON(policy, simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
print(policy_as_list)
transact(policy_as_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendTransaction(configuration)
Sys.sleep(1)

print("Transacting identity")
identity_as_list <- fromJSON(identity, simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
print(identity_as_list)
transact(identity_as_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendTransaction(configuration)
Sys.sleep(1)

q1 <- '{
  "select": {
    "?s": ["*"]
  },
  "where": {
      "@id": "?s",
      "schema:name": "?name"
  }
}'

query_list <- fromJSON(q1, simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F)
print("Execute query without a policy applied")
query(query_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendQuery(configuration)
Sys.sleep(1)

q2 <- '{
  "select": {
    "?s": ["*"]
  },
  "where": {
      "@id": "?s",
      "schema:name": "?name"
  },
  "opts": {
     "role": "ex:rootRole"
  }
}'

print("Execute a query with policy applied (via role)")
query_list <- fromJSON(q2, simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F)
query(query_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendQuery(configuration)
Sys.sleep(1)

q3 <- '{
  "select": {
    "?s": ["*"]
  },
  "where": {
      "@id": "?s",
      "schema:name": "?name"
  },
  "opts": {
     "did": "did:fluree:TexCb7aQXKkAVAFpfjheQiGK7j2gHSjVAzJ"
  }
}'

print("Execute a query with policy applied (via did)")
query_list <- fromJSON(q3, simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F)
query(query_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendQuery(configuration)
Sys.sleep(1)

hist <- '{
  "commit-details": true,
  "t": { "at": "latest" }
}'

print("Execute a history query")
history_query_list <- fromJSON(hist, simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F)
history(history_query_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendHistoryQuery(configuration)
Sys.sleep(1)

test_delete <- '{
  "insert": [
    {
      "@id": "freddy",
      "name": "Freddy"
    },
    {
      "@id": "alice",
      "name": "Alice"
    }
  ]
}'

print("Transacting data for delete")
data_as_list <- fromJSON(test_delete, simplifyDataFrame = F, simplifyMatrix = F, simplifyVector = F)
print(data_as_list)
transact(data_as_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendTransaction(configuration)
Sys.sleep(1)

print("Now delete an entry ('freddy')")
delete(c("freddy"))
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendTransaction(configuration)
Sys.sleep(1)

q4 <- '{
  "select": {
    "?s": ["*"]
  },
  "where": {
      "@id": "?s",
      "name": "?name"
  }
}'

print("Execute a query to test if delete was successful")
query_list <- fromJSON(q4, simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F)
query(query_list)
Sys.sleep(1)
configuration = fromJSON(Sys.getenv("config"))
sendQuery(configuration)
Sys.sleep(1)