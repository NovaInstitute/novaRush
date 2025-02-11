# NovaRush

This serves as a R wrapper around the Fluree API, providing a more convenient way of interacting (transacting, querying & deleting) with Fluree V3 databases.
It is largely based on the Fluree client SDK written for TypeScript/JavaScript which can be found [here]().

Additionally functions to handle data migration are also included...TODO

## Usage

### The two approaches

This package allows for an object-oriented approach as well as a functional approach to interact with the Fluree instance.  The object-oriented approach mirrors the Typescript SDK mentioned earlier.

The documentation for the functional approach follows below with a quick guide of the object-oriented approach at the end (if it were to be applicable).

#### Functional approach

###### StartUp

The first step is to configure the parameters needed to interact with the Fluree instance.

```
config <- list(
  host = 'localhost', 
  port = 58090, 
  ledger = 'policy-view-age', 
  create = TRUE, 
  privateKey = key_get("privateKey", keyring = "Fluree"))

setConfiguration(config = config)
```

NOTE: here the `keyring` package is used to deal with the private key more securely.

Now that the variables have been set, the `defaultContext` can be set and a connection
can be established.

```
connect()

Sys.sleep(1)

c <- list(
  "f" = "https://ns.flur.ee/ledger#", 
  "ex" = "http://example.org/",
  "schema" = "http://schema.org/")

setContext(c)
```

###### Transacting 

After a connection has been established to the Fluree instance,  a simple transaction
is done as follows

```
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


transactionInstance <- transact(data)
Sys.sleep(1)
sendTransaction(transactionInstance)

```

###### Querying

A simple query is done as follows

```
simpleQuery <- '{
  "select": {
    "?s": ["*"]
  },
  "where": {
      "@id": "?s",
      "schema:name": "?name"
  }
}'

queryInstance <- query(simpleQuery)
Sys.sleep(1)
sendQuery(queryInstance)

```
