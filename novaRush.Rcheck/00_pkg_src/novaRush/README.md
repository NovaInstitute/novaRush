# NovaRush

This serves as a R wrapper around the Fluree API, providing a more convenient way of interacting (transacting, querying & deleting) with Fluree V3 databases.
It is largely based on the Fluree client SDK written for TypeScript/JavaScript which can be found [here](https://github.com/fluree/fluree-client/tree/main).

## Usage

Below follows a quick walk through of the functions included in this packages.

Before starting it is important to note that this package makes use of the `keyring` R package to handle private keys.
Of which the documentation can be found [here](https://cran.r-project.org/web/packages/keyring/keyring.pdf).

#### Configuration

##### Setting the configuration parameters

The first step is to configure the parameters needed to interact with the Fluree instance.

```
conf <- setConfig(host = datadudes2.xyz, ledger = "demo", signMessages = TRUE))
```

Additionally a port number may be specified. 
The ledger name is the only mandatory field.  If none of the other arguments are specified, the following default values will be used:

- host = "datadudes2.xyz"
- port = NULL
- signMessages = TRUE


##### Updating the configuration parameters

Once a configuration has been set, individual fields can be updated as follows

```
conf <- updateConfig(config, newConfig = list(ledger = "test", signMessages(FALSE)))
```

In the example given above the existing `ledger` and `signMessages` fields in the old config will be replaced with the new ones
to produce the new, merged config.


#### Context

##### Setting the default context 

The default context being set will be included in all future transactions and queries.

```
c <- list(
  "f" = "https://ns.flur.ee/ledger#",
  "ex" = "http://example.org/",
  "schema" = "http://schema.org/")
  
conf <- setContext(currentConfig = conf, context = c) {
```

##### Updating the default context

The function described above (`setContext()`) replaces the default context with the the new context passed as argument.
If one wishes to simply update or add to the default context (without replacing it entirely) the following function can be used.

```
newElements <- list(
  "rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
  "owl" = "http://www.w3.org/2002/07/owl#"
)

conf <- addToContext(currentConfig = conf, context = newElements)

```

This will return the updated config with the default context now including the two new elements
together with any previously configured ones.



#### Transacting 

When it comes to transacting two functions are used.  The first configures the transaction and
the second actually sends it to the Fluree HTTP endpoint.

##### Configuring & signing the transaction

```
exampleData <- '{
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


transactionInstance <- transact(exampleData)

```

This creates an "instance" of the transaction, which can then be signed or interacted with as follows:

```
signedTransaction <- signTransaction(transactionInstance)

txn <- getTransactionText(signedTransaction)
sig <- getTransactionSignature(signedTransaction)
```

##### Sending the transaction

Once configured the signed/unsigned transaction can be sent as follows:

```
sendTransaction(transactionInstance)

// OR

sendTransaction(signedTransaction)
```


###### Querying

Querying follows the same logic as transacting.

##### Configuring & signing the query

```
exampleQuery <- '{
  "select": {
    "?s": ["*"]
  },
  "where": {
      "@id": "?s",
      "schema:name": "?name"
  }
}'

queryInstance <- query(simpleQuery)

```

This creates an "instance" of the query, which can then be signed or interacted with as follows:

```
signedQuery <- signQuery(queryInstance)

qry <- getQueryText(signedQuery)
sig <- getQuerySignature(signedQuery)

```

##### Sending the query

Once configured the signed/unsigned query can be sent as follows:

```
sendQuery(queryInstance)

// OR

sendQuery(signedQuery)
```

#### Of note:

For convenience two wrapper functions have also been implemented namely `Transact()` and `Query()`.
These functions handle both the configuration and sending of any transactions or queries respectively,
by calling the relevant functions, thereby not requiring a transaction/query to be configured
before sending it to Fluree.

Below follows an example:

```
exampleData <- '{
      "insert": [
          {
              "@id": "ex:andrew",
              "@type": [
                  "ex:Yeti",
                  "schema:Person"
              ],
              "schema:age": 35
          }
      ]
  }'
  
Transact(config = conf, ledger = 'demo', exampleData, signTransaction = FALSE)

exampleQuery <- '{
  "select": {
    "?s": ["*"]
  },
  "where": {
      "@id": "?s",
      "schema:name": "?name"
  }
}'

Query(config = conf, ledger = 'demo', exampleQuery, signQuery = FALSE)

```







