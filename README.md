# NovaRush

This serves as a R wrapper around the Fluree API, providing a more convenient way of interacting (transacting, querying & deleting) with Fluree V3 databases.
It is largely based on the Fluree client SDK written for TypeScript/JavaScript which can be found [here](https://github.com/fluree/fluree-client/tree/main).

## Package scope and legacy utilities

The intended long-term responsibility of `novaRush` is generic communication
with Fluree. This includes connection configuration, authentication, ledger and
branch management, transactions, queries, named graphs, history, vector search,
and Fluree developer memory operations.

The package currently also contains survey-domain, RDF-transformation,
visualization, and data-wrangling utilities inherited from its earlier role.
These functions are being retained unchanged so that existing users and Nova
Institute colleagues can inspect them before repository ownership is decided.
Being listed below does not mean that a function is obsolete or approved for
removal.

Most of these utilities are expected eventually to live in, or be incorporated
into, `novaGraphDB`, because that package owns conversion of legacy survey forms
into ontology-aligned JSON-LD. Migration will happen only after the functions'
purpose, consumers, tests, and replacement path have been reviewed.

### Likely `novaGraphDB` candidates

Survey and form conversion:

- `SurveyOntologyNotas.R`
- `cto_to_jsonld.R`
- `formdef2graph.R`
- `make_surveycto_context.R`
- `map_cto_to_rdf.R`
- `parseCTOMetadata.R`

Generic RDF and JSON-LD transformation used by the survey pipeline:

- `createSPO.R`
- `entitiesFromOnt.R`
- `expandIRIs.R`
- `nodify.R`
- `properties2kv.R`
- `rdf_from_tibble.R`
- `schema_from_tripples.R`
- `triples_to_jsonld.R`
- `tripples_to_jsonld_helpers.R`

RDF inspection and visualization:

- `plot_rdf_triples_generic_p.R`
- `plot_rdf_triples_interactive.R`
- `plot_rdf_tripples_generic.R`

Supporting data reshaping:

- `pivot_longer_with_type.R`
- `system2tibble.R`
- `unnest_all.R`

### Review rule

No flagged function should be removed, unexported, deprecated, or moved until
its current consumers have been identified and equivalent behavior is covered
by tests in its destination package. Until then, the current API remains
available from `novaRush`.

## Live Fluree integration test

The ordinary test suite uses mocked HTTP requests and does not require Fluree.
An additional opt-in test verifies the complete create, insert, query, upsert,
and conditional-update workflow against a real Fluree v4 server.

Start Docker Desktop, then start a local Fluree container:

```bash
docker run -d \
  --name novarush-fluree-test \
  -p 8090:8090 \
  -v novarush-fluree-test-data:/var/lib/fluree \
  -e FLUREE_LISTEN_ADDR=0.0.0.0:8090 \
  fluree/server:latest
```

Confirm that it is ready:

```bash
curl http://localhost:8090/health
```

From the `novaRush` package directory, run only the live test:

```bash
FLUREE_LIVE_TEST=true \
FLUREE_BASE_URL=http://localhost:8090 \
FLUREE_TEST_LEDGER=novarush-integration \
Rscript -e 'devtools::test(filter = "live-core")'
```

Optional settings are `FLUREE_TEST_BRANCH`, `FLUREE_API_TOKEN`, and
`FLUREE_REQUEST_TIMEOUT`. The test ledger and its uniquely identified test
entities are retained for inspection. Cleanup is deliberately manual:

```bash
docker stop novarush-fluree-test
docker rm novarush-fluree-test
docker volume rm novarush-fluree-test-data
```

Removing the Docker volume permanently deletes the retained test ledger.

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





