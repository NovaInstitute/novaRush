# novaRush

An R client for the [Fluree](https://flur.ee) v4 graph database. Tibbles in, tibbles
out, pipes throughout.

```r
library(novaRush)

con <- fluree_connect("127.0.0.1", ledger = "novaRush/demo", port = 8090,
                      context = c(schema = "http://schema.org/"))

fluree_query(con) |>
  fq_where(`@id` = "?s", `@type` = "schema:Person",
           `schema:name` = "?name", `schema:age` = "?age") |>
  fq_select(name, age) |>
  fq_filter(age > 25, startsWith(name, "A")) |>
  fq_order_by(desc(age)) |>
  fq_limit(10) |>
  collect()
#> # A tibble: 1 × 2
#>   name    age
#>   <chr> <int>
#> 1 Alice    30
```

Full walkthrough: `vignette("tidy_fluree")`.

## Install

`semanticModelR` (>= 0.2.0) is required and is not on CRAN, so install with `remotes`
(or `pak`, or `devtools`). Those read the `Remotes:` field in `DESCRIPTION` and pull it
in for you:

```r
# install.packages("remotes")
remotes::install_github("NovaInstitute/novaRush@novaRush_v2")
```

Once `novaRush_v2` is merged the `@novaRush_v2` suffix can be dropped.

`install.packages()`, `R CMD INSTALL` and a bare `renv::restore()` all ignore
`Remotes:`, so they fail with `there is no package called 'semanticModelR'`. If you hit
that, or you already have a copy older than 0.2.0, install the dependency first:

```r
remotes::install_github("NovaInstitute/semanticModelR")
```

## Reading

Queries are composed locally and sent only by `collect()`, so a partly built query is
a value you can reuse.

| Verb | Purpose |
|---|---|
| `fluree_query(con)` | start a JSON-LD Query |
| `fq_where()` | add a node pattern; call it repeatedly |
| `fq_select()` / `fq_crawl()` | columns, or whole nodes |
| `fq_filter()` | filter, written as R expressions |
| `fq_order_by()` / `fq_group_by()` | `desc()` supported |
| `fq_limit()` / `fq_offset()` | paging |
| `fq_context()` / `fq_reasoning()` / `fq_at()` | prefixes, RDFS/OWL inference, time travel |
| `collect()` | send, and return a tibble |
| `show_query()` | print the query without sending it |

Also `fluree_sparql(con, query)` for SPARQL 1.1 and `fluree_history(con)` for the
commit log. Both return tibbles, with values coerced from the datatypes in the
response.

### Filters are translated, and that matters

Fluree v4.1 expects filters **inside** the `where` clause as prefix s-expressions.
Both plausible alternatives fail *silently* — no error, just a wrong answer:

| What is sent | What Fluree does |
|---|---|
| `"where": [{…}, ["filter", "(> ?age 25)"]]` | filters correctly |
| `"where": [{…}, ["filter", "?age > 25"]]` | returns **zero rows** |
| top-level `"filter": [...]` | **ignored**, returns everything |

`fq_filter()` always emits the first form:

```r
fq_filter(q, age > 25)              # (> ?age 25)
fq_filter(q, age > 25 & age < 40)   # (and (> ?age 25) (< ?age 40))
fq_filter(q, !is.na(age))           # (bound ?age)
fq_filter(q, startsWith(name, "A")) # (strStarts ?name "A")
fq_filter(q, grepl("^A", name))     # (regex ?name "^A")
fq_filter(q, nchar(name) > 3)       # (> (strlen ?name) 3)
fq_filter(q, age %in% c(25, 30))    # (in ?age [25 30])
```

Bare names become query variables; unquote a local value with `!!`. Unrecognised
function names pass through, so any Fluree function can be called, and a plain string
is used verbatim as an escape hatch.

## Writing

`fluree_insert()`, `fluree_upsert()`, `fluree_update()` and `fluree_delete()` accept a
triple table, a `tidygraph` graph, or raw JSON-LD.

```r
people <- tibble::tribble(
  ~subject,   ~predicate,    ~object,         ~object_type, ~datatype,
  "ex:alice", "rdf:type",    "schema:Person", "uri",        NA,
  "ex:alice", "schema:name", "Alice",         "literal",    "string",
  "ex:alice", "schema:age",  "30",            "literal",    "integer")

fluree_insert(con, people)

# any data frame, via semanticModelR
fluree_insert(con, semanticModelR::pivot_longer_with_type(head(iris, 10)))
```

The `datatype` column decides the JSON type, so numbers are stored as numbers rather
than strings. An ordinary data frame is refused rather than guessed at — minting IRIs
from columns is a modelling decision.

## Working on the graph

`fluree_graph()` returns a [tidygraph](https://tidygraph.data-imaginist.com/) graph,
so a ledger extract can be traversed with dplyr verbs and plotted with ggraph.

```r
library(tidygraph)
library(semanticModelR)

g <- fluree_graph(con, subject_type = "schema:Person")

g |> activate(nodes) |> mutate(degree = centrality_degree(mode = "all"))
rdf_neighbourhood(g, "ex:alice", order = 2)
rdf_summary(g)
rdf_ggraph(g)

fluree_insert(con, g)   # and back again
```

## Branches and named graphs

Branches and named graphs solve different problems. A **branch** isolates a mutable
state of the whole ledger — an unaccepted tagging run, say. **Named graphs** partition
categories of knowledge *inside* that state: survey data, embeddings, candidate
hierarchies, review decisions.

```r
listBranches(config = config)
createBranch(config = config, branch = "candidate-01", from = "main")
branchExists(config = config, branch = "candidate-01")
```

```r
survey_graph <- "https://data.nova.org/graphs/survey"

upsertNamedGraph(document = questionnaire_jsonld, graph = survey_graph,
                 config = config, branch = "main")

questions <- queryNamedGraph(query = taggable_question_query, graph = survey_graph,
                             config = config, branch = "main")
```

A named graph is registered implicitly when its first resource is written. novaRush
treats graph contents as generic JSON-LD and does not interpret survey, embedding,
hierarchy, tag or reviewer semantics.

## Vector search

Embeddings are stored with Fluree's native `@vector` datatype and searched with exact
inline similarity functions. You supply every property IRI, so the client imposes no
tagging ontology:

```r
embedding_graph <- "https://data.nova.org/graphs/embeddings/model-v1"
embedding_property <- "https://data.nova.org/tagging/embedding"

upsertVectors(
  records = embedding_records, graph = embedding_graph,
  vector_property = embedding_property,
  model = "text-embedding-3-small",
  model_property = "https://data.nova.org/tagging/embeddingModel",
  dimension_property = "https://data.nova.org/tagging/embeddingDimension",
  config = config, branch = "main")

nearest <- searchVectors(
  graph = embedding_graph, vector_property = embedding_property,
  query_vector = query_embedding, metric = "cosine", limit = 10,
  config = config, branch = "main")
```

Vectors must be numeric, finite, non-empty and consistently dimensioned; Fluree stores
them as 32-bit floats. Query literals use the full `f:embeddingVector` datatype because
`@vector` is transaction shorthand. HNSW indexing is a future optimisation — the search
API is deliberately independent of it.

## Signing

```r
setKey()   # prompts once, stores in the system keyring
con <- fluree_connect("127.0.0.1", ledger = "novaRush/demo", port = 8090,
                      sign = TRUE, private_key = getKey())
```

`private_key` must be passed explicitly. The keyring is deliberately never read for
you, because unlocking it can prompt, which would hang a non-interactive script.

## Lower-level interfaces

Both older interfaces are unchanged and still exported.

**R6** — full control of the request cycle; see `vignette("fluree_v4_api")`.

```r
db <- FlureeInstance$new(list(host = "localhost", port = 8090,
                              ledger = "novaRush/demo"))$connect()
db$query(list(select = list("?s"), where = list(list(`@id` = "?s", `?p` = "?o"))))$send()
```

**Functional** — `setConfig()`, `setContext()`, `query()`/`sendQuery()`,
`transact()`/`sendTransaction()`, and the `Query()`/`Transact()`/`Insert()`/`Update()`
wrappers.

The tidy verbs are built on the R6 classes, so the three interfaces can be mixed;
`con$instance` is the underlying `FlureeInstance` if you need to reach it.

## Testing

The offline suite mocks HTTP and needs no server. The live suite is opt-in:

```bash
docker run -d --name novarush-fluree-test -p 8090:8090 \
  -v novarush-fluree-test-data:/var/lib/fluree \
  -e FLUREE_LISTEN_ADDR=0.0.0.0:8090 fluree/server:latest
curl http://localhost:8090/health
```

```bash
FLUREE_LIVE_TEST=true FLUREE_BASE_URL=http://localhost:8090 \
FLUREE_TEST_LEDGER=novarush-integration \
Rscript -e 'devtools::test()'
```

Also honoured: `FLUREE_TEST_BRANCH`, `FLUREE_API_TOKEN`, `FLUREE_REQUEST_TIMEOUT`.
Filter to one area with `devtools::test(filter = "live-core")` — or `live-branch`,
`live-named-graph`, `live-vector`.

Test ledgers, branches and graphs are deliberately **retained** for inspection;
cleanup is manual, and removing the Docker volume deletes them permanently:

```bash
docker stop novarush-fluree-test && docker rm novarush-fluree-test
docker volume rm novarush-fluree-test-data
```

## What changed in 0.3.0

- **Branches, named graphs and vector search**, and a single `fluree_request()` layer
  underneath every HTTP call — centralised timeouts, and structured transport errors
  that distinguish a timeout from a refusal and flag whether the failed operation was
  a write.
- **Fixed: signed queries and transactions were assembled wrongly.** The signing
  helpers were passed a `qry`/`transaction` key where they expected `query`, and their
  nested result was never unwrapped, so a list was sent where a JWT string belonged.
  Signed requests also now go out as `application/jose`, which is what Fluree v4
  expects; `application/jwt` is still accepted as an input alias.
- **Writes route explicitly.** `transact()` takes an `endpoint` argument, and
  `fluree_insert()`/`fluree_update()` pass it rather than relying on inference from the
  presence of a `where` clause — a misrouted write is one of the failures Fluree v4
  does not report.
- Connections carry a `branch`, and `fluree_connect()` accepts the full v4
  configuration (`branch`, `timeout`, `api_key`, `base_url`, `api_path`).

## What changed in 0.2.0

- The tidy interface above, and the first test suite this package has had — offline
  tests covering query construction, filter translation and response parsing, plus a
  live suite that runs when `FLUREE_LIVE_TEST=true`.
- **The semantic modelling functions moved to
  [semanticModelR](https://github.com/NovaInstitute/semanticModelR).** Turning tables
  and SurveyCTO form definitions into RDF is modelling, not client work, and keeping
  two copies meant they could drift. The 31 names are still exported here and forward
  to semanticModelR, warning once per session, so existing scripts keep working:

  ```r
  create_uri_safe("A B")
  #> Warning: create_uri_safe() has moved to semanticModelR and will be removed from
  #>   novaRush in a future version. Use semanticModelR::create_uri_safe() instead.
  ```

  They will be removed in a future version — see `?"novaRush-moved"`. semanticModelR
  also carries fixes never applied here, and adds a tidygraph bridge.
  This dropped 27 files and about 2,900 lines from novaRush, and took roxygen from 31
  warnings to none.
- **Fixed: the R6 query and transaction paths could not send at all.**
  `QueryInstance$send()` and `TransactionInstance$send()` read the `Content-Type`
  header with `$` from what `generateFetchParams()` returns as a *named character
  vector*, so every call raised `$ operator is invalid for atomic vectors`. This also
  dropped the `Authorization` header. The functional path was unaffected.
- **Fixed: `DESCRIPTION` omitted five packages the code imports** — `R6`,
  `flureeCrypto`, `httr`, `jsonlite`, `keyring` — so `R CMD check` failed at
  `checking package dependencies`.
- Terminology follows Fluree v4: the native JSON query language is **JSON-LD Query**.
  "FlureeQL" was the v2/v3 name.

## Known issues

`R CMD check` is clean of errors. What remains:

- `tidyr`, `dplyr` and `magrittr` are in `Depends` rather than `Imports`, so they are
  attached for every user. They are now also imported, so the namespace works
  unattached, but the attachment side effect stands until the bare `dplyr`/`tidyr`
  calls in `createBody.R` and `transactionUtils.R` are qualified.
- Two `flureeCrypto:::` calls reach unexported functions (`serialize_jws`,
  `deserialize_jws`). Fixing this needs those exported upstream.
- `LICENSE` is not referenced from `DESCRIPTION`, which R notes. Apache 2.0 is not an
  extensible licence, so `+ file LICENSE` is not permitted; the file is shipped anyway
  because Apache 2.0 asks that the licence travel with the work.
- `data/forms.Rda` is excluded from the build. R only recognises `.rda`/`.RData`, so
  it has never been a loadable dataset, nothing references it, and it is survey data
  that most likely belongs in `semanticModelR`.
- `FlureeInstance$create()` sets its own `Content-Type` and so does not send the
  `Authorization` header, which will matter for ledger creation against the hosted
  service.
- The vignettes are documentation, not executable: their code blocks are plain fenced
  R rather than knitr chunks, because every example needs a live Fluree server.
