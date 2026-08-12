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

```r
# install.packages("remotes")
remotes::install_github("NovaInstitute/novaRush@novaRush_v2")
```

`semanticModelR` (>= 0.2.0) is required and comes from the same organisation.

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

## What changed in 0.2.0

- The tidy interface above, and the first test suite this package has had — offline
  tests covering query construction, filter translation and response parsing, plus 29
  that run against a live ledger when `FLUREE_TEST_HOST` is set.
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

- **`License` is unset** in `DESCRIPTION` (`What license is it under?`), so
  `R CMD check` warns. Picking one is a decision for the maintainers.
- `tidyr`, `dplyr` and `magrittr` are in `Depends` rather than `Imports`, so they are
  attached for every user.
- The vignettes live in `Vignettes/` with a capital V and there is no
  `VignetteBuilder` field, so they are not built. That works on a case-insensitive
  filesystem like macOS but would not on Linux or CI.
- `FlureeInstance$create()` sets its own `Content-Type` and so does not send the
  `Authorization` header, which will matter for ledger creation against the hosted
  service.
