# novaRush

R interface to Fluree v4 — a semantic RDF graph database. This context covers the Fluree operational model and the novaRush-specific terms that wrap it.

## Language

### Fluree operations

**Insert**:
An operation that adds new triples to a ledger without touching existing ones. The appropriate choice when transacting data for the first time.
_Avoid_: create, add, write, transact (v3 term)

**Upsert**:
An operation that replaces the values of every supplied predicate on each subject; predicates not mentioned are left unchanged. Idempotent: running the same upsert twice produces the same ledger state.
_Avoid_: merge, sync, replace, update

**Update**:
A conditional operation that matches existing data with a `where` clause, retracts matched triples with `delete`, and adds new triples with `insert`. Used when the old value must be bound before it can be replaced.
_Avoid_: patch, modify, edit

**Delete**:
Retraction of triples from a ledger, expressed as an update with a wildcard `delete` clause. Not a standalone endpoint — always routed through update.
_Avoid_: remove, drop, retract

**Query**:
A JSON-LD query sent to Fluree's `/fluree/query` endpoint. Expressed with `select`, `where`, `filter`, `groupBy`, `orderBy`, and optionally `reasoning` fields.
_Avoid_: select, read, fetch, FlureeQL (v3 term)

**SPARQL**:
An alternative W3C-standard query language for Fluree. Sent to the same `/v1/fluree/query/{ledger}` endpoint as JSON-LD Query but with `Content-Type: application/sparql-query` and a raw SPARQL string body. Returns W3C SPARQL JSON format. Use when cross-system compatibility matters; use Query otherwise.

### Fluree data model

**Ledger**:
A named, versioned RDF graph database — the unit of deployment in Fluree. A single server hosts many ledgers.
_Avoid_: database, db, store, graph

**Commit**:
An immutable, content-addressed snapshot of a ledger produced by each successful transaction. Commits form the complete audit chain.
_Avoid_: version, snapshot, revision

**Branch**:
An isolated transaction space within a ledger. Branches can be merged. Analogous to a git branch.
_Avoid_: fork, workspace

**Named Graph**:
A graph within a ledger identified by an IRI. The default graph holds application data; a dedicated schema graph holds SHACL shapes and ontology.
_Avoid_: graph, dataset, context (which means something else in JSON-LD)

**Schema Graph**:
The named graph within a ledger where SHACL shapes and OWL/RDFS ontology are stored, isolated from application data.
_Avoid_: schema, vocabulary graph, ontology graph

**Default Context**:
The JSON-LD `@context` configured on a FlureeInstance that is merged into every subsequent query and transaction, so prefixes need not be repeated on every call.
_Avoid_: global context, namespace map, prefix configuration

**Reasoning Mode**:
The OWL/RDFS inference level requested for a specific query: `rdfs`, `owl2ql`, `owl2rl`, `datalog`, or `none`. Set per query; can also be configured as a ledger-wide default.
_Avoid_: inference, entailment, reasoning level

### novaRush

**FlureeInstance**:
The primary entry point of the novaRush package. Holds connection config and dispatches all Fluree operations (`$insert()`, `$upsert()`, `$update()`, `$delete()`, `$query()`, `$sparql()`, `$history()`).
_Avoid_: client, connection, session

**nova-skills**:
The shared git repository (`~/nova-skills/`) of Claude Code skills for semantic web and Fluree work, reusable across projects.
