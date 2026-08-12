# Outstanding Work

## Repository ownership review

- [ ] Review the survey and form conversion utilities currently retained in
  `novaRush` and map their behavior to `novaGraphDB`.
- [ ] Review the generic RDF/JSON-LD transformation utilities and identify
  which are required by the `novaGraphDB` extraction pipeline.
- [ ] Decide whether RDF visualization helpers belong in `novaGraphDB` or in a
  separate reusable package.
- [ ] Review the supporting data-reshaping utilities for external consumers and
  duplication in `novaGraphDB`.
- [ ] Add destination-package tests before moving any retained utility.
- [ ] Do not remove, unexport, deprecate, or migrate a flagged utility until its
  consumers and replacement path have been confirmed by the team.

## novaRush v4 update

- [x] Fix `testLedgers()` in `FlureeInstance.R` — updated to v4 JSON-LD Query format (list, not JSON string)
- [x] Update stale docstrings in `transactionHandling.R` — delete and upsert now correctly describe v4 endpoints
- [x] Add `Insert()` and `Update()` functional wrappers in `transactionHandling.R` to match `Transact()` pattern
- [x] Rich example data: `Vignettes/fluree_v4_api.Rmd` — covers `insert`, `upsert`, `update`, `delete`, `query` (with RDFS reasoning), `sparql`, `history`, and time-travel queries against a live Fluree v4 instance

## nova-skills

- [ ] `/fluree-temporal` — time travel queries, git-style branching, history
- [ ] `/fluree-policy` — graph-native per-query/transaction access control
- [ ] `/fluree-iceberg` — Iceberg/Parquet as a Fluree graph source
- [ ] `/fluree-ai` — MCP server, vector search, RAG integration

## Broader semantic expansion (GHG_methodologies)

See `~/GHG_methodologies/SEMANTIC_EXPANSION.md` for full design. Key dependencies on novaRush:

- [ ] Update `novaRush` for Fluree v4 ✅ (done)
- [ ] SKOS hierarchy materialisation helper (Step 3 in SEMANTIC_EXPANSION.md)
- [ ] `inst/shacl/` + `inst/concepts/` for pilot packages (`cdmAmsIa`, `cdmAcm0002`)
- [ ] `variable_registry_*()` functions for pilot packages
- [ ] Redesigned `check_applicability_*()` with Fluree integration
- [ ] `~/cdmVocabulary/` repo — `cdm.ttl` + `cdm-concepts.ttl`
