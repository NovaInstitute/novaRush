# Outstanding Work

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
