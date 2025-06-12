# Method 1: Full pipeline with demo
# KiA_adaptation_ACTIVE
# "novanpc"
# "https://novapc.surveycto.com/"


result <- demo_mapping(formdef)
jsonld_obj <- result$jsonld_obj
jsonld_string <- result$jsonld_string

# Method 2: Direct conversion
jsonld_obj <- cto_to_jsonld(formdef)
jsonld_string <- export_jsonld(jsonld_obj)

# Method 3: Step by step
triples <- map_cto_to_rdf(formdef)
jsonld_obj <- triples_to_jsonld(triples, surveycto_context)
jsonld_string <- export_jsonld(jsonld_obj)
