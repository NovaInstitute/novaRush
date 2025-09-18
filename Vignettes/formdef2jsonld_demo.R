# Method 1: Full pipeline with demo
# KiA_adaptation_ACTIVE
# "novanpc"
# "https://novapc.surveycto.com/"

kia_adapt <- novaCTO::readCTO("KiA_adaptation_ACTIVE")
formdef <- kia_adapt$fromschema$kia_adaptation

#triples <- map_cto_to_rdf(formdef, base_uri = "https://novapc.surveycto.com/", instrument = "KiA_adaptation_ACTIVE")


# Method 2: Direct conversion
jsonld_obj <- cto_to_jsonld(formdef,
                            base_uri = "https://novapc.surveycto.com/",
                            instrument = "KiA_adaptation_ACTIVE")
jsonld_string <- export_jsonld(jsonld_obj)

# Method 3: Step by step
triples <- map_cto_to_rdf(formdef, base_uri = "https://novapc.surveycto.com/", instrument = "KiA_adaptation_ACTIVE")
jsonld_obj <- triples_to_jsonld(triples, context_df = ,surveycto_context)
jsonld_string <- export_jsonld(jsonld_obj)
