# This function maps the columns of a dataframe to predicate specifications using conventions compatible with Fluree V3 
# These predicates can be used to define a schema for Fluree transactions
# Each predicate mapping specifies
# 1. @id: the IRI of the predicate in full. @context to incorporate ontology prefixes are handled later in the data pipeline
# 2. the domain of the predicate as IRI
# 3. the range of the predicate as IRI
# 4. the corresponding column (variable) name in the dataframe
# This function just builds the predicate specification. The heavy lifting (the actual content of the specification) still has to be done manually - 
# In future (post 12/2024), the goal is to automate this process to a greater degree. 
mapPredicates <- function(varnames, predIRIs, domains, ranges){
  schema_list <- list()
  for(i in 1:length(varnames)){
    schema_list[[i]] <- list(
      "@id" = predIRIs[i],
      "http://www.w3.org/2000/01/rdf-schema#domain" = domains[i],
      "http://www.w3.org/2000/01/rdf-schema#range" = ranges[i],
      "varname" = varnames[i]
    )
  }
  return(schema_list)
}
