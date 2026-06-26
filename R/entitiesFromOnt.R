#
#
# entitiesFromOnt <- function(Ont = NULL){
#   if(is.null(ont)) stop("A ontology model must be provided: use 'getOntology()' to load one.")
# # Select all the classes
#
#   ontname <- map_df(ont, ~ tibble(`@id` = .$`@id`,
#                                   `@type` = .$`@type`[[1]])) %>%
#     filter(`@type` == "http://www.w3.org/2002/07/owl#Ontology") %>%
#     pull(`@id`)
#
#   ontology <- ont[map_lgl(ont, ~.$`@id` == ontname)]
#
#   dfClass <- map_df(ont, ~ tibble(`@id` = .$`@id`,
#                                   `@type` = .$`@type`[[1]],
#                                   "Entity Description" = .$`http://www.w3.org/2004/02/skos/core#definition`[[1]]$`@value`,
#                                   "Comment" = .$`http://www.w3.org/2000/01/rdf-schema#comment`[[1]]$`@value`)) %>%
#     filter(`@type` == "http://www.w3.org/2002/07/owl#Class") %>%
#     mutate("Entity Description" = case_when(is.na(`Entity Description`) ~ Comment,
#                                             TRUE ~ `Entity Description`)) %>%
#     select(c(1,3))
#
#   dfObjProp <- map_df(ont, ~ tibble(`@id` = .$`@id`,
#                                   `@type` = .$`@type`[[1]],
#                                   domain = .$`http://www.w3.org/2000/01/rdf-schema#domain`[[1]]$`@id`,
#                                   range  = .$`http://www.w3.org/2000/01/rdf-schema#range`[[1]]$`@id`)) %>%
#     filter(`@type` == "http://www.w3.org/2002/07/owl#ObjectProperty")
#
#   dfDataProp <- map_df(ont, ~ tibble(`@id` = .$`@id`,
#                                   `@type` = .$`@type`[[1]],
#                                   domain = .$`http://www.w3.org/2000/01/rdf-schema#domain`[[1]]$`@id`,
#                                   range  = .$`http://www.w3.org/2000/01/rdf-schema#range`[[1]]$`@id`)) %>%
#     filter(`@type` == "http://www.w3.org/2002/07/owl#DatatypeProperty")
#
# }
