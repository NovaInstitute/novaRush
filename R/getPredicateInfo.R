getPredicateInfo <- function(ontology_path,
                             pred_vn) {
  # load the ontology
  rdf_data <- rdf_parse(ontology_path)
  
  # construct the queries to get domains and ranges respectively
  # format for SPARQL: <IRI1>, <IRI2>, ...
  predicates <- names(pred_vn)
  predicate_filter <- paste(sprintf("<%s>", predicates), collapse = ", ")
  
  dom_q <- sprintf("
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  
  SELECT ?predicate ?domain
  WHERE { ?predicate rdfs:domain ?domain .
    FILTER(?predicate IN (%s))
  }
  ", predicate_filter)
  
  ran_q <- sprintf("
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  
  SELECT ?predicate ?range
  WHERE { ?predicate rdfs:range ?range .
    FILTER(?predicate IN (%s))
  }
  ", predicate_filter)
  
  # execute the queries 
  dom_res <- rdf_query(rdf_data, dom_q)
  ran_res <- rdf_query(rdf_data, ran_q)
  
  vn_res <- tibble(
    predicate = names(pred_vn),
    varname = unlist(pred_vn, use.names = FALSE))
  
  ex_classes <- excludedClasses(dom_res, ran_res, id_tb) %>% left_join(vn_res)
  
  # "unproblematic" mappings thus far
  mapped <- dom_res %>% 
    inner_join(ran_res) %>% 
    anti_join(ex_classes) %>% 
    left_join(vn_res)
  
  # amendment i) - class is not present in the node spec but a (unique) subclass is
  sc_replace <- replaceSubclass(rdf_data, ex_classes, id_tb)
  
  mapped <- sc_replace %>% 
    filter(!is.na(new_domain) & !is.na(new_range)) %>% 
    select(-domain, -range) %>% 
    rename(domain = new_domain, range = new_range) %>% 
    full_join(mapped) %>% 
    left_join(vn_res)
  
  # exception i) - neither class nor unique subclass present in node spec
  exceptions <- sc_replace %>% 
    filter(is.na(new_domain) | is.na(new_range)) %>% 
    select(-new_domain, -new_range) %>% 
    mutate(reason = "The class that corresponds to the domain and/or range of this predicate was not found in your node specifications.")
  
  # exception ii) - domain and/or range not found for object properties
  dr_excep <- checkDRObjectProp(rdf_data, pred_vn, dom_res, ran_res) %>% 
    left_join(vn_res)
  exceptions <- exceptions %>% full_join(dr_excep)
  
  # exception iii) - varname = NA for datatype properties
  dt_excep <- checkVNDataProp(rdf_data, pred_vn, vn_res)
  exceptions <- exceptions %>% full_join(dt_excep)
  
  # exception v) - no vn and no domain
  dm_excep <- checkVNDomain(pred_vn, dom_res, ran_res, vn_res)
  exceptions <- exceptions %>% full_join(dm_excep)
  
  return(list(mapped = mapped, exceptions = exceptions))
}

# TODO documentation
excludedClasses <- function(dom_res, 
                            ran_res,
                            id_tb) {
  # classes in domain of predicates but not in node specification
  exc_dom <- dom_res %>% 
    #select(domain) %>% 
    anti_join(id_tb, by = c("domain" = "type"))
  
  # classes in range of predicates but not in node specification
  exc_ran <- ran_res %>% 
    # select(range) %>% 
    anti_join(id_tb, by = c("range" = "type")) %>% 
    filter(!grepl("^http://www.w3.org/2001/XMLSchema#", range)) # exclude datatype properties
  
  # create list of classes
  ex_classes <- exc_ran %>% full_join(exc_dom)
}

# TODO documentation
replaceSubclass <- function(rdf_data, 
                          ex_classes, 
                          id_tb) {

  class_list <- c(ex_classes$domain, ex_classes$range)
  class_filter <- paste(sprintf("<%s>", class_list), collapse = ", ")
  
  # create query to get subclasses
  query <- sprintf("
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  SELECT ?subclass ?class WHERE {
    { ?subclass rdfs:subClassOf ?class . }
    UNION
    { ?subclass rdfs:subClassOf ?intermediate .
      ?intermediate rdfs:subClassOf ?class . }
    FILTER(?class IN (%s))
  }", class_filter)
  
  sc_result <- rdf_query(rdf_data, query)
  
  # helper function to check that only one subclass for each class is present in the node specification
  check_unique_sc <- function(data) {
    common_value <- intersect(id_tb %>% select(type) %>% pull(), data %>% pull())
    
    if(length(common_value) == 1) {
      return(common_value)
    } else {
      return(NA)
    }
  }
  
  subclasses <- sc_result %>% 
    group_by(class) %>% 
    nest() %>% # creates a tibble with two columns: class (grouping variable) and data
    mutate(uniq_sc = map(data, ~check_unique_sc(.x))) %>% 
    mutate(uniq_sc = unlist(uniq_sc))
  
  # get only the unique subclasses
  subclasses <- subclasses %>% 
    filter(!is.na(uniq_sc))
  
  # look for the classes from the class column of "subclasses" in the domain and/or range columns of "ex_classes" 
  # and replace them with the "uniq_sc" value from "subclasses"
  x <- ex_classes %>% 
    filter(domain %in% subclasses$class | range %in% subclasses$class) %>% 
    rowwise() %>% 
    mutate(new_domain = first(subclasses %>% 
                                filter(class == domain) %>% 
                                select(uniq_sc) %>% 
                                pull())) %>% 
    mutate(new_range = first(subclasses %>% 
                               filter(class == range) %>% 
                               select(uniq_sc) %>% 
                               pull()))

  ex_classes <- ex_classes %>% 
    left_join(x)
  
  return(ex_classes)
}

# TODO documentation
# check that the domain and range of object properties are specified
checkDRObjectProp <- function(rdf_data, 
                              pred_vn,
                              dom_res,
                              ran_res) {
  # get all object properties 
  predicates <- names(pred_vn)
  predicate_filter <- paste(sprintf("<%s>", predicates), collapse = ", ")
  
  objp_q <- sprintf("
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?predicate ?property
  WHERE { ?predicate rdf:type ?property .
    FILTER(?predicate IN (%s))
  }
  ", predicate_filter)
  
  obj_props <- rdf_query(rdf_data, objp_q) %>% 
    filter(property == "http://www.w3.org/2002/07/owl#ObjectProperty")
  
  # get all domains and ranges for object properties
  objp_dr <- dom_res %>% 
    full_join(ran_res) %>% 
    right_join(obj_props %>% select(predicate))
  
  dr_excep <- objp_dr %>% 
    filter(is.na(domain) | is.na(range)) %>% 
    mutate(reason = "Domain and/or range for owl:ObjectProperty not specified.")
}

# check that a variable name is associated with each datatype property
checkVNDataProp <- function(rdf_data,
                            pred_vn,
                            vn_res) {
  
  # get all datatype properties 
  predicates <- names(pred_vn)
  predicate_filter <- paste(sprintf("<%s>", predicates), collapse = ", ")
  
  dtp_q <- sprintf("
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?predicate ?property
  WHERE { ?predicate rdf:type ?property .
    FILTER(?predicate IN (%s))
  }
  ", predicate_filter)
  
  dt_props <- rdf_query(rdf_data, dtp_q) %>% 
    filter(property == "http://www.w3.org/2002/07/owl#DatatypeProperty")
  
  full_res <- full_join(dom_res, ran_res)
  
  dt_excep <- vn_res %>% 
    filter(is.na(varname)) %>% 
    inner_join(dt_props %>% select(predicate)) %>% 
    left_join(full_res) %>% 
    mutate(reason = "No variable name associated with owl:DatatypeProperty")
  
  return(dt_excep)
}

# check that each property that has no domain has an associated variable name
checkVNDomain <- function(pred_vn,
                          dom_res, 
                          ran_res, 
                          vn_res) {
  
  pred_tib <- tibble(predicate = names(pred_vn))
  no_domain <- full_join(dom_res, pred_tib) %>% 
    filter(is.na(domain))
    
  dom_excep <- no_domain %>% 
    inner_join(vn_res %>% filter(is.na(varname))) %>% 
    left_join(ran_res) %>% 
    mutate(reason = "No domain found and no variable name specified")
}
