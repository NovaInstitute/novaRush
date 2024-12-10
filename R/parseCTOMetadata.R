# create node specifications and predicate mappings for metadata that is always (or mostly always) present for SurveyCTO surveys
# output: SPO table

# node specifications:
# interview
# interviewresponse
# survey
# int_dev
# hh_addr
# hh

# predicate specifications:
# startedattime
# endedattime
# duration
# gps_location
# hasinterviewresponse
# conductedwithdevice
# interviewwithhh
# hhhasaddr
# devicephonenum
# deviceinfo

# TODO handle NULL parameter values
parseCTO <- function(data,
                     starttime = "starttime",
                     endtime = "endtime",
                     devicephonenum = "devicephonenum",
                     device_info = "device_info",
                     duration = "duration",
                     geo_location = "gps_location",
                     village = "village",
                     standnumber = "stand_number_1",
                     survey,
                     ...) {
  
  # create node specifications
  int_spec <- list(
    type = "https://nova.org.za/nova-o#Interview",
    id_col = "instanceid",
    comb_id_col = NULL
  )
  
  int_resp <- list(
    type = "https://nova.org.za/nova-o#InterviewResponse",
    id_col = NULL,
    comb_id_col = NULL
  )
  
  # TODO what is the best way to handle this?
  survey_spec <- list(
    type = "https://nova.org.za/nova-o#Survey",
    id_col = NULL,
    comb_id_col = NULL
  )
  
  hh_spec <- list(
    type = "https://nova.org.za/nova-o#Household",
    id_col = NULL,
    comb_id_col = c("village", "stand_number_1", "respondent_surname")
  )
  
  hh_addr_spec <- list(
    type = "https://nova.org.za/nova-o#HouseholdAddress",
    id_col = NULL,
    comb_id_col = NULL
  )
  
  intdev_spec <- list(
    type = "https://nova.org.za/nova-o#InterviewDevice",
    id_col = "deviceid",
    comb_id_col = NULL
  )
  
  node_spec <- list(int_spec, int_resp, survey_spec, hh_spec, hh_addr_spec, intdev_spec)
  node_result <- identify_nodes(node_spec, data)
  id_data <- node_result$data
  id_tb <- node_result$id_tb
  
  # create predicate specifications that map nodes to one another
  varnames <- c(starttime, endtime, devicephonenum, device_info, 
                duration, geo_location, village, standnumber)

  # TODO autofill inverse properties?
  predIRIs <- c(
    "http://www.w3.org/ns/prov#startedAtTime",
    "http://www.w3.org/ns/prov#endedAtTime",
    "https://www.w3.org/2006/vcard/ns#hasTelephone",
    "https://nova.org.za/nova-o#hasDeviceInfo",
    "http://purl.org/aiaontology#hasDuration",
    "http://purl.org/aiaontology#hasLocation",
    "https://nova.org.za/nova-o#inVillage",
    "https://nova.org.za/nova-o#hasStandNumber",
    # predicates that map nodes to one another
    "https://nova.org.za/nova-o#conductedWithDevice",
    "https://nova.org.za/nova-o#isResponseOf",
    # TODO technically the domain and range of this predicate are Instrument and Activity, respectively
    "http://purl.org/aiaontology#usedToPerform",
    "https://nova.org.za/nova-o#interviewWithHousehold",
    "https://nova.org.za/nova-o#hasAddress"
  )

  # TODO - autofill domains and ranges if possible from pred IRI?
  domains <- c(
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#InterviewDevice",
    "https://nova.org.za/nova-o#InterviewDevice",
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#HouseholdAddress",
    "https://nova.org.za/nova-o#HouseholdAddress",
    # predicates that map nodes to one another
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#InterviewResponse",
    "https://nova.org.za/nova-o#Survey",
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#Household"
    
  )

  ranges <- c(
    "http://www.w3.org/2001/XMLSchema#dateTime",
    "http://www.w3.org/2001/XMLSchema#dateTime",
    "http://www.w3.org/2001/XMLSchema#string",
    "http://www.w3.org/2001/XMLSchema#string",
    "https://www.w3.org/2006/time#Duration",
    "http://purl.org/aiaontology#Location",
    "https://nova.org.za/nova-o#Village",
    "http://www.w3.org/2001/XMLSchema#string",
    # predicates that map nodes to one another
    "https://nova.org.za/nova-o#InterviewDevice",
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#Interview",
    "https://nova.org.za/nova-o#Household",
    "https://nova.org.za/nova-o#HouseholdAddress"
  )

  # apply predicate mappings - non-ID and ID predicates respectively
  
  # pad varnames with NAs for predicates that have no corresponding column
  length(varnames) <- length(predIRIs)
  
  predlist <- mapPredicates(varnames, predIRIs, domains, ranges)
  prefixed <- replace_iris_with_prefixes(predlist)
  pred_tib <- predicateTibble(prefixed)
  
  id_predlist <- specIDPredicates(id_data, id_tb) # id_tb from identify_nodes
  id_prefixed <- replace_iris_with_prefixes(id_predlist)
  pred_id_tb <- predicateTibble(id_prefixed)
  
  # join id and non-id predicate mappings
  pred_tib <- rbind(pred_tib, pred_id_tb)
  
  # map data
  mapped <- pivotLongerSPO(id_data, pred_tib)
  
  return(mapped)
}
