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


#' Autofill standard triples for SurveyCTO triples
#' 
#' TODO not complete
#' 
#' This function:
#'  
#'  a) Constructs node (class) specifications compliant with [identify_nodes()] for the following classes from nova-o: 
#'  `Interview`, `InterviewResponse`, `Survey`, `InterviewDevice`, `HouseholdAddress`, and `Household` 
#'  
#'  b) Constructs, where necessary, additional `<classname>_ID` variables used to identify nodes (instances of classes) above
#'  
#'  c) Constructs predicate mappings for the following predicates:
#'  `http://www.w3.org/ns/prov#startedAtTime`,
#' `http://www.w3.org/ns/prov#endedAtTime`,
#' `https://www.w3.org/2006/vcard/ns#hasTelephone`,
#' `https://nova.org.za/nova-o#hasDeviceInfo`,
#' `http://purl.org/aiaontology#hasDuration`,
#' `http://purl.org/aiaontology#hasLocation`,
#' `https://nova.org.za/nova-o#inVillage`,
#' `https://nova.org.za/nova-o#hasStandNumber`,
#' `https://nova.org.za/nova-o#conductedWithDevice`,
#' `https://nova.org.za/nova-o#isResponseOf`,
#' `http://purl.org/aiaontology#usedToPerform`,
#' `https://nova.org.za/nova-o#interviewWithHousehold`,
#' `https://nova.org.za/nova-o#hasAddress`
#'  
#'  d) Maps `data` to SPO format using the above class specifications and predicates. 
#'
#' @param data [data.frame] The survey response data
#' @param instanceid [string] Column in `data` that contains ID for interview
#' @param starttime [string] Column in `data` that contains starting time for interview
#' @param endtime [string] Column in `data` that contains ending time for interview
#' @param devicephonenum  [string] Column in `data` that contains phone number for the device used to conduct the survey
#' @param device_info [string] Column in `data` that contains information on the device used to conduct the interview
#' @param duration [string] Column in `data` that contains duration of the interview
#' @param geo_location [string] Column in `data` that contains location at which interview was conducted
#' @param village [string] Column in `data` that contains the village respondent lives in
#' @param standnumber [string] Column in `data` that contains the respondent's stand number
#' @param survey [string] Identifier for the survey these responses form part of.
#'
#' @return [data.frame] Containing columns `subject`, `predicate`, `object`. This dataframe can be parsed to triples.
#' @export
parseCTO <- function(data,
                     instanceid = "instanceid",
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
    comb_id_col = NULL,
    const_id = NULL
  )
  
  int_resp <- list(
    type = "https://nova.org.za/nova-o#InterviewResponse",
    id_col = NULL,
    comb_id_col = c("instanceid", "starttime", "endtime", "deviceid", "device_info", "duration"),
    const_id = NULL
  )
  
  survey_spec <- list(
    type = "https://nova.org.za/nova-o#Survey",
    id_col = NULL,
    comb_id_col = NULL,
    const_id = "KiA_adaptation_Q"
  )
  
  hh_spec <- list(
    type = "https://nova.org.za/nova-o#Household",
    id_col = NULL,
    comb_id_col = c("village", "stand_number_1", "respondent_surname"),
    const_id = NULL
  )
  
  hh_addr_spec <- list(
    type = "https://nova.org.za/nova-o#HouseholdAddress",
    id_col = NULL,
    comb_id_col = c("village", "stand_number_1"),
    const_id = NULL
  )
  
  intdev_spec <- list(
    type = "https://nova.org.za/nova-o#InterviewDevice",
    id_col = "deviceid",
    comb_id_col = NULL,
    const_id = NULL
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
  
  id_predlist <- specIDPredicates(id_tb) # id_tb from identify_nodes
  id_prefixed <- replace_iris_with_prefixes(id_predlist)
  pred_id_tb <- predicateTibble(id_prefixed)
  
  # join id and non-id predicate mappings
  pred_tib <- rbind(pred_tib, pred_id_tb)
  
  # map data
  mapped <- pivotLongerSPO(id_data, pred_tib)
  
  return(mapped)
}
