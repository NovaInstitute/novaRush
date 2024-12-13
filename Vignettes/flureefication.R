# -----------------------------------------------------------------------------
# FLUREEFICATION - FROM RELATIONAL TO GRAPH DATABASES
# -----------------------------------------------------------------------------

# 1. Data preprocessing
# 2. Create a schema
# 3. Map data from dataframe to SPO triples using schema from 2.
# 4. Create a Fluree transaction
# 5. Transact to Fluree ledger

# -----------------------------------------------------------------------------
# 1. PREPROCESS DATA FOR CTO EXAMPLE: KIA ADAPTATION
# -----------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(novaCTO)
library(openssl)
library(rlang)
library(stringr)
library(jsonlite)

# load survey data
kia_adapt <- novaCTO::readCTO("KiA_adaptation_ACTIVE")

# unnest relevant structures
# for simple questions
kia_data <- kia_adapt$data$kia_adaptation

# for questions that are answered multiple times, once for each instance of a certain class
# e.g. the question "What species is this tree?" is answered once for each tree the household owns
repeats <- kia_adapt$repeats 

# to get variable names for later schema design
kia_varnames <- kia_adapt$fromschema$kia_adaptation$name # (list)

# select small subset of rows to use as example
small_kia_data <- kia_data %>%
  select(1:9, respondent_surname) %>% 
  select(-devicephonenum) %>% 
  head(n = 10)

# -----------------------------------------------------------------------------
# 2. CREATE SCHEMA
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 2.1 SPECIFY NODES
# -----------------------------------------------------------------------------

# Node specification as a list of named lists. There should be an entry for each class of entity implied by the data.
# Each named list should have the following fields:
# a) rdf:type for each node according to nova-o
# b) the variable that can be used as ID for the node
# c) if b) is NULL, which combination of columns can be used to create an ID for the node. The specified columns are hashed to create the ID.
# d) if both b) and c) are null, the constant identifier for this node, preferably as IRI.

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
node_result <- identify_nodes(node_spec, small_kia_data)
small_kia_data <- node_result$data
id_tb <- node_result$id_tb

# -----------------------------------------------------------------------------
# 2.2 PREDICATE MAPPINGS
# -----------------------------------------------------------------------------

# Each predicate mapping contains:
# 1. predicate: the predicate IRI
# 2. domain
# 3. range
# 4. variable name corresponding to this predicate

# NOTE: predicates for variables that correspond to IDs, i.e. those variables that are used as subjects in triples,
# are not specified manually, but rather automatically generated from node specifications from the function mapIDPredicates

varnames <- kia_adapt$data$kia_adaptation %>% 
  select(1:9) %>% # small subset of columns for development
  select(-devicephonenum,) %>% # this column is all NAs
  select(-instanceid, -deviceid) %>% # exclude ID predicates, they are handled separately
  colnames()
# starttime", "endtime", "device_info", "duration", "village", "stand_number_1"

pred_vn <- list(
  "http://www.w3.org/ns/prov#startedAtTime" = "starttime",
  "http://www.w3.org/ns/prov#endedAtTime" = "endtime",
  "https://nova.org.za/nova-o#hasDeviceInfo" = "device_info",
  "http://purl.org/aiaontology#hasDuration" = "duration",
  "https://nova.org.za/nova-o#inVillage" = "village",
  "https://nova.org.za/nova-o#hasStandNumber" = "stand_number_1",
  # predicates that map nodes to one another
  "https://nova.org.za/nova-o#conductedWithDevice" = NA,
  "https://nova.org.za/nova-o#isResponseOf" = NA,
  "http://purl.org/aiaontology#usedToPerform" = NA,
  "https://nova.org.za/nova-o#interviewWithHousehold" = NA,
  "https://nova.org.za/nova-o#hasAddress" = NA
)

# -----------------------------------------------------------------------------
# 2.3 APPLY PREDICATE MAPPINGS
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 2.3.1 NON-ID PREDICATES
# -----------------------------------------------------------------------------

# autofill information as far as possible
ontology_path = "../nova-o/nova-o.rdf" # REPLACE WITH YOURS
autofill <- suppressMessages(getPredicateInfo(ontology_path, pred_vn, id_tb))
pred_tb <- autofill$mapped 
exceptions <- autofill$exceptions

# address issues raised in "exceptions"
# a) nova-o:Village is in the range of nova-o:inVillage - amend node specification
# vill_spec <- list(
#   type = "https://nova.org.za/nova-o#Village",
#   id_col = NULL,
#   comb_id_col = NULL,
#   const_id = 
# ) # TODO add to id_tb and update pred_tb

# b) aia:hasDuration has no domain or range specified - specify manually and update pred_tb
varname <- c("duration")
predIRI <- c("http://purl.org/aiaontology#hasDuration")
domain <- c("https://nova.org.za/nova-o#Interview")
range <- c("http://www.w3.org/2001/XMLSchema#int")
amended <- mapPredicates(varname, predIRI, domain, range)

pred_tb <- rbind(pred_tb, amended)

# prefix verbose IRIs and extract context to use in body of Fluree transaction
prefixed <- prefixIRIs(pred_tb)
pred_tb <- prefixed$output
context <- prefixed$context

# -----------------------------------------------------------------------------
# 2.3.2 ID PREDICATES
# -----------------------------------------------------------------------------

small_id_tb <- specIDPredicates(id_tb)

# note that further wrangling expects prefixed IRIs; do this step here
id_pref_result <- prefixIRIs(small_id_tb)
id_geprefix <- id_pref_result$output

# join id and non-id predicate mappings
pred_tb <- rbind(pred_tb, id_geprefix)

# -----------------------------------------------------------------------------
# 3. MAP DATA
# -----------------------------------------------------------------------------

# create dataframe with correct subjects, predicates, and objects
small_kia_long <- pivotLongerSPO(small_kia_data %>% slice(1:2), pred_tb)

# create triples
small_kia_trip <- rdf_from_df3(small_kia_long)

# export graph for inspection
rdflib::rdf_serialize(small_kia_trip, format = "turtle", doc = "rdf_graph.ttl", base = "https://nova.org.za#")

# -----------------------------------------------------------------------------
# 4. CREATE A TRANSACTION
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 4.1 CREATE A LEDGER
# -----------------------------------------------------------------------------

# <novaRush Fluree functionality here>

# -----------------------------------------------------------------------------
# 4.1 CREATE TRANSACTION BODY
# -----------------------------------------------------------------------------

insert_body <- createBody("nova/kia", small_kia_long %>% distinct, context)

# -----------------------------------------------------------------------------
# 5. TRANSACT TO A LEDGER
# -----------------------------------------------------------------------------

# <novaRush Fluree functionality here>