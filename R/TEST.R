# -----------------------------------------------------------------------------
# FLUREEFICATION
# -----------------------------------------------------------------------------
# 1. Data preprocessing
# 2. Create a schema
# 3. Map data from dataframe to JSON-LD using schema from 2.
# 4. Create a Fluree transaction
# 5. Transact to Fluree ledger

# -----------------------------------------------------------------------------
# 1. PREPROCESS DATA
# -----------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(novaCTO)
library(novaRush)
# library(novaFluree)
library(openssl)
library(rlang)
library(stringr)

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

# select small subset of rows to use as example for development
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

# create node identification specification
survey_spec <- list(
  type = "https://nova.org.za/nova-o#Survey",
  id_col = "instanceid",
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

node_spec <- list(survey_spec, hh_spec, hh_addr_spec, intdev_spec)
node_result <- identify_nodes(node_spec, small_kia_data)
small_kia_data <- node_result$data
id_tb <- node_result$id_tb

# -----------------------------------------------------------------------------
# 2.2 PREDICATE MAPPINGS
# -----------------------------------------------------------------------------

# Use updated function mapPredicates.R to create predicate mappings
# Each predicate mapping contains:
# 1. @id: the predicate IRI
# 2. domain
# 3. range
# 4. variable name corresponding to this predicate
# only 1. - 3. will be included in the eventual Fluree transaction

# NOTE: predicates for variables that correspond to IDs, i.e. those variables that are used as subjects in triples,
# are not specified manually, but rather automatically generated from node specifications from the function mapIDPredicates

varnames <- kia_adapt$data$kia_adaptation %>% 
  select(1:9) %>% # small subset of columns for development
  select(-devicephonenum,) %>% # this column is all NAs
  select(-instanceid, -deviceid) %>% # exclude ID predicates, they are handled separately
  colnames()
# starttime", "endtime", "device_info", "duration", "village", "stand_number_1"

predIRIs <- c(
  "http://www.w3.org/ns/prov#startedAtTime",
  "http://www.w3.org/ns/prov#endedAtTime",
  "https://nova.org.za/nova-o#hasDeviceInfo",
  "http://purl.org/aiaontology#hasDuration", # TODO edit in aia-o later to have domain = Activity?
  "https://nova.org.za/nova-o#inVillage",
  "https://nova.org.za/nova-o#hasStandNumber"
)

# TODO - autofill domains and ranges if possible from pred IRI?
domains <- c(
  "https://nova.org.za/nova-o#Survey", # NOTE: not the case in general, only for this dataframe
  "https://nova.org.za/nova-o#Survey",
  "https://nova.org.za/nova-o#InterviewDevice",
  "https://nova.org.za/nova-o#Survey",
  "https://nova.org.za/nova-o#HouseholdAddress",
  "https://nova.org.za/nova-o#HouseholdAddress"
)

ranges <- c(
  "http://www.w3.org/2001/XMLSchema#dateTime",
  "http://www.w3.org/2001/XMLSchema#dateTime",
  "http://www.w3.org/2001/XMLSchema#string",
  "https://www.w3.org/2006/time#Duration",
  "https://nova.org.za/nova-o#Village",
  "http://www.w3.org/2001/XMLSchema#string"
)

# -----------------------------------------------------------------------------
# 2.3 APPLY PREDICATE MAPPINGS
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 2.3.1 NON-ID PREDICATES
# -----------------------------------------------------------------------------

# predicate mapping as list
small_predlist <- mapPredicates(varnames, predIRIs, domains, ranges)

# replace long IRIs with prefixed IRIs
geprefix <- replace_iris_with_prefixes(small_predlist)

# predicate mapping as tibble
pred_tb <- predicateTibble(geprefix)

# -----------------------------------------------------------------------------
# 2.3.2 ID PREDICATES
# -----------------------------------------------------------------------------

small_id_predlist <- specIDPredicates(small_kia_data, id_tb)
id_geprefix <- replace_iris_with_prefixes(small_id_predlist)
small_id_tb <- predicateTibble(id_geprefix)

# join id and non-id predicate mappings
pred_tb <- rbind(pred_tb, small_id_tb)

# -----------------------------------------------------------------------------
# 3. MAP DATA
# -----------------------------------------------------------------------------

# create dataframe with correct predicate names
small_kia_long <- pivot_longer_with_type(small_kia_data)

triptib <- small_kia_long %>% 
  left_join(pred_tb, by = join_by(predicate == varname)) %>% 
  select(subject, `@id`, object) # TODO ADJUST THIS SELECTION

# create triples
small_kia_trip <- rdf_from_df3(triptib, subject = "subject", predicate = "@id", object = "object")

# export graph for inspection
rdflib::rdf_serialize(small_kia_trip, format = "turtle", doc = "rdf_graph.ttl")

