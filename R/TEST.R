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
library(novaFluree)

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

# -----------------------------------------------------------------------------
# 2. CREATE SCHEMA
# -----------------------------------------------------------------------------

# approaches to schema design:
# a) manually design a schema
# b) AI augmentation with human moderation

# a) Use updated function mapPredicates.R to create predicate mappings
# Each predicate mapping contains:
# 1. @id: the predicate IRI
# 2. domain
# 3. range
# 4. variable name corresponding to this predicate
# only 1. - 3. will be included in the eventual Fluree transaction

varnames <- kia_adapt$data$kia_adaptation %>% 
  select(1:9) %>% # small subset of columns for development
  select(-devicephonenum) %>% # this column is all NAs
  colnames()
# "instanceid", "starttime", "endtime", "deviceid, "device_info", "duration", "village", "stand_number_1"

predIRIs <- c(
  "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
  "http://www.w3.org/ns/prov#startedAtTime",
  "http://www.w3.org/ns/prov#endedAtTime",
  "https://nova.org.za/nova-o#conductedWith",
  "https://nova.org.za/nova-o#hasDeviceInfo",
  "http://purl.org/aiaontology#hasDuration", # TODO edit in aia-o later to have domain = Activity?
  "https://nova.org.za/nova-o#inVillage",
  "https://nova.org.za/nova-o#hasStandNumber"
)

# TODO - autofill domains and ranges if possible from pred IRI?
domains <- c(
  "http://www.w3.org/2002/07/owl#Thing",
  "https://nova.org.za/nova-o#Survey", # NOTE: not the case in general, only for this dataframe
  "https://nova.org.za/nova-o#Survey",
  "https://nova.org.za/nova-o#Survey",
  "https://nova.org.za/nova-o#InterviewDevice",
  "https://nova.org.za/nova-o#Survey",
  "https://nova.org.za/nova-o#HouseholdAddress",
  "https://nova.org.za/nova-o#HouseholdAddress"
)

ranges <- c(
  "https://nova.org.za/nova-o#Survey",
  "http://www.w3.org/2001/XMLSchema#dateTime",
  "http://www.w3.org/2001/XMLSchema#dateTime",
  "http://www.w3.org/2001/XMLSchema#string",
  "http://www.w3.org/2001/XMLSchema#string",
  "https://www.w3.org/2006/time#Duration",
  "https://nova.org.za/nova-o#Village",
  "http://www.w3.org/2001/XMLSchema#string"
)

small_predlist <- mapPredicates(varnames, predIRIs, domains, ranges)

# replace long IRIs with prefixed IRIs
geprefix <- replace_iris_with_prefixes(small_predlist)

# -----------------------------------------------------------------------------
# 3. MAP DATA
# -----------------------------------------------------------------------------
small_kia_data <- kia_data %>% # small subset of rows for development
  select(1:9) %>% 
  select(-devicephonenum) %>% 
  head(n = 10)

# create dataframe with correct predicate names
triptib <- pivot_longer_with_type(small_kia_data) %>% 
  left_join(pred_tb, by = join_by(predicate == varname)) %>% 
  select(subject, `@id`, object)

small_kia_trip <- rdf_from_df3(triptib, subject = "subject", predicate = "@id", object = "object")
rdflib::rdf_serialize(small_kia_trip, format = "turtle", doc = "rdf_graph.ttl")

