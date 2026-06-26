# connect to data.flur.ee
library(httr)
library(jsonlite)
library(tidyverse)

CJP1_APIKEY <- "Qcv6uCMxzYRhHyZbixzgl8FuV5jfS2s16qml4nzATe1GNVl7XEJIMnhaUP61ygqjNczHVJEp_4U7ow0gqtb59g"
#key_prefix <- "dfdlb"
fluree_base_url <- "https://data.flur.ee/api/fluree/" # [ query | transact | history ]
ledger_base <- "christiaanpauw"
transaction_type <- c("transact", "query", "history")[2]
datasets_url <- "https://data.flur.ee/datasets?owner&collaborator"
test1_ledger <- "christiaanpauw/test1"


ledgers_query <- list(
  select = list("id"),
  from = datasets_url,
  where = list(list("@id", "?s"))
)

res <- httr::POST(
  sprintf("https://data.flur.ee/api/%s/create-dataset", handle),
  add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer ", CJP1_APIKEY),
    "x-user-handle" =  ledger_base
  ),
  body = toJSON(ledgers_query, auto_unbox = TRUE)
)

# Check status
status_code(res)

# Parse response
content <- content(res, "text", encoding = "UTF-8")
ledgers <- fromJSON(content)
print(ledgers)

