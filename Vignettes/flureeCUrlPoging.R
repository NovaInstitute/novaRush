


# Wrtit this in httr
library(httr)
library(jsonlite)

API_KEY <- "QuALrBMCulZLPwocVX0FyNTV7-GxlXLUrBb_w-NNRzghklpC2LlZD4D1_8enTRb1r2uJaUqjbnqE_qS-aP2o4g"

transaction_type <- c("transact", "query", "history")[2]
ledger <- "christiaanpauw/test1"
url <- glue::glue("https://data.flur.ee/fluree/{transaction_type}")

context <- list(
  nova = "http://nova.org.za/nova-o",
  prov = "http://www.w3.org/ns/prov#",
  aia = "http://purl.org/aiaontology#",
  schema = "http://schema.org/"
)

dfcontext <- context %>% as_tibble() %>% pivot_longer(cols = everything()) %>% set_names("prefix", "IRI")

# Add what content tye it is
headers <- add_headers(
  `Content-Type` = "application/json",
  `Authorization` = paste("Bearer", API_KEY)
)

body <- list(
  `@context` = context,
  from = ledger,
  select = list(
    "?s" = list("*")
  ),
  where = list(
    "@id" = "?s",
    "@type" = "f:AccessPolicy"
  )
)

response <- httr::POST(url, headers, body = toJSON(body, auto_unbox = TRUE))

# Upload schema first
df1 <- pivot_longer_with_type(iris)

transaction_type <- c("transact", "query", "history")[1]
schema_url <- glue::glue("https://data.flur.ee/fluree/{transaction_type}")

headers <- add_headers(
  `Content-Type` = "application/json",
  `Authorization` = paste("Bearer", API_KEY)
)

schema_body <- c(ledger = ledger,
                 tx = schema_from_tripples(df = df1, name = "iris"))

schema_response <- httr::POST(schema_url, headers, body = toJSON(schema_body, auto_unbox = TRUE), encode = "json")
schema_response_content <- content(schema_response, as = "text")
schema_response_content



