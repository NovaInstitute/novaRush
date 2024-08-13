

source("~/novaRush/R/sign_query.R")
source("~/novaRush/R/pk2pem.R")


# Your transaction data
transaction_data <- toJSON(list(
  `@context` = list(
    ex = "http://example.org/",
    schema = "http://schema.org/"
  ),
  ledger = "cookbook/base",
  insert = list(
    list(
      `@id` = "ex:freddy",
      `@type` = "ex:Yeti",
      `schema:age` = 4,
      `schema:name` = "Freddy",
      `ex:verified` = TRUE
    )
  )
), auto_unbox = TRUE)

# Signing the transaction
tx_type <- "transact"
ledger <- "cookbook/base"
signature <- sign_query(private_key = "private_key.pem", transaction_data, tx_type, ledger)

# Make the HTTP POST request
response <- POST(
  url = "http://localhost:8095/fluree/create",
  add_headers(
    "Content-Type" = "application/json",
    "Authorization" = signature  # Include the signature in the headers
  ),
  body = transaction_data,
  encode = "json"
)

# Print the response
print(content(response, "text"))
