library(jsonlite)  # construction of JSON objects
library(httr) # HTTP requests
library(flureeCrypto) # signature creation

# Run the docker image
system("docker run -d -p 58090:8090 --name node_test fluree/server:5839ffe273062b8da972b120deb54dd62e7c3d1f")

# Set environment variables
#        this should be done whenever a new instance is initialised
Sys.setenv(PORT = "58090", LEDGER_NAME = "signingTest", ENDPOINT = "create", "private_key" = '913524961748600e1a7fd57e8724d2c3ddaa5b5377e0985e873c7f5294a480d1')

# Generate account ID
accountID <- flureeCrypto::account_id_from_private(Sys.getenv("private_key"))
fluree_did <- sprintf("did:fluree:%s", accountID)

# Construct the transaction body
#            the following transaction adds 2 users to the ledger "signingTest"
#            it also adds a "f:view" user policy

payload1 <- list(
  insert = list(
    list(
      '@id' = 'ex:alice',
      '@type' = 'ex:User',
      'ex:secret' = "alice's secret"
    ), list(
      '@id' = 'ex:bob',
      '@type' = 'ex:User',
      'ex:secret' = "bob's secret"
    ), list(
      '@id' = 'ex:userPolicy',
      '@type' = list('f:AccessPolicy', 'ex:UserPolicy'),
      'f:action' = list(list('@id' = 'f:view')),
      'f:query' = list(
        '@type' = '@json',
        '@value' = {}
        )
      ), list(
        '@id' = 'ex:secretsPolicy',
        '@type' = list('f:AccessPolicy', 'ex:UserPolicy'),
        'f:onProperty' = list(
          '@id' = 'ex:secret'
        ),
        'f:action' = list(list( '@id' = 'f:view')),
        'f:query' = list(
          '@type' = '@json',
          '@value' = list(
            '@context' = list(
              f = 'https://ns.flur.ee/ledger#',
              ex = 'http://example.org/'
            ),
            where = list(
              '@id' = '?$identity',
              'ex:user' = list(
                '@id' = '?$this'
              )
            )
          )
        )
      ), list(
        '@id' = fluree_did,
        'ex:user' = list(
          '@id' = 'ex:alice'
        ),
        'f:policyClass' = list(
          '@id' = 'ex:UserPolicy'
        )
      )
    ),
  ledger = Sys.getenv("LEDGER_NAME"),
  '@context' = list(
    f = 'https://ns.flur.ee/ledger#',
    ex = 'http://example.org/'
  )
)

body <- toJSON(payload1, auto_unbox = TRUE, pretty = TRUE)

# Ledger creation with the above transaction as its first entry
port <- Sys.getenv("PORT")
endpoint <- Sys.getenv("ENDPOINT")
base_url <- sprintf("http://localhost:%s/fluree/%s", port, endpoint)

# Send POST request
response <- POST(
  url = base_url,
  add_headers(`Content-Type` = "application/json"),
  body = body,
  encode = "raw"  # Send raw string data
)

# Output the results
print(content(response, as = "text"))









# Now attempt to modify the ledger without modify permissions:

payload2 <- list(
  insert = list(
    list(
      `@id` = "ex:alice",
      `ex:secret` = "alice's new secret"
    )
  ),
  ledger = "signingTest",
  `@context` = list(
    f = "https://ns.flur.ee/ledger#",
    ex = "http://example.org/"
  )
)

body <- toJSON(payload2, auto_unbox = TRUE, pretty = TRUE)

signing_input <- toString(body)

jwt <- flureeCrypto:::serialize_jws(payload = signing_input, signing_key = Sys.getenv("private_key"))


# The endpoint needs to change once the ledger already exists
endpoint <- "transact"
base_url <- sprintf("http://localhost:%s/fluree/%s", port, endpoint)

# Try sending the signed transaction without modify permissions
response <- POST(
  url = base_url,
  add_headers(`Content-Type` = "application/jwt"),
  body = jwt,
  encode = "raw"  # Send raw string data
)

# Output the results
print(content(response, as = "text"))





# Add modify permissions to the ledger

payload3 <- list(
  insert = list(
    list(
      `@id` = "ex:secretsPolicy",
      `f:action` = list(
        `@id` = 'f:modify' 
      )
    )
  ),
  ledger = "signingTest",
  `@context` = list(
    f = "https://ns.flur.ee/ledger#",
    ex = "http://example.org/"
  )
)

body <- toJSON(payload3, auto_unbox = TRUE, pretty = TRUE)

response <- POST(
  url = base_url,
  add_headers(`Content-Type` = "application/json"),
  body = body,
  encode = "raw"  # Send raw string data
)

# Output the results
print(content(response, as = "text"))








# Now try sending the signed transaction again with modify permissions
response <- POST(
  url = base_url,
  add_headers(`Content-Type` = "application/jwt"),
  body = jwt,
  encode = "raw"  # Send raw string data
)

# Output the results
print(content(response, as = "text"))
