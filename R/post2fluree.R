

#' post2fluree
#' @description
#' POST transaction data to Fluree.
#'
#' @param signature Character. The signature to be included in the headers. The the result
#' of the sign_query function.
#' @param transaction_data Character. The data to be sent to Fluree. JSON string.
#' @param fluree_link Character. The URL of the Fluree server.
#' @param endpoint Character. The endpoint to be used. Default is "fluree/create".
#' @param privateKey Character. The private key to be used to sign the query. Either a .pem file or of class "key" or "ecdsa".
#' @param tx_type Character. The type of transaction. Default is "transact" # check fluree docs for types
#' @param ledger Character. The name of the ledger.
#'
#' @return Response from the POST request.
#' @export
#'
#' @examples
#' transaction_data <- list(`@context` = list(ex = "http://example.org/", schema = "http://schema.org/"),
#' ledger = "cookbook/base",
#' insert = structure(list(`@id` = "ex:freddy", `@type` = "ex:Yeti", `schema:age` = 4L, `schema:name` = "Freddy",  `ex:verified` = TRUE),
#' class = "data.frame", row.names = 1L))
#' pk <- "private_key.pem"
#' p1 <- post2fluree(signature = NULL, privateKey = pk, tx_type = "transact", ledger = "cookbook/base", transaction_data = transaction_data)

post2fluree <- function(signature = NULL,
                        privateKey = NULL,
                        tx_type = "transact",
                        ledger,
                        transaction_data,
                        fluree_link = "http://localhost:8095/",
                        endpoint= "fluree/create") {
  URL <- glue::glue("{fluree_link}{endpoint}")
  # test if it is a valid URL
  if (RCurl::url.exists(URL, .header = TRUE) %>% `[`("statusMessage") == "Not Found" ) {
    stop("Please provide a valid URL.")
  }

  # Either signature or privateKey must be provided
  if (is.null(signature) & is.null(privateKey)) {
    stop("Please provide a signature or a private key.")
  }

  # If signature is not provided and privateKey is, check if the private key a .pem file or of class "key" or  "ecdsa"
  if (is.null(signature) & !is.null(privateKey)) {
    # make sure ledger and transaction_data are provided
    if (is.null(ledger) | is.null(transaction_data)) {
      stop("Please provide the ledger and transaction data.")
    }
    # Check if the private key a .pem file or of class "key" or  "ecdsa"
    if (!inherits(privateKey, "key") & !inherits(privateKey, "ecdsa")) {
      # Check if the private key is a .pem file
      if (grepl(".pem", privateKey)) {
        privateKey <- openssl::read_key(privateKey)
      } else {
        stop("Please provide a valid private key\n
             - which is either a .pem file or of class 'key' or 'ecdsa' .")
      }
    }
    signature <- sign_query(private_key = privateKey, transaction_data = transaction_data, tx_type = tx_type, ledger = ledger)
  }

  response <- httr::POST(
    url = URL,
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = signature  # Include the signature in the headers
    ),
    body = transaction_data,
    encode = "json"
  )
  response
}


#' makeTransaction
#' @description
#' Make a transaction_data object to be sent to Fluree.
#' @param id Character. The id of the object to be inserted or deleted.
#' @param transaction_type Character. The type of transaction. Default is "insert".
#' @param data Data frame with two columns: Key and Value. The data to be inserted or deleted.
#' The first column is the key and the second column is the value.
#' @example
#' transaction_data <- list(`@context` = list(ex = "http://example.org/", schema = "http://schema.org/"),
#' ledger = "cookbook/base",
#' insert = structure(list(`@id` = "ex:freddy", `@type` = "ex:Yeti", `schema:age` = 4L, `schema:name` = "Freddy",  `ex:verified` = TRUE),
#' class = "data.frame", row.names = 1L))

makeTransaction <- function(id = NULL,
                            transaction_type = c("insert", "delete"),
                            data) {
  data <- data.frame(data)
  data <- data %>% dplyr::mutate(`@id` = id, `@type` = type)
  data
}
