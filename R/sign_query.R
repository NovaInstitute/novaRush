#' sign_query
#' @description Function to sign the query
#' @references  https://stackoverflow.com/questions/66554462/turn-rsa-public-and-private-keys-into-strings-in-r
#' @param private_key Character. The private key as class ""key" "ecdsa" or the path to a .pem file.
#' You can transform the private key to a .pem file using the function pk2pem.
#' @param transaction_data Character. The data to be signed. JSON string.
#' @param tx_type Character. The type of transaction. Default is "transact" # check fluree docs for types
#' @param ledger Character. The name of the ledger.
#' @return character
#' @export
#' @import openssl

sign_query <- function(private_key,
                       transaction_data,
                       tx_type = "transact",
                       ledger) {
  # Check if all arguments are provided
  if (missing(private_key) | missing(transaction_data) | missing(ledger)) {
    stop("Please provide all the necessary arguments.")
  }
  # Check if the private key a .pem file or of class "key" or  "ecdsa"
  if (!inherits(private_key, "key") & !inherits(private_key, "ecdsa")) {
    # Check if the private key is a .pem file
    if (grepl(".pem", private_key)) {
      private_key <- openssl::read_key(private_key)
    } else {
      stop("Please provide a valid private key\n
           - which is either a .pem file or of class 'key' or 'ecdsa' .")
    }
  }

  if (class(transaction_data) != "json") {
    transaction_data <- jsonlite::toJSON(transaction_data)
  }

  # Combine the necessary components
  message <- paste0(tx_type, ledger, transaction_data)

  # Hash the message
  hash <- openssl::sha256(charToRaw(message))

  # Sign the hashed message with the private key
  signature <- openssl::signature_create(data = hash, key = private_key, hash = openssl::sha256)

  # Encode the signature in base64
  openssl::base64_encode(signature)
}
