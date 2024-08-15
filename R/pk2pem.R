
#' flureePK2pem
#' @description
#' Function to convert a Fluree private key to a PEM file.
#'
#' @param k Character. The 64 character private key.
#' @param file_path Character. The path to save the PEM file. Default is "private_key.pem".
#' @param writePEM Logical. If TRUE, the PEM file will be saved to the file_path.
#' If FALSE, returns the key as class  "key" "ecdsa". Default is TRUE.
#' @return Writes to file_path and returns a message
#' @export
#' @importFrom digest digest
#' @importFrom gmp as.bigz
#' @examples
#' flureePK2pem(k, "private_key.pem")

flureePK2pem <- function(k,  file_path = "private_key.pem", writePEM = TRUE) {
  # Convert the hex key into a big integer
  key_int <- gmp::as.bigz(paste0("0x", k))

  # Convert the big integer to a binary raw vector
  key_raw <- as.raw(gmp::as.bigz(key_int))

  private.key <- digest::digest(as.character(key_raw))
  pub.key <- sub(".*:\\s+", "", capture.output(key_raw)[2])
  # private.key

  # Export the key in PEM format
  pem_key <- openssl::write_pem(private_key)
  if (!writePEM) return(openssl::read_key(textConnection(pem_key)))

  # Save to a file
  writeLines(pem_key, con = file_path)
  message("The private key has been saved to ", file_path)
}
