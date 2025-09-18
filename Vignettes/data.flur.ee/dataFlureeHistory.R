
dataFLureeHistory <- function(ctx = NULL, api_key = NULL, ledger, subject, predicate) {
  if (is.null(ctx)) {
    ctx <- list(schema="http://schema.org/", ex="http://example.org/")
  }

  if (is.null(api_key)) {
    api_key <- Sys.getenv("FLUREE_API_KEY")
    if (api_key == "") {
      stop("Please provide a Fluree API key either as an argument or by setting the FLUREE_API_KEY environment variable.")
    }
  }
  ctx <- list(schema="http://schema.org/", ex="http://example.org/")
  h <- list("@context" = ctx,
            from       = ledger,
            history    = list("ex:alice", "schema:name")  # subject, predicate
            )

res_h <- httr::POST(
  "https://data.flur.ee/fluree/history",
  httr::add_headers(
    "Content-Type"="application/json","Accept"="application/json",
    "Authorization"=paste("Bearer", api_key),
    "x-user-handle"=handle
  ),
  body = jsonlite::toJSON(h, auto_unbox=TRUE),
  encode="raw"
)
cat(httr::content(res_h, "text", encoding="UTF-8"))
}
