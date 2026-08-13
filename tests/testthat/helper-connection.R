# check = FALSE builds a handle without touching the network, so every
# query-construction test runs offline.
test_connection <- function(ledger = "novaRush/test", context = c(schema = "http://schema.org/"),
                            branch = "main") {
  fluree_connect("localhost", ledger = ledger, port = 8090, context = context,
                 branch = branch, check = FALSE)
}

# the filter s-expression a single fq_filter() expression assembles to
filter_expr <- function(...) {
  q <- fluree_query(test_connection())
  q <- fq_where(q, `@id` = "?s")
  q <- fq_select(q, s)
  q <- fq_filter(q, ...)
  where <- fluree_query_body(q)$where
  filters <- Filter(function(w) is.list(w) && identical(w[[1]], "filter"), where)
  vapply(filters, function(w) w[[2]], character(1))
}
