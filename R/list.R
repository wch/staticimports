
escape_dbl_quotes <- function(x) {
  gsub('"', '\\\\"', x)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
