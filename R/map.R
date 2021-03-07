map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_null <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  NULL
}
