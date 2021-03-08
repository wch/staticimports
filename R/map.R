map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

walk <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  NULL
}
