# =====================================================================
# purrr-like functions
# =====================================================================
# These functions provide a similar API to purrr, but some are significantly
# faster.

walk <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  NULL
}

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_lgl <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA)
  }
}

map_int <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_integer_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_integer_)
  }
}

map_dbl <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_real_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_real_)
  }
}

map_chr <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_character_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_character_)
  }
}



walk2 <- function(.x, .y, .f, ...) {
  if (length(.x) != length(.y)) {
    stop(".x and .y must be the same length.")
  }
  for (i in seq_along(.x)) {
    .f(.x[[i]], .y[[i]], ...)
  }
  NULL
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, ..., SIMPLIFY = FALSE)
}
