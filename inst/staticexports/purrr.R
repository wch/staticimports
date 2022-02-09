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
  res <- vector("list", length(.x))
  for (i in seq_along(.x)) {
    res[[i]] <- .f(.x[[i]], .y[[i]], ...)
  }
  names(res) <- names(.x)
  res
}

map2_lgl <- function(.x, .y, .f, ...) {
  res <- as.logical(map2(.x, .y, .f, ...))
  names(res) <- names(.x)
  res
}

map2_int <- function(.x, .y, .f, ...) {
  res <- as.integer(map2(.x, .y, .f, ...))
  names(res) <- names(.x)
  res
}

map2_dbl <- function(.x, .y, .f, ...) {
  res <- as.double(map2(.x, .y, .f, ...))
  names(res) <- names(.x)
  res
}

map2_chr <- function(.x, .y, .f, ...) {
  res <- as.character(map2(.x, .y, .f, ...))
  names(res) <- names(.x)
  res
}



vec_index <- function(x) {
  names(x) %||% seq_along(x)
}

iwalk <- function(.x, .f, ...) {
  walk2(.x, vec_index(.x), .f, ...)
}

imap <- function(.x, .f, ...) {
  map2(.x, vec_index(.x), .f, ...)
}

imap_lgl <- function(.x, .f, ...) {
  map2_lgl(.x, vec_index(.x), .f, ...)
}

imap_int <- function(.x, .f, ...) {
  map2_int(.x, vec_index(.x), .f, ...)
}

imap_dbl <- function(.x, .f, ...) {
  map2_dbl(.x, vec_index(.x), .f, ...)
}

imap_chr <- function(.x, .f, ...) {
  map2_chr(.x, vec_index(.x), .f, ...)
}

keep <- function(.x, .f, ...) {
  .x[vapply(.x, .f, ..., FUN.VALUE = NA)]
}

discard <- function(.x, .f, ...) {
  .x[!vapply(.x, .f, ..., FUN.VALUE = NA)]
}

compact <- function(.x) {
  Filter(length, .x)
}
