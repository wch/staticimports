
named_list_val <- list(a = 1)[0]
# Returns an empty named list
named_list <- function() named_list_val

named_chr_val <- c(a = "c")[0]
# Returns an empty named character vector
named_chr <- function() named_chr_val


# Add a class to x. Faster than `structure(x, class = class)`, which is
# surprisingly slow.
add_class <- function(x, class) {
  class(x) <- class
  x
}

escape_dbl_quotes <- function(x) {
  gsub('"', '\\\\"', x)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}



cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}
