
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_AsIs <- function(x) {
  inherits(x, "AsIs")
}

is_html_chr <- function(x) {
  is.character(x) && inherits(x, "html")
}
