
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_shiny_tag <- function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list"))
}

is_AsIs <- function(x) {
  inherits(x, "AsIs")
}
