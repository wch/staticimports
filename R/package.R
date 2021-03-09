is_loaded <- function(x) {
  !is.null(.getNamespace(x))
}

is_installed <- function(x) {
  nzchar(system.file(package = x))
}
