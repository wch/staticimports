is_windows <- function() .Platform$OS.type == "windows"

is_unix <- function() .Platform$OS.type == "unix"

is_mac <- function() Sys.info()[["sysname"]] == "Darwin"

is_linux <- function() Sys.info()[["sysname"]] == "Linux"

is_emscripten <- function() Sys.info()[["sysname"]] == "Emscripten"

os_name <- function() {
  if (is_windows()) {
    "win"
  } else if (is_mac()) {
    "mac"
  } else if (is_linux()) {
    "linux"
  } else if (is_emscripten()) {
    "emscripten"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    "unknown"
  }
}

is_r_version <- function(min, max = min) {
  v <- .get_r_version()

  if (missing(min)) min <- NULL
  if (is.null(min)) min <- "0.0.0"
  stopifnot(is.character(min))

  if (is.character(min) && missing(max)) {
    min_len <- length(strsplit(min, "\\.")[[1]])
    if (min_len == 1) {
      max <- paste0(min, ".9999.9999")
      min <- paste0(min, ".0.0")
    } else if (min_len == 2) {
      max <- paste0(min, ".9999")
      min <- paste0(min, ".0")
    }
  }

  if (is.null(max)) max <- max(min, as.character(v))
  stopifnot(is.character(max))

  v >= min && v <= max
}

.get_r_version <- function() {
  getRversion()
}
