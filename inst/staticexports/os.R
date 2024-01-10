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
