is_windows <- function() .Platform$OS.type == "windows"

is_mac     <- function() Sys.info()[['sysname']] == 'Darwin'

is_linux   <- function() Sys.info()[['sysname']] == 'Linux'

os_name <- function() {
  if (is_windows()) {
    "win"
  } else if (is_mac()) {
    "mac"
  } else if (is_linux()) {
    "linux"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    "unknown"
  }
}
