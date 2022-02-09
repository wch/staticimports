is_rcmd_check <- function() {
  Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
}
