is_loaded <- function(x) {
  !is.null(.getNamespace(x))
}

is_installed <- function(package, version = NULL) {
  installed <- requireNamespace(package, quietly = TRUE)
  if (is.null(version)) {
    return(installed)
  }
  installed && isTRUE(fastPackageVersion(package) >= version)
}

# Since I/O can be expensive, only utils::packageVersion() if the package isn't already loaded
fastPackageVersion <- function(pkg) {
  ns <- .getNamespace(pkg)
  if (is.null(ns)) {
    utils::packageVersion(pkg)
  } else {
    as.package_version(ns$.__NAMESPACE__.$spec[["version"]])
  }
}

