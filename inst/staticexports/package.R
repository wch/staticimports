is_loaded <- function(x) {
  !is.null(.getNamespace(x))
}

is_installed <- function(pkg, version = NULL) {
  installed <- isNamespaceLoaded(pkg) || nzchar(system.file(package = pkg))
  if (is.null(version)) {
    return(installed)
  }
  installed && isTRUE(get_package_version(pkg) >= version)
}

# Since I/O can be expensive, only utils::packageVersion() if the package isn't already loaded
get_package_version <- function(pkg) {
  ns <- .getNamespace(pkg)
  if (is.null(ns)) {
    utils::packageVersion(pkg)
  } else {
    as.package_version(ns$.__NAMESPACE__.$spec[["version"]])
  }
}

