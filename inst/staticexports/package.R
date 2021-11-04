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

# Wrapper around base::system.file. In base::system.file, the package directory
# lookup is a bit slow. This caches the package directory, so it is much faster.
system_file <- local({
  package_dir_cache <- character()

  function(..., package = "base") {
    if (!is.null(names(list(...)))) {
      stop("All arguments other than `package` must be unnamed.")
    }

    if (package %in% names(package_dir_cache)) {
      package_dir <- package_dir_cache[[package]]
    } else {
      package_dir <- system.file(package = package)
      package_dir_cache[[package]] <<- package_dir
    }

    file.path(package_dir, ...)
  }
})
