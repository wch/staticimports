# Simplified version rlang:::s3_register() that just uses
# warning() instead of rlang::warn() when registration fails
# https://github.com/r-lib/rlang/blob/main/R/compat-s3-register.R
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  register <- function(...) {
    envir <- asNamespace(package)

    # Refresh the method each time, it might have been updated by
    # `devtools::load_all()`
    method_fn <- get_method(method)
    stopifnot(is.function(method_fn))

    # Only register if generic can be accessed
    if (exists(generic, envir)) {
      registerS3method(generic, class, method_fn, envir = envir)
    } else {
      warning(
        "Can't find generic `", generic, "` in package ", package,
        " register S3 method. Do you need to update ", package,
        " to the latest version?", call. = FALSE
      )
    }
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...) {
    register()
  })

  # Avoid registration failures during loading (pkgload or regular).
  # Check that environment is locked because the registering package
  # might be a dependency of the package that exports the generic. In
  # that case, the exports (and the generic) might not be populated
  # yet (#1225).
  if (isNamespaceLoaded(package) && environmentIsLocked(asNamespace(package))) {
    register()
  }

  invisible()
}


register_upgrade_message <- function(pkg, version, error = FALSE) {

  msg <- sprintf(
    "This version of '%s' is designed to work with '%s' >= %s.
    Please upgrade via install.packages('%s').",
    environmentName(environment(register_upgrade_message)),
    pkg, version, pkg
  )

  cond <- if (error) stop else packageStartupMessage

  if (pkg %in% loadedNamespaces() && !is_installed(pkg, version)) {
    cond(msg)
  }

  # Always register hook in case pkg is loaded at some
  # point the future (or, potentially, but less commonly,
  # unloaded & reloaded)
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      if (!is_installed(pkg, version)) cond(msg)
    }
  )
}
