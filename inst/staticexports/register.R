register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case pkg is loaded at some
  # point the future (or, potentially, but less commonly,
  # unloaded & reloaded)
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
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
