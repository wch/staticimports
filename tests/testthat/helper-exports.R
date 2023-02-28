exports <- new.env(parent = globalenv())

local({
  export_files <- dir(
    system.file("staticexports", package = "staticimports"),
    pattern = "\\.[r|R]$",
    full.names = TRUE
  )
  for (export_file in export_files) {
    source(export_file, local = exports, keep.source = TRUE)
  }
})


# Load exports objects into specified environment.
load_exports <- function(env = parent.frame()) {
  invisible(list2env(as.list(exports), envir = env))
}
