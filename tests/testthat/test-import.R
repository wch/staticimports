
test_that("staticimports declaration parsing", {
  outdir <- tempfile("staticimports-test")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE))

  writeLines(
"# @staticimports pkg:staticimports
#   is_string cat0

# @staticimports pkg:staticimports
#   map_chr walk
", file.path(outdir, "file1.R"))

  writeLines(
"
123

# @staticimports pkg:staticimports
#   map_int
#   all_named

# @staticimports pkg:foo
#   a b
", file.path(outdir, "file2.R"))

  res <- find_staticimports(outdir)

  # Clean up path component so it doesn't hard code absolute path
  res <- lapply(res, function(x) {
    x$path <- sub(".*/(\\S+)", "\\1", x$path, perl = TRUE)
    x
  })

  expect_identical(
    res,
    list(
      list(
        label = "pkg:foo",
        path = "",
        names = c("a", "b")
      ),
      list(
        label = "pkg:staticimports",
        path = "staticexports",
        names = c("all_named", "cat0", "is_string", "map_chr", "map_int", "walk")
      )
    )
  )
})
