test_that("roclet", {
  dir <- file.path(tempdir(), "testpkg")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  writeLines(
    c(
      'Package: testpkg',
      'Title: Test Package',
      'Version: 0.0.1',
      'Encoding: UTF-8',
      'Roxygen: list(',
      '    roclets = c("namespace", "rd", "staticimports::staticexport_roclet"))'
    ),
    file.path(dir, "DESCRIPTION")
  )

  dir.create(file.path(dir, "R"))

  writeLines(
    c(
      "#' @staticexport",
      "f <- function() 123"
    ),
    file.path(dir, "R", "f.R")
  )

  # `@export` and `@importFrom` tags should be stripped
  writeLines(
    c(
      "#' g function",
      "#' @export",
      "#' @importFrom foo bar",
      "#' @staticexport g_suffix",
      "g <- function() 123"
    ),
    file.path(dir, "R", "g.R")
  )

  # Shouldn't be copied (no `@staticexport`)
  writeLines(
    c(
      "#' h function",
      "#' @param ... Dots",
      "h <- function(...) 123"
    ),
    file.path(dir, "R", "h.R")
  )

  # Shouldn't be copied (not a roxygen comment)
  writeLines(
    c(
      "# @staticexport",
      "i <- function(...) 123"
    ),
    file.path(dir, "R", "i.R")
  )

  roxygen2::roxygenize(dir)

  expect_true(dir.exists(file.path(dir, "inst", "staticexports")))
  expect_equal(
    dir(file.path(dir, "inst", "staticexports")),
    c("f.R", "g_suffix.R")
  )

  expect_snapshot_value(
    readLines(file.path(dir, "inst", "staticexports", "f.R")),
    style = "deparse"
  )
  expect_snapshot_value(
    readLines(file.path(dir, "inst", "staticexports", "g_suffix.R")),
    style = "deparse"
  )
})
