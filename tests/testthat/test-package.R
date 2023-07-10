test_that("is_installed(version=) is character errors in testing, warns in use", {
  load_exports()

  expect_error(
    is_installed("staticimports", 1),
    "`version` must be a character string"
  )

  expect_silent(
    expect_true(
      is_installed("staticimports", "0.0.0.1")
    )
  )

  is_testing <- Sys.getenv("TESTTHAT")
  Sys.setenv(TESTTHAT = "temporarily false")
  on.exit(Sys.setenv(TESTTHAT = is_testing))

  expect_warning(
    expect_false(
      is_installed("staticimports", 1),
    ),
    "`version` must be a character string"
  )
})

test_that("is_installed(version=) when package/numeric version", {
  load_exports()

  expect_silent(
    expect_false(
      is_installed("staticimports", package_version("100.99.98")),
    )
  )
  expect_silent(
    expect_false(
      is_installed("staticimports", numeric_version("100.99.98")),
    )
  )
})
