
test_that("is_installed(version=) is a character", {
  load_exports()

  expect_warning(
    is_installed("staticimports", 1),
    "`version` must be a character string"
  )
  expect_warning(
    is_installed("staticimports", "0.0.0.1"),
    NA
  )
  expect_warning(
    is_installed("staticimports", package_version("1.2.3")),
    NA
  )
  expect_warning(
    is_installed("staticimports", numeric_version("1.2.3")),
    NA
  )
})
