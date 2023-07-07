
test_that("is_installed(version=) is a character", {
  load_exports()

  expect_error(
    is_installed("staticimports", 1),
    "`version` must be a character string"
  )
  expect_error(
    is_installed("staticimports", "0.0.0.1"),
    NA
  )
})
