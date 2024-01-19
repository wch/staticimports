test_that("is_r_version() works", {
  load_exports()

  assign(".get_r_version", function() package_version("4.2.2"), exports)
  expect_true(is_r_version("4.2.2"))
  expect_true(is_r_version("4.2"))
  expect_true(is_r_version("4"))
  expect_true(is_r_version("3.6.3", "4.2.2"))
  expect_true(is_r_version(max = "4.3"))

  expect_false(is_r_version("3", "4"))
  expect_false(is_r_version(max = "3.6"))
  expect_false(is_r_version("4.2.1"))

  assign(".get_r_version", function() package_version("3.6.3"), exports)
  expect_true(is_r_version("3.6.3"))
  expect_true(is_r_version("3.6"))
  expect_true(is_r_version("3"))

  expect_false(is_r_version("4"))
  expect_false(is_r_version("3.6.4"))
  expect_false(is_r_version(max = "3.5"))
  expect_false(is_r_version("4.2.3"))

  expect_error(is_r_version(min = 4))
  expect_error(is_r_version(max = 4))
  expect_error(is_r_version(min = "abcd"))
})
