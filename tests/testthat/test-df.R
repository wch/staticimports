x <- data.frame(
  a = c(1L, 2L, NA, 3L),
  b = c(0.1, 0.2, NA, 0.3),
  c = c("a", "b", NA, "c"),
  d = c(TRUE, FALSE, NA, TRUE),
  stringsAsFactors = FALSE
)

test_that("df_to_rowlist", {
  load_exports()

  # Another implementation to compare to. This one is much simpler but not as
  # fast on some platforms.
  df_to_rowlist_mapply <- function(x) {
    .mapply(list, unclass(x), NULL)
  }

  expect_identical(df_to_rowlist(x), df_to_rowlist_mapply(x))
})



test_that("rowlist_to_df", {
  load_exports()

  y <- df_to_rowlist(x)
  expect_identical(x, rowlist_to_df(y))

  # Need to be robust for rows that have a different key order.
  y1 <- y
  y1[[4]] <- list(
    c = y1[[4]]$c,
    d = y1[[4]]$d,
    a = y1[[4]]$a,
    b = y1[[4]]$b
  )
  rowlist_to_df(y1)
  expect_identical(x, rowlist_to_df(y1))

  # If a row has a missing key, should error.
  y1 <- y
  y1[[4]]$c <- NULL
  expect_error(rowlist_to_df(y1))

})

