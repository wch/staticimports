test_that("staticexports source parsing", {
  outdir <- tempfile("staticimports-test")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE))

  writeLines(
"# Comment attached to f
f <- function() 123

# Comment separated from g

g <- local(
  function() {
    x <- 123
    x
  }
)

# Comment separated from x

# Comment attached to x
# And another line
x <- list(
  # Comment within the definition

  1,
  2,
  3
)
", file.path(outdir, "file1.R"))

  writeLines(
    "`%infix%` <- function(lhs, rhs) lhs",
    file.path(outdir, "file2.R")
  )

  res <- process_source_texts(lapply(dir(outdir, full.names = TRUE), readLines))

  expect_equal(names(res), c("f", "g", "x", "%infix%"))

  expect_identical(
    res,
    list(
      f = c(
        "# Comment attached to f",
        "f <- function() 123"
      ),
      g = c(
        "g <- local(",
        "  function() {",
        "    x <- 123",
        "    x",
        "  }",
        ")"
      ),
      x = c(
        "# Comment attached to x",
        "# And another line",
        "x <- list(",
        "  # Comment within the definition",
        "",
        "  1,",
        "  2,",
        "  3",
        ")"
      ),
      `%infix%` = "`%infix%` <- function(lhs, rhs) lhs"
    )
  )
})

test_that("staticexports source parsing with uncommon assignment operators", {
  # In R < 3.6, parse data does not properly keep track of assignments using `=`
  skip_if(getRversion() < 3.6)

  outdir <- tempfile("staticimports-test")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE))

writeLines(
"# other assigns

# equals assign
h = function(x, y) {
  x == y
}

# right assign
list(
  3,
  2,
  1
) -> r", file.path(outdir, "file3.R"))

  res <- process_source_texts(lapply(dir(outdir, full.names = TRUE), readLines))

  expect_equal(names(res), c("h", "r"))

  expect_identical(
    res,
    list(
      h = c(
        "# equals assign",
        "h = function(x, y) {",
        "  x == y",
        "}"
      ),
      r = c(
        "# right assign",
        "list(",
        "  3,",
        "  2,",
        "  1",
        ") -> r"
      )
    )
  )
})
