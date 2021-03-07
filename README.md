
<!-- README.md is generated from README.Rmd. Please edit that file -->

# staticutils

<!-- badges: start -->
<!-- badges: end -->

**staticutils** provides utility functions that R programmers can use in
their packages or other projects. The “static” part means that the
functions are statically “imported” – that is they copied into the
project as text, instead of being loaded from a package at run time.

The benefits of doing things this way:

-   A package/project that uses staticutils will not take a run-time
    dependency staticutils.
-   Any changes to a function in **staticutils** will not affect a
    project that uses them, until the author decides to re-import them.
-   You can specify the individual functions to import. This is faster
    and lighter-weight than loading a separate package, especially if
    the package has much more functionality than you need.

Instead of copying and pasting utility functions from project to
project, the utility functions can be centralized in **staticutils**,
where they can be vetted and tested.

The functions in **staticutils** are designed to be:

-   Fast
-   Simple
-   Have no external dependencies

If your project imports a function from **staticutils**, and that
function changes in a way that has a negative impact on your project,
you can simply stop importing it from **staticutils**, and copy the old
version to a separate file.

## Installation

You can install the development version of staticutils with:

``` r
remotes::install_github("wch/staticutils")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(staticutils)

staticutils::import()
```

## Some conventions

### Don’t use `force()`

Two reasons:

-   Error reporting
-   Missing args

``` r
with_option <- function(x, y) {
  force(x)
}

with_option(a = 123)
#> Error in with_option(a = 123): unused argument (a = 123)

with_option2 <- function(x, y) {
  x
}

with_option2(a = 123)
#> Error in with_option2(a = 123): unused argument (a = 123)

err <- function() { stop("asdf") } 

f()
#> Error in f(): could not find function "f"
```

### Don’t use `match.arg()`

-   `match.arg()` is slow, at least if values are inferred
    automatically.