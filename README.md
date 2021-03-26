
<!-- README.md is generated from README.Rmd. Please edit that file -->

# staticimports

<!-- badges: start -->

[![R-CMD-check](https://github.com/wch/staticimports/workflows/R-CMD-check/badge.svg)](https://github.com/wch/staticimports/actions)
<!-- badges: end -->

staticimports makes it easy to statically import functions into R
projects or packages. The “static” importing means that the functions
are copied into the project as text, instead of being loaded from a
separate package at run time.

The benefits of doing things this way:

-   A project or package that uses staticimports will not take a
    run-time dependency on the staticimports package.
-   Statically imported functions will not change underneath you
    unexpectedly. They only will change when you re-import them, and
    then you can inspect the diff for changes that may cause problems.
-   You can specify the individual functions to import. This is faster
    and lighter-weight than loading a separate package, especially if
    the package has much more functionality than you need.
-   There is zero commitment: After you use staticimports once, you
    never have to use it again. You can even move the imported functions
    out to other files and treat them like any other functions in your
    project.

Where can statically imported functions come from?

-   The staticimports package itself contains many utility functions
    that can be imported into a project.
-   You can a set of utility functions on in a directory on your
    computer. For example, you may maintain your own set of functions
    that you can statically import into a project.

Instead of copying and pasting utility functions from project to
project, the utility functions can be centralized in a place where they
can be vetted and tested.

The functions in staticimports are designed to be:

-   Fast
-   Simple
-   Have no external dependencies

If your project imports a function from staticimports, and that function
changes in a way that has a negative impact on your project, you can
simply stop importing it from staticimports, and copy the old version to
a separate file.

## Installation

You can install the development version of staticimports with:

``` r
remotes::install_github("wch/staticimports")
```

## Usage

To use, put a comment block starting with `# @staticimports` in one of
your R source files. For example, your `utils.R` may have this at the
top:

    # @staticimports pkg:staticimports
    #  os_name %||%
    #  map walk

The `pkg:staticimports` tells it to import from staticimports package.
To import from a different package, use `pkg:mypackage`. It looks in a
directory of the package named
[`staticexports`](https://github.com/wch/staticimports/tree/main/inst/staticexports)
to find the objects.

The following lines name the objects to import from the source. In this
case, they are `os_name`, `%||%`, `map`, and `walk`.

To perform the import, run:

``` r
library(staticimports)
import()
```

By default this will write the functions to a file `R/staticimports.R`
in your project.

You examine the output by writing to `stdout()` instead of
`R/staticimports.R`. Notice how importing `os_name` automatically brings
in `is_windows`, `is_mac`, and `is_linux`.

``` r
import(outfile = stdout())
```

    #> # Generated by staticimports; do not edit by hand.
    #> 
    #> `%||%` <- function(a, b) {
    #>   if (is.null(a)) b else a
    #> }
    #> 
    #> is_linux <- function() Sys.info()[['sysname']] == 'Linux'
    #> 
    #> is_mac <- function() Sys.info()[['sysname']] == 'Darwin'
    #> 
    #> is_windows <- function() .Platform$OS.type == "windows"
    #> 
    #> map <- function(.x, .f, ...) {
    #>   lapply(.x, .f, ...)
    #> }
    #> 
    #> os_name <- function() {
    #>   if (is_windows()) {
    #>     "win"
    #>   } else if (is_mac()) {
    #>     "mac"
    #>   } else if (is_linux()) {
    #>     "linux"
    #>   } else if (.Platform$OS.type == "unix") {
    #>     "unix"
    #>   } else {
    #>     "unknown"
    #>   }
    #> }
    #> 
    #> walk <- function(.x, .f, ...) {
    #>   for (i in seq_along(.x)) {
    #>     .f(.x[[i]], ...)
    #>   }
    #>   NULL
    #> }

For testing what the output will look like, you can use `import_objs()`
to see what it looks like when you import specific objects by name:

``` r
import_objs(c("map", "walk"), outfile = stdout())
#> # Generated by staticimports; do not edit by hand.
#> 
#> map <- function(.x, .f, ...) {
#>   lapply(.x, .f, ...)
#> }
#> 
#> walk <- function(.x, .f, ...) {
#>   for (i in seq_along(.x)) {
#>     .f(.x[[i]], ...)
#>   }
#>   NULL
#> }
```

## Under the hood

The functions provided by staticimports are in the
[inst/staticexports](https://github.com/wch/staticimports/tree/main/inst/staticexports)
directory of the repository. When `import()` is called, it sources all
of those files into a new environment. Then it finds all the internal
dependencies among those functions. If a function `fn_a` is requested,
and it uses a function `fn_b`, then both of those functions will be
copied to the project.

If a different set of source files is used, it sources all of the files
in the target directory into a new environment and then proceeds the
same way.

For functions, it will use source refs to copy the original text. For
non-functions (say, a list with some precomputed values), it will call
`deparse()` on the object and write that to the file. Note that
deparsing will not work correctly for all objects.

## TODO

-   Figure out and explain licensing issues.
-   Write license info in generated file.
