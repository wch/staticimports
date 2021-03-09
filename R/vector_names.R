# Given a vector, return TRUE if any elements are named, FALSE otherwise.
# For zero-length vectors, always return FALSE.
any_named <- function(x) {
  if (length(x) == 0) return(FALSE)

  nms <- names(x)

  !is.null(nms) && any(nzchar(nms))
}

# Given a vector, return TRUE if any elements are unnamed, FALSE otherwise.
# For zero-length vectors, always return FALSE.
any_unnamed <- function(x) {
  if (length(x) == 0) return(FALSE)

  nms <- names(x)
  is.null(nms) || !all(nzchar(nms))
}

# Given a vector, return TRUE if all elements are named, FALSE otherwise.
# For zero-length vectors, always return TRUE.
# If an element is named "", it is considered unnamed; return FALSE.
all_named <- function (x) {
  if (length(x) == 0) return(TRUE)
  nms <- names(x)
  !is.null(nms) && all(nzchar(nms))
}

# Given a vector, return TRUE if all elements are unnamed, FALSE otherwise.
# For zero-length vectors, always return TRUE.
# If there is no name attribute for the vector, or if all elements are named "",
# return TRUE.
all_unnamed <- function (x) {
  if (length(x) == 0) return(TRUE)
  nms <- names(x)
  is.null(nms) && !any(nzchar(nms))
}



# Different from the all_named and any_named functions, in that those functions
# report whether each element is named or not. This function reports whether the
# vector itself has a name attribute; it returns TRUE even if some names are "",
# or the vector is length-0.
is_named <- function(x) {
  !is.null(names(x))
}

is_unnamed <- function(x) {
  is.null(names(x))
}


named_list_val <- list(a = 1)[0]
# Returns an empty named list
named_list <- function() named_list_val

named_chr_val <- c(a = "c")[0]
# Returns an empty named character vector
named_chr <- function() named_chr_val

