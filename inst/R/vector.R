last <- function(x) {
  x[length(x)]
}

# Append an element to a vector. This can be convenient, but it is also slower
# than directly assigning past the end of a vector, because is creates a copy
# of the object.
`%append%`<- function(x, y) {
  eval(
    substitute( x[[length(x) + 1]] <- y ),
    parent.frame()
  )
}


# x <- 1:5
# y <- 11:13
# idx <- 4

# x_len <- 5
# y_len <- 3
# x_after <- 4  # x_len - y_len + 1L

# Splice two vectors together.
splice <- function(x, y, idx = length(x) + 1L) {
  x_len <- length(x)

  if (idx == x_len + 1L) {
    # Common case: appending y to x
    x[x_len + seq_along(y)] <- y

  } else {
    y_len <- length(y)
    x_after_len <- x_len - idx + 1L

    x[idx + y_len + seq_len(x_after_len) - 1L] <- x[idx + seq_len(x_after_len) - 1L]
    x[idx + seq_len(y_len) - 1L] <- y
  }

  x
}

# Splice two vectors together.
splice2 <- function(x, y, idx = length(x) + 1L) {
  x_len <- length(x)

  if (idx == x_len + 1L) {
    # Common case: appending y to x
    eval(
      substitute( x[length(x) + seq_along(y)] <- y ),
      parent.frame()
    )

  } else {

    y_len <- length(y)
    x_after_len <- x_len - idx + 1L

    x[idx + y_len + seq_len(x_after_len) - 1L] <- x[idx + seq_len(x_after_len) - 1L]
    x[idx + seq_len(y_len) - 1L] <- y
  }

}




`%splice%` <- function(x, y) {
  eval(
    substitute( x[length(x) + seq_along(y)] <- y ),
    parent.frame()
  )
}

insert <- function(x, y, idx) {


}
