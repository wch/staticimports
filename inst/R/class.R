# Add a class to x. Faster than `structure(x, class = class)`, which is
# surprisingly slow.
add_class <- function(x, class) {
  class(x) <- class
  x
}
