# Set the class of x. Faster than `structure(x, class = class)`, which is
# surprisingly slow.
set_class <- function(x, class) {
  class(x) <- class
  x
}
