str_replace <- function(text, pattern, replacement, fixed = FALSE, perl = !fixed, ...) {
  sub(pattern = pattern, replacement = replacement, x = text, fixed = fixed, perl = perl, ...)
}

str_replace_all <- function(text, pattern, replacement, fixed = FALSE, perl = !fixed, ...) {
  gsub(pattern = pattern, replacement = replacement, x = text, fixed = fixed, perl = perl, ...)
}

str_remove <- function(text, pattern, ...) {
  str_replace(text, pattern = pattern, replacement = "", ...)  
}

str_remove_all <- function(text, pattern, ...) {
  str_replace_all(text, pattern = pattern, replacement = "", ...)
}

trim_leading <- function(text) {
  str_remove_all(text, pattern = "^\\s+")
}

trim_trailing <- function(text) {
  str_remove_all(text, pattern = "\\s+$")
}

str_trim <- function(text, side = "both") {
  if (side == "both" || side == "left") {
    text <- trim_leading(text)
  }
  if (side == "both" || side == "right") {
    text <- trim_trailing(text)
  }
  text
}

str_detect <- function(text, pattern, fixed = FALSE, perl = !fixed, ...) {
  grepl(pattern = pattern, x = text, perl = perl, fixed = fixed, ...)
}

str_extract <- function(text, pattern) {
  regmatches(
    x = text,
    m = regexpr(
      pattern = pattern,
      text = text,
      perl = TRUE
    )
  )
}
