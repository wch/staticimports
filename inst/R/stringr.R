str_replace <- function(text, pattern, replacement){
  sub(pattern = pattern, replacement = "", x = text, perl = TRUE)
}

str_replace_all <- function(text, pattern, replacement){
  gsub(pattern = pattern, replacement = "", x = text, perl = TRUE)
}

str_remove_all <- function(text, pattern){
  str_replace_all(text, pattern = pattern, replacement = "")
}

trim_leading <- function(text){
  str_remove_all(text, pattern = "^\\s+")
}

trim_trailing <- function(text){
  str_remove_all(text, pattern = "\\s+$")
}

str_trim <- function(text){
  trim_trailing(trim_leading(text))
}

str_detect <- function(text, pattern){
  grepl(pattern = pattern, x = text, perl = TRUE)
}

str_extract <- function(text, pattern){
  regmatches(
    x = text,
    m = regexpr(
      pattern = pattern,
      text = text,
      perl = TRUE
    )
  )
}
