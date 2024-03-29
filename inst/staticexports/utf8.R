raw_to_utf8 <- function(data) {
  res <- rawToChar(data)
  Encoding(res) <- "UTF-8"
  res
}

read_raw <- function(file) {
  readBin(file, "raw", n = file.info(file, extra_cols = FALSE)$size)
}

# Read file as UTF-8
read_utf8 <- function(file) {
  res <- read_raw(file)
  raw_to_utf8(res)
}

# Write text as UTF-8
write_utf8 <- function(text, ...) {
  writeBin(charToRaw(enc2utf8(text)), ...)
}
