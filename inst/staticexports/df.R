# Convert a data frame to a list of named lists, each element of the outer list
# represents one row. This is the format used by D3, for example.
df_to_rowlist <- function(df) {
  nrows <- nrow(df)
  ncols <- length(df)
  row_idxs <- seq_len(nrows)
  col_idxs <- seq_len(ncols)
  colnames <- names(df)
  df <- as.list(df)

  res <- vector("list", nrows)

  for (i in row_idxs) {
    row <- vector("list", ncols)
    for (j in col_idxs) {
      row[[j]] <- df[[j]][[i]]
    }
    names(row) <- colnames
    res[[i]] <- row
  }

  res
}

# The implementation of `df_to_rowlist` above is the fastest way I've found to
# convert a data frame to a list of named lists (other than `purrr::transpose`,
# which is implemented in C and also marked as superseded). In the benchmarks
# here, it is named `nested_for`: https://rpubs.com/wch/1008771
