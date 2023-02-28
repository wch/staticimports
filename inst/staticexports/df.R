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


# Convert a list of named lists to a data frame. This is the inverse of
# `df_to_rowlist`.
#
# Each "row" list can contain the keys in any order. The key order from the
# first "row" will be used for the columns in the data frame.
#
# This function assumes that all columns are present in the first "row" of `x`.
# If later "rows" contain more columns, it will throw an error.
#
# Note that it can't correctly handle factors -- they will get converted to
# integers. But that should be OK, because the input data will generally have
# strings instead of factors.
rowlist_to_df <- function(x) {
  nrows <- length(x)
  ncols <- length(x[[1]])
  row_idxs <- seq_len(nrows)
  colnames <- names(x[[1]])

  df_args <- list()

  for (colname in colnames) {
    col <- vector(typeof(x[[1]][[colname]]), nrows)
    for (i in row_idxs) {
      col[[i]] <- x[[i]][[colname]]
    }
    df_args[[colname]] <- col
  }

  names(df_args) <- colnames
  df_args$stringsAsFactors <- FALSE
  do.call(data.frame, df_args)
}
