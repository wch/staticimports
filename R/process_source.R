# Given a list of char vectors (each should be the output of `readLines()`),
# process the text and return a named list of objects.
process_source_texts <- function(source_texts) {
  results <- lapply(source_texts, process_source_text_one)
  unlist(unname(results), recursive = FALSE)
}

# Given a char vector of lines (from readLines()), process the text and return a
# named list of objects.
process_source_text_one <- function(text) {
  # There are three states for this state machine:
  # - "SCANNING": This means we're looking for comments or object definitions.
  # - "OBJECT_DEFINITION": We're inside an object definition (usually a
  #     function).
  # - "LEADING_COMMENT": We're in a comment that (probably) precedes an object
  #     definition.
  chunks <- list()
  state <- "SCANNING"
  current_chunk <- vec_accumulator()
  current_chunk_name <- NULL

  for (line in text) {
    if (state == "SCANNING") {
      if (grepl("^\\S+ *<-", line)) {
        # Lines that start with "foo <-" tell us that the object definition
        # starts here.
        state <- "OBJECT_DEFINITION"
        current_chunk$add(line)
        current_chunk_name <- extract_object_name(line)

        # Special case: check if it's a one-liner (one-liners will be parseable;
        # an opening line for multi-line functions/expressions will not.). If
        # so, transition to SCANNING state.
        if (is_parseable(line)) {
          state <- "SCANNING"
          chunks[[current_chunk_name]] <- current_chunk$get()
          current_chunk$reset()
          current_chunk_name <- NULL
        }

      } else if (grepl("^#", line)) {
        state <- "LEADING_COMMENT"
        current_chunk$add(line)
      }

    } else if (state == "OBJECT_DEFINITION") {
      current_chunk$add(line)
      if (grepl("^\\S", line)) {
        # If the line starts with anything _other_ than whitespace, then we know
        # we've reached the end of the object definition.
        state <- "SCANNING"
        chunks[[current_chunk_name]] <- current_chunk$get()
        current_chunk$reset()
        current_chunk_name <- NULL
      }

    } else if (state == "LEADING_COMMENT") {
      if (grepl("^\\S+ *<-", line)) {
        # Lines that start with "foo <-" tell us that the object definition
        # starts here.
        state <- "OBJECT_DEFINITION"
        current_chunk$add(line)
        current_chunk_name <- extract_object_name(line)

        # Special case: check if it's a one-liner (one-liners will be parseable;
        # an opening line for multi-line functions/expressions will not.). If
        # so, transition to SCANNING state.
        if (is_parseable(line)) {
          state <- "SCANNING"
          chunks[[current_chunk_name]] <- current_chunk$get()
          current_chunk$reset()
          current_chunk_name <- NULL
        }

      } else if (grepl("^#", line)) {
        # Stay in current state.
        current_chunk$add(line)

      } else {
        # We've hit the end of a comment block but no definition here. Just
        # discard the block and go back to scanning mode.
        state <- "SCANNING"
        current_chunk$reset()
      }
    }
  }

  chunks
}


# A wrapper object for vectors, which makes it easy to efficiently append items
# to it.
vec_accumulator <- function(init = logical(0)) {
  s <- init
  list(
    add = function(x) {
      s[length(s) + seq_along(x)] <<- x
    },
    get = function() {
      s
    },
    reset = function() {
      s <<- logical(0)
    }
  )
}


is_parseable <- function(txt) {
  parseable <- NULL
  tryCatch({
    parse(text = txt)
    return(TRUE)
  },
    error = function(e) {}
  )
  return(FALSE)
}

# Given text like "foo <- function() {", return "foo". Also handles the case
# where the name is surounded with backticks.
extract_object_name <- function(txt) {
  result <- sub("^(\\S+).*", "\\1", txt)

  # Remove backticks, so that strings like "`%||%`" are converted to "%||%"
  if (grepl("^`\\S+`$", result)) {
    result <- sub("^`(\\S+)`$", "\\1", result)
  }
  result
}
