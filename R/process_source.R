# Given a list of char vectors (each should be the output of `readLines()`),
# process the text and return a named list of objects.
process_source_texts <- function(source_texts) {
  results <- lapply(source_texts, process_source_text_one)
  unlist(unname(results), recursive = FALSE)
}

# Given a char vector of lines (from readLines()), process the text and return a
# named list of objects.
process_source_text_one <- function(text) {
  parse_data <- utils::getParseData(
    parse(text = paste(text, collapse = "\n"), keep.source = TRUE)
  )

  assignment_ops <- parse_data[
    parse_data$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"),
  ]

  object_definitions_idx <- which(
    # Object definitions are the parent expressions of assignment operators
    parse_data$id %in% assignment_ops$parent &
      # Top-level expressions have a parent attribute of 0
      parse_data$parent == 0,
  )

  object_definition_ids <- parse_data[object_definitions_idx, "id"]

  result <- list()

  for (i in seq_along(object_definition_ids)) {
    id <- object_definition_ids[[i]]

    start_line <- parse_data[parse_data$id == id, "line1"]
    end_line <- parse_data[parse_data$id == id, "line2"]

    # A comment's parent attribute is 0 - the next expression's id
    comments <- parse_data[parse_data$parent == -id, ]
    # Include comments if there are no empty lines between the comment
    # and the object definition
    leading_comments_idx <- which(
      start_line - comments$line1 <= rev(seq_len(nrow(comments)))
    )
    comment_lines <- comments[leading_comments_idx, "line1"]

    staticexport_lines_idx <- seq(min(comment_lines, start_line), end_line)
    result[[i]] <- text[staticexport_lines_idx]

    names(result)[[i]] <- extract_object_name(parse_data, id, assignment_ops)
  }

  result
}

extract_object_name <- function(parse_data, definition_id, assignment_ops) {
  assignment_idx <- which(assignment_ops$parent == definition_id)
  assignment_line <- assignment_ops[assignment_idx, "line1"]
  assignment_col <- assignment_ops[assignment_idx, "col1"]

  object_name_idx <- which(
    parse_data$parent == definition_id &
      parse_data$line2 <= assignment_line &
      parse_data$col2 < assignment_col
  )

  object_name_id <- parse_data[object_name_idx, "id"]

  result <- utils::getParseText(parse_data, object_name_id)

  # Remove backticks, so that strings like "`%||%`" are converted to "%||%"
  if (grepl("^`\\S+`$", result)) {
    result <- sub("^`(\\S+)`$", "\\1", result)
  }

  result
}

