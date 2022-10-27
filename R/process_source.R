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
    parse_data$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"),
  ]
  # Assignments are the parent expressions of assignment operators
  assignments <- parse_data[parse_data$id %in% assignment_ops$parent, ]
  # Top-level expressions have a parent attribute of 0
  object_definitions <- assignments[assignments$parent == 0, ]

  staticexports <- vector("list", nrow(object_definitions))

  for (i in seq_len(nrow(object_definitions))) {
    object_definition <- object_definitions[i, ]

    names(staticexports)[i] <- extract_object_name(
      parse_data, object_definition, assignment_ops
    )

    # A leading comment's parent attribute is 0 - the next expression's id
    leading_comments <- parse_data[parse_data$parent == -object_definition$id, ]

    # If there is no whitespace between a leading comment and object definition,
    # expand the lines of the object definition to include the comment
    j <- nrow(leading_comments)
    while (j > 0) {
      if (object_definition$line1 - leading_comments$line2[j] > 1) {
        break
      }

      object_definition$line1 <- leading_comments$line1[j]
      j <- j - 1
    }

    staticexport_lines <- seq(object_definition$line1, object_definition$line2)
    staticexport_text <- text[staticexport_lines]
    staticexports[[i]] <- staticexport_text
  }

  staticexports
}

extract_object_name <- function(parse_data, object_definition, assignment_ops) {
  assignment <- assignment_ops[assignment_ops$parent == object_definition$id, ]

  object_name_expr <- parse_data[
    parse_data$parent == object_definition$id &
      if (assignment$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
        parse_data$line2 <= assignment$line1 & parse_data$col2 < assignment$col1
      } else {
        parse_data$line1 >= assignment$line2 & parse_data$col1 > assignment$col2
      },
  ]

  result <- utils::getParseText(parse_data, object_name_expr$id)

  # Remove backticks, so that strings like "`%||%`" are converted to "%||%"
  if (grepl("^`\\S+`$", result)) {
    result <- sub("^`(\\S+)`$", "\\1", result, perl = TRUE)
  }

  result
}

