# Given a list of files, process the text and return a named list of objects.
process_source_files <- function(files) {
  results <- lapply(files, process_source_file_one)
  unlist(unname(results), recursive = FALSE)
}

# Given a file, process the text and return a named list of objects.
process_source_file_one <- function(file) {
  parse_data <- utils::getParseData(parse(file = file, keep.source = TRUE))
  text <- readLines(file)

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

    # Exclude leading comments if there are any empty lines between the comment
    # and the object definition
    leading_comments <- leading_comments[
      object_definition$line1 - leading_comments$line1 <=
        rev(seq_len(nrow(leading_comments))),
    ]

    staticexport_lines <- seq(
      min(leading_comments$line1, object_definition$line1),
      max(leading_comments$line2, object_definition$line2)
    )
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

