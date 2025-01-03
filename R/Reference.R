#' Constructor for a basic location.
#' @param item The name of the item the location is in.
#' @param field The name of the field the location is in.
#' @param column_start the column index (can be either character or numeric).
#' @param row_start the row index (can be either character or numeric).
#' @param column_end the column index (can be either character or numeric).
#' @param row_end the row index (can be either character or numeric).
new_Coordinate <- function(
  item,
  field,
  column_start,
  column_end,
  row_start,
  row_end) {
  structure(
    list(
      item = item,
      field = field,
      column_start = column_start,
      column_end = column_end,
      row_start = row_start,
      row_end = row_end
      ),
    class = "Coordinate"
  )
}

#' Constructor for referencing other items in a formula.
#' @param item The name of the item being referenced.
#' @param field The name of the field within the item.
#' @param column_start The initial column index (can be either character or numeric).
#' @param column_end The final column index (can be either character or numeric).
#' @param row_start The initial row index (can be either character or numeric).
#' @param row_end The final row index (can be either character or numeric).
#' @param column_start_fixed Is the initial column index fixed?
#' @param column_end_fixed Is the final column index fixed?
#' @param row_start_fixed Is the initial row index fixed?
#' @param row_end_fixed Is the final row index fixed?
new_Reference <- function(
  item,
  field,
  column_start,
  column_end,
  row_start,
  row_end,
  column_start_fixed,
  column_end_fixed,
  row_start_fixed,
  row_end_fixed
) {
  structure(
    list(
      item = item,
      field = field,
      column_start = column_start,
      column_end = column_end,
      row_start = row_start,
      row_end = row_end,
      column_start_fixed = column_start_fixed,
      column_end_fixed = column_end_fixed,
      row_start_fixed = row_start_fixed,
      row_end_fixed = row_end_fixed
      ),
    class = "Reference"
  )
}

#' Constructor for defining types of references.
#' @param pattern Regular expression with extraction points.
#' @param extractions character vector indicating the reference element being extracted.
#' @param implicits named character vector indicating the implicits reference element values.
#' @param equivalences named character vector indicating where one element equals another, previously calculated element.
new_ReferenceSchema <- function(
  pattern,
  extractions,
  implicits,
  equivalences
) {
  structure(
    list(
      pattern = pattern,
      extractions = extractions,
      implicits = implicits,
      equivalences = equivalences
      ),
    class = "ReferenceSchema"
  )
}

#' Method to cast objects to a Reference.
#' @param x An object
#' @param location Location the reference is being made from.
#' @param ... Other parameter.
Reference <- function(x, location = NULL, ...) {
  UseMethod("Reference")
}

#' Cast a list to Reference
#' @inheritParams Reference
Reference.list <- function(x, location = NULL, ...) {
  new_Reference(
  item = x$item,
  field = x$field,
  column_start = x$column_start,
  column_end = x$column_end,
  row_start = x$row_start,
  row_end = x$row_end,
  column_start_fixed = x$column_start_fixed,
  column_end_fixed = x$column_end_fixed,
  row_start_fixed = x$row_start_fixed,
  row_end_fixed = x$row_end_fixed
  )
}

#' Internal function for converting a character reference into a proper
#' reference object using a ReferenceSchema
#' @param x A character string.
#' @param reference_schema A ReferenceSchema
#' @param location Coordinate object indicating where the reference is.
apply_reference_schema_to_char <- function(
  x,
  reference_schema,
  location = NULL) {

  extract_pattern <- paste0("^", reference_schema$pattern, "$")

  l <- list()
  for (i in seq_along(reference_schema$extractions)) {
    matches <- regexec(extract_pattern, x, perl = TRUE)
    groups <- regmatches(x, matches)
    l[[reference_schema$extractions[[i]]]] <- groups[[1]][[i + 1]]
  }

  for (i in seq_along(reference_schema$implicits)) {
    l[[names(reference_schema$implicits)[[i]]]] <- reference_schema$implicits[[i]]
  }

  for (i in seq_along(reference_schema$equivalences)) {
    l[[names(reference_schema$equivalences)[[i]]]] <- l[[reference_schema$equivalences[[i]]]]
  }

  fixed_args <- c(
    "column_start_fixed",
    "column_end_fixed",
    "row_start_fixed",
    "row_end_fixed"
  )

  for (i in fixed_args) {
    l[[i]] <- l[[i]] == "$"
  }

  if (is.na(l[["item"]])) {
    l[["item"]] <- location$item
  }

  if (is.na(l[["field"]])) {
    l[["field"]] <- location$field
  }

  SAME_LOCATION <- location$item == l$item && location$field == l$field
  if (is.na(l[["row_start"]]) && SAME_LOCATION) {
    l[["row_start"]] <- location$row_start
  }
  if (is.na(l[["row_end"]]) && SAME_LOCATION) {
    l[["row_end"]] <- location$row_end
  }
  if (is.na(l[["column_start"]]) && SAME_LOCATION) {
    l[["column_start"]] <- location$column_start
  }
  if (is.na(l[["column_end"]]) && SAME_LOCATION) {
    l[["column_end"]] <- location$column_end
  }

  if (is.na(l[["column_start"]])) {
    l[["column_start"]] <- 1
  }

  if (is.na(l[["row_start"]])) {
    l[["row_start"]] <- 1
  }

  if (is.na(l[["column_end"]])) {
    l[["column_end"]] <- Inf
  }

  if (is.na(l[["row_end"]])) {
    l[["row_end"]] <- Inf
  }

  Reference(l)
}

#' Internal function for converting a character reference into a proper
#' reference object using a ReferenceSchema
#' @param x A character string.
#' @param reference_schema A ReferenceSchema
check_char_against_reference_schema <- function(
  x,
  reference_schema) {

  match_pattern <- paste0("^", reference_schema$pattern, "$")

  grepl(match_pattern, x, perl = TRUE)
}

name_grep <- "[a-z,A-Z](?:\\w|_|\\.)*"
name_or_int_grep <- "[a-z,A-Z](?:\\w|_|\\.)*|\\d+"

reference_schema_list <- list(
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};\\{(?:F-|)(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}\\:(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\};(|\\$)\\{(?:R-|)(<name_or_int_grep>)\\}\\:(|\\$)\\{(?:R-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item", "field",
      "column_start_fixed", "column_start",
      "column_end_fixed", "column_end",
      "row_start_fixed", "row_start",
      "row_end_fixed", "row_end"
      ),
    implicits = c(),
    equivalences = c()),
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};\\{(?:F-|)(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\};(|\\$)\\{(?:R-|)(<name_or_int_grep>)\\}\\:(|\\$)\\{(?:R-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item", "field",
      "column_start_fixed", "column_start",
      "row_start_fixed", "row_start",
      "row_end_fixed", "row_end"
      ),
    implicits = c(),
    equivalences = c(
      column_end_fixed = "column_start_fixed",
      column_end = "column_start"
    )),
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};\\{(?:F-|)(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}\\:(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\};(|\\$)\\{(?:R-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item", "field",
      "column_start_fixed", "column_start",
      "column_end_fixed", "column_end",
      "row_start_fixed", "row_start"
      ),
    implicits = c(),
    equivalences = c(
      row_end_fixed = "row_start_fixed",
      row_end = "row_start"
    )),
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};\\{(?:F-|)(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\};(|\\$)\\{(?:R-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item", "field",
      "column_start_fixed", "column_start",
      "row_start_fixed", "row_start"
      ),
    implicits = c(),
    equivalences = c(
      "column_end_fixed" = "column_start_fixed",
      "column_end" = "column_start",
      row_end_fixed = "row_start_fixed",
      row_end = "row_start"
    )),
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};\\{(?:F-|)(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}\\:(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item", "field",
      "column_start_fixed", "column_start",
      "column_end_fixed", "column_end"
      ),
    implicits = c(
      row_start_fixed = FALSE,
      row_start = NA_character_,
      row_end_fixed = FALSE,
      row_end = NA_character_),
    equivalences = c(
    )),
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};\\{(?:F-|)(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item", "field",
      "column_start_fixed", "column_start"
      ),
    implicits = c(
      row_start_fixed = FALSE,
      row_start = NA_character_,
      row_end_fixed = FALSE,
      row_end = NA_character_),
    equivalences = c(
      "column_end_fixed" = "column_start_fixed",
      "column_end" = "column_start"
    )),
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};\\{F-(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item", "field",
      "column_start_fixed", "column_start"
      ),
    implicits = c(
      row_start_fixed = FALSE,
      row_start = NA_character_,
      row_end_fixed = FALSE,
      row_end = NA_character_),
    equivalences = c(
      "column_end_fixed" = "column_start_fixed",
      "column_end" = "column_start"
    )),
  new_ReferenceSchema(
    pattern = glue::glue("\\{F-(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "field",
      "column_start_fixed", "column_start"
      ),
    implicits = c(
      item = NA_character_,
      row_start_fixed = FALSE,
      row_start = NA_character_,
      row_end_fixed = FALSE,
      row_end = NA_character_),
    equivalences = c(
      "column_end_fixed" = "column_start_fixed",
      "column_end" = "column_start"
    )),
  new_ReferenceSchema(
    pattern = glue::glue("\\{(?:I-|)(<name_grep>)\\};(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "item",
      "column_start_fixed", "column_start"
      ),
    implicits = c(
      field = NA_character_,
      row_start_fixed = FALSE,
      row_start = NA_character_,
      row_end_fixed = FALSE,
      row_end = NA_character_),
    equivalences = c(
      "column_end_fixed" = "column_start_fixed",
      "column_end" = "column_start"
    )),
  new_ReferenceSchema(
    pattern = glue::glue("(|\\$)\\{(?:C-|)(<name_or_int_grep>)\\}", .open = "<", .close = ">"),
    extractions = c(
      "column_start_fixed", "column_start"
      ),
    implicits = c(
      item = NA_character_,
      field = NA_character_,
      row_start_fixed = FALSE,
      row_start = NA_character_,
      row_end_fixed = FALSE,
      row_end = NA_character_),
    equivalences = c(
      "column_end_fixed" = "column_start_fixed",
      "column_end" = "column_start"
    ))
  )

#' Cast a character to a Reference object
#' @inheritParams Reference
Reference.character <- function(x, location = NULL, ...) {
  for (i in seq_along(reference_schema_list)) {
    # if (i == 6) {browser()}
    MATCH <- check_char_against_reference_schema(
      x,
      reference_schema_list[[i]]
    )
    if (MATCH) {
      return(
        apply_reference_schema_to_char(
          x,
          reference_schema_list[[i]],
          location = location
        )
      )
    }
  }
  stop("Failed to match a ReferenceSchema.")
}


function() {
  devtools::load_all()
  x <- "${col1}"
  location <- new_Coordinate(
    "table2", "body", "col3", "col4", "row1",  "row4"
  )
  blah <- as.character()
  Reference.character(x, location = location)

  grepl(as.character(reference_schema_list[[6]]$pattern), x, perl = TRUE)

  grep("(?<!<<)(|\\$)([a-z,A-Z](?:\\w|_|\\.)*|\\d+)", ref_as_char, perl = TRUE)
  debugonce(apply_reference_schema_to_char)
}