#' Constructor for the Formula class
#' @param statement A string indicating the eventual excel formula.
#' @param references A list of Reference objects which correspond to the statement's placeholders.
new_Formula <- function(
  statement,
  references) {
  structure(
    list(
      statement = statement,
      references = references
    ),
    class = "Formula"
  )
}

#' Create a formula from a character string.
#' @param x Character string.
#' @param location Location the reference is being made from.
Formula <- function(x, location) {

  x <- gsub("\\s", "", x)

  COMPLETE <- FALSE
  references <- list()
  ref_count <- 0
  while (!COMPLETE) {
    COMPLETE <- TRUE
    for (i in seq_along(reference_schema_list)) {
      matches <- gregexpr(reference_schema_list[[i]]$pattern, x, perl = TRUE)[[1]]
      if (matches[[1]] == -1) {
        next
      }
      ref_count <- ref_count + 1
      replacement <- paste0("<<", ref_count, ">>")
      str_reference <- regmatches(x, matches)
      reference <- Reference.character(str_reference, location = location)
      references <- append(
        references,
        list(reference)
      )
      regmatches(x, matches) <- replacement
      COMPLETE <- FALSE
    }
  }
  new_Formula(
    statement = x,
    references = references
  )
}

function() {
  devtools::load_all()
  x <- "= {table1};{body};{col1};{row3} + {col2} + SUM({table2};{col1})"
  location <- new_Coordinate(
    "table2", "body", "col3", "col4", "row1",  "row4"
  )
  Formula(x, location = location)
}