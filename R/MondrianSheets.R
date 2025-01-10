#' Constructor for layout component `MondrianSheets`
#' @param sheets data.frame of the sheets.
#' @param layout data.frame of the layout of items and nodes.
new_MondrianSheets <- function(
  sheets,
  layout
) {
  structure(
    list(
      sheets = sheets,
      layout = layout
    ),
    class = "MondrianSheets"
  )
}

#' Creator function for `Mondrian Sheets`
#' @export
create_MondrianSheets <- function() {
  sheets <- data.frame(
    id = integer(),
    name = character(),
    label = character()
  )
  layout <- data.frame(
    id = integer(),
    sheet = integer(),
    parent_id = integer(),
    child_id = integer(),
    type = character(),
    item = character(),
    axis = character(),
    row_extent = integer(),
    column_extent = integer(),
    within_parent_row_location = integer(),
    within_parent_col_location = integer(),
    absolute_row_location = integer(),
    absolute_col_location = integer()
  )
  new_MondrianSheets(
    sheets = sheets,
    layout = layout
  )
}

#' Method for adding a sheet to a layout.
#' @param x object
#' @param name name of the sheet.
#' @param label Label of the sheet.
add_sheet <- function(
  x,
  name,
  label
) {
  UseMethod("add_sheet")
}

#' Method for Excelable objects.
#' @inheritParams add_sheet
#' @export
add_sheet.Excelable <- function(
  x,
  name,
  label) {

  x$layout <- add_sheet(x$layout)

  return(x)
}

next_id <- function(x) {
  if (nrow(x) == 0) {
    return(1L)
  }
  return(max(x$id) + 1)
}

#' Method for Excelable objects.
#' @inheritParams add_sheet
#' @export
add_sheet.MondrianSheets <- function(
  x,
  name,
  label) {

  new_id <- next_id(x$sheets)
  new_sheet <- data.frame(
    id = new_id,
    name = name,
    label = label
  )

  new_layout_element <- data.frame(
    id = next_id(x$layout),
    sheet = new_id,
    parent_id = integer(1),
    child_id = integer(1),
    type = "sheet",
    item = character(1),
    axis = character(1),
    row_extent = integer(1),
    column_extent = integer(1),
    within_parent_row_location = integer(1),
    within_parent_col_location = integer(1),
    absolute_row_location = 1L,
    absolute_col_location = 1L
  )

  x$sheets <- rbind(
    x$sheets,
    new_sheet
  )

  x$layout <- rbind(
    x$layout,
    new_layout_element
  )

  return(x)
}

function() {
  devtools::document()
  devtools::load_all()
  layout <- create_MondrianSheets()
  add_sheet(x = layout, name = "summary", label = "Summary")
}