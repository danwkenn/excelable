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

#' Method for adding an item to an excelable's layout as a terminal node.
#' @param x `Excelable` object
#' @param sheet name of the sheet.
#' @param item name of the item
#' @export
add_terminal_node <- function(
  x,
  sheet,
  item
) {

  sheet_id <- x$layout$sheets$id[which(x$layout$sheets$name == sheet)]
  new_layout_element <- data.frame(
    id = next_id(x$layout$layout),
    sheet = sheet_id,
    parent_id = NA_integer_,
    child_id = NA_integer_,
    type = "item",
    item = item,
    axis = NA_character_,
    row_extent = NA_integer_,
    column_extent = NA_integer_,
    within_parent_row_location = NA_integer_,
    within_parent_col_location = NA_integer_,
    absolute_row_location = NA_integer_,
    absolute_col_location = NA_integer_
  )

  x$layout$layout <- rbind(
    x$layout$layout,
    new_layout_element
  )

  x
}

#' Method for adding a node to an excelable's layout connecting other nodes.
#' @param x `Excelable` object
#' @param sheet name of the sheet.
#' @param id1 ID of the node 1
#' @param id2 ID of the node 2
#' @param axis direction (row or column).
#' @export
add_fork_node <- function(
  x,
  sheet,
  id1,
  id2,
  axis) {

  sheet_id <- x$layout$sheets$id[which(x$layout$sheets$name == sheet)]
  new_id <- next_id(x$layout$layout)
  new_layout_element <- data.frame(
    id = new_id,
    sheet = sheet_id,
    parent_id = NA_integer_,
    child_id = NA_integer_,
    type = "fork",
    item = NA_character_,
    axis = axis,
    row_extent = NA_integer_,
    column_extent = NA_integer_,
    within_parent_row_location = NA_integer_,
    within_parent_col_location = NA_integer_,
    absolute_row_location = NA_integer_,
    absolute_col_location = NA_integer_
  )

  x$layout$layout <- rbind(
    x$layout$layout,
    new_layout_element
  )

  id1_row <- which(x$layout$layout$id == id1)
  id2_row <- which(x$layout$layout$id == id2)
  x$layout$layout$parent_id[id1_row] <- new_id
  x$layout$layout$parent_id[id2_row] <- new_id
  x$layout$layout$within_parent_col_location[id1_row] <- 1
  x$layout$layout$within_parent_row_location[id1_row] <- 1
  if (axis == "row") {
    x$layout$layout$within_parent_row_location[id1_row] <- 1
  }
  if (axis == "column") {
    x$layout$layout$within_parent_col_location[id1_row] <- 1
  }
  x
}

#' Method for Excelable objects.
#' @inheritParams add_sheet
#' @export
add_sheet.Excelable <- function(
  x,
  name,
  label) {

  x$layout <- add_sheet(x$layout, name = name, label = label)

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
    parent_id = NA_integer_,
    child_id = NA_integer_,
    type = "sheet",
    item = NA_character_,
    axis = NA_character_,
    row_extent = NA_integer_,
    column_extent = NA_integer_,
    within_parent_row_location = NA_integer_,
    within_parent_col_location = NA_integer_,
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

 cell1 <- Cell(
    name = "test1",
    formatting = list(
      Formatting(
      formula = NULL,
      actions = NULL
    )
    )
  )
  cell1 <- add_formula(cell1, "= {test2};{body};{1};{1}")
  cell2 <- Cell(
    name = "test2",
    formatting = list(
      Formatting(
      formula = NULL,
      actions = NULL
    )
    )
  )
  cell2 <- add_data(cell2, 1)
  item_set <- ItemSet(
    cell1, cell2
  )
  excelable <- Excelable(
    item_set = item_set,
    layout = create_MondrianSheets()
  )
  excelable <- add_sheet(x = excelable, name = "summary", label = "Summary")
  excelable <- add_terminal_node(x = excelable, sheet = "summary", item = "test1")
  excelable <- add_terminal_node(x = excelable, sheet = "summary", item = "test2")
  excelable <- add_fork_node(
    excelable,
    sheet = "summary",
    id1 = 2,
    id2 = 3,
    axis = "row"
  )
  x <- excelable
  x
}