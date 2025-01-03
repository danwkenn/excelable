#' Constructor for a Field.
#' 
#' The point of a field is that items have a name and fields. There might need to be other types of field as well.
#' @param content Takes the actual content, which is either a formula or data.
#' @param formatting Takes a list of formatting elements to be used for formatting.
#' @param row_names Names of the rows for references.
#' @param column_names Names of the columns for references.
new_Field <- function(
  content,
  formatting,
  row_names,
  column_names
) {
  structure(
    list(
      content = content,
      formatting = formatting,
      row_names = row_names,
      column_names = column_names
    ),
    class = "Field"
  )
}

#' Constructor for the most basic item, a cell.
#' @param name Name for the item.
#' @param fields A named list of the fields.
new_Cell <- function(
  name,
  fields
) {
  structure(
    list(
      name = name,
      fields = fields
    ),
    class = "Cell"
  )
}

#' A Cell item object.
#' @param name Name of the item.
#' @param formatting list of formatting elements.
#' @export
Cell <- function(
  name,
  formatting) {
  new_Cell(
    name = name,
    fields = list(
      body = new_Field(
      content = NULL,
      formatting = formatting,
      row_names = NULL,
      column_names = NULL
      )
    )
  )
}

#' Constructor for the formatting element.
#' @param formula Formula for conditional formatting.
#' @param actions List of formatting actions.
new_Formatting <- function(
  formula,
  actions) {

  structure(
    list(
      formula = formula,
      actions = actions
    ),
    class = "Formatting"
  )
}

#' Constructor for the formatting element.
#'
#' Not currently implemented.
#' @inheritParams new_Formatting
Formatting <- function(
  actions,
  formula = NULL) {
  new_Formatting(
    formula,
    actions
  )
}

#' Method for adding formulas.
#'
#' Formulas need to be added after the item is created, since the formula syntax allows for ommissions which assume reference to the object itself.
#' @param item Item to add the formula to.
#' @param content String for the formula
#' @param field_name Name of the field to add the formula to (may be ignored).
#' @param location Used for recurssion, but might be ignored.
#' @export
add_formula <- function(
  item,
  content,
  field_name,
  location = NULL) {
  UseMethod("add_formula")
}

#' Method for adding formulas to cells.
#'
#' Formulas need to be added after the item is created, since the formula syntax allows for ommissions which assume reference to the object itself.
#' @param item Item to add the formula to.
#' @param content String for the formula
#' @param field_name Name of the field to add the formula to (may be ignored).
#' @param location Used for recurssion, but might be ignored.
#' @export
add_formula.Cell <- function(item, content, field_name, location = NULL) {
  location <- new_Coordinate(
    item$name,
    field = "body",
    1,
    1,
    1,
    1
  )
  item$field$body$content <- Formula(
    content,
    location
  )
  item
}

function() {
  devtools::load_all()
  cell <- Cell(
    name = "test",
    formatting = list(
      Formatting(
      formula = NULL,
      actions = NULL
    )
    )
  )
  cell <- add_formula(cell, "= {table1};{body};{col1};{row3} + SUM({table2};{col1})")

}