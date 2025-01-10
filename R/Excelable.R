#' Constructor for Excelable object.
#' @param item_set An `ItemSet` object.
#' @param layout A layout object with defined methods.
new_Excelable <- function(
  item_set,
  layout
) {
  structure(
    list(
      item_set = item_set,
      layout = layout
    ),
    class = "Excelable"
  )
}

#' Excelable object for creating the excel file with.
#' @inheritParams new_Excelable
#' @export
Excelable <- function(
  item_set,
  layout) {

  new_Excelable(
    item_set = item_set,
    layout = layout
  )
}
