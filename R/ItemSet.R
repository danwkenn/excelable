#' Constructor for the object which holds items.
#' @param items A list of items.
new_ItemSet <- function(
  items) {
  structure(
    list(
      items = items
    ),
    class = "ItemSet"
  )
}

#' Create a list of items.
#' @param ... items
#' @export
ItemSet <- function(
  ...) {
  structure(
    list(
      items = list(...)
    ),
    class = "ItemSet"
  )
}

function() {
  devtools::document()
  devtools::load_all()
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
  items <- list(cell1, cell2)
}