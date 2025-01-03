test_that("", {
  ref_as_char <- "{col2}"
  location <- new_Coordinate(
    "table2", "body", "col3", "col4", "row1", "row4"
  )
  result <- Reference(ref_as_char, location = location)
  expect_true(result$item == "table2")
  expect_true(result$field == "body")
  expect_true(result$column_start == "col2")
  expect_true(result$column_end == "col2")
  expect_true(result$row_start == "row1")
  expect_true(result$row_end == "row4")
  expect_true(result$column_start_fixed == FALSE)
  expect_true(result$column_end_fixed == FALSE)
  expect_true(result$row_start_fixed == FALSE)
  expect_true(result$row_end_fixed == FALSE)
})
