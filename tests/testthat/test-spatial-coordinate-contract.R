test_that("coordinate columns are found whatever the case of their names", {
  upper <- data.frame(ID = 1:2, X = c(3, 4), Y = c(5, 6))
  found <- .spx_find_coordinate_columns(upper)
  expect_identical(found$x, "X")
  expect_identical(found$y, "Y")

  mixed <- data.frame(
    Cell = c("a", "b"),
    Center_X = c(1, 2),
    Center_Y = c(3, 4)
  )
  found <- .spx_find_coordinate_columns(mixed)
  expect_identical(found$x, "Center_X")
  expect_identical(found$y, "Center_Y")
})
