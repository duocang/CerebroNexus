test_that("workbook sheet inventory runs outside the Shiny process", {
  source <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "enhancements.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "callr::r_bg(", fixed = TRUE)
  expect_match(source, "process$is_alive()", fixed = TRUE)
  expect_match(source, "process$get_result()", fixed = TRUE)
  expect_match(source, "builder_table_inventory_metadata", fixed = TRUE)
  expect_match(source, "supervise = TRUE", fixed = TRUE)
})
