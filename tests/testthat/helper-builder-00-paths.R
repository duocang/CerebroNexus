builder_test_description_path <- function() {
  path <- testthat::test_path("..", "..", "DESCRIPTION")
  if (!file.exists(path)) {
    path <- system.file(
      "DESCRIPTION",
      package = "CerebroNexus",
      mustWork = TRUE
    )
  }
  path
}
