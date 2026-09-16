builder_path_contract_files <- function() {
  source_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "createShinyApp.R"
  )
  runtime_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "core",
    "bundle_path_contract.R"
  )
  if (!file.exists(runtime_path)) {
    runtime_path <- system.file(
      "builder",
      "core",
      "bundle_path_contract.R",
      package = "CerebroNexus"
    )
  }
  list(source = source_path, runtime = runtime_path)
}

builder_path_contract_definitions <- function(path) {
  expressions <- parse(file = path, keep.source = FALSE)
  definitions <- list()
  for (expression in expressions) {
    if (
      is.call(expression) &&
        identical(expression[[1L]], as.name("<-")) &&
        is.symbol(expression[[2L]]) &&
        is.call(expression[[3L]]) &&
        identical(expression[[3L]][[1L]], as.name("function"))
    ) {
      definitions[[as.character(expression[[2L]])]] <- paste(
        deparse(expression[[3L]], width.cutoff = 500L),
        collapse = "\n"
      )
    }
  }
  definitions
}

test_that("Builder path safety functions match the generated-App contract", {
  paths <- builder_path_contract_files()

  testthat::skip_if_not(
    file.exists(paths$source),
    "R/ source tree not present (installed-package layout)"
  )
  expect_true(file.exists(paths$runtime))
  source <- builder_path_contract_definitions(paths$source)
  runtime <- builder_path_contract_definitions(paths$runtime)
  expect_gt(length(runtime), 0L)
  expect_true(all(names(runtime) %in% names(source)))
  expect_identical(source[names(runtime)], runtime)
})
