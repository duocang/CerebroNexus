builder_path_contract_files <- function() {
  source_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "bundle_path_contract.R"
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

test_that("Builder path safety is a byte-identical core contract", {
  paths <- builder_path_contract_files()

  testthat::skip_if_not(
    file.exists(paths$source),
    "R/ source tree not present (installed-package layout)"
  )
  expect_true(file.exists(paths$runtime))
  expect_identical(
    readBin(paths$source, "raw", n = file.info(paths$source)$size),
    readBin(paths$runtime, "raw", n = file.info(paths$runtime)$size)
  )
})
