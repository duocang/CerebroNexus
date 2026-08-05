test_that("the real-data benchmark has a repository-only package boundary", {
  repo <- normalizePath(file.path("..", "..", "..", ".."))
  bench <- file.path(repo, "tools", "bench")

  expect_true(dir.exists(bench))
  expect_true(file.exists(file.path(bench, "run_sweep.sh")))
  expect_true(file.exists(file.path(bench, "run_contract_tests.R")))
  expect_true(dir.exists(file.path(bench, "tests", "testthat")))

  package_tests <- list.files(
    file.path(repo, "tests", "testthat"),
    pattern = "^test-bench-.*[.]R$"
  )
  expect_length(package_tests, 0L)

  build_ignore <- readLines(file.path(repo, ".Rbuildignore"), warn = FALSE)
  expect_true("^tools/bench$" %in% build_ignore)
  expect_false("^tests/bench$" %in% build_ignore)

  description <- read.dcf(file.path(repo, "DESCRIPTION"))
  suggests <- trimws(strsplit(description[1, "Suggests"], ",")[[1]])
  expect_false("rhdf5" %in% suggests)
})

test_that("the pinned development shell includes benchmark dependencies", {
  repo <- normalizePath(file.path("..", "..", "..", ".."))
  nix <- readLines(file.path(repo, "default.nix"), warn = FALSE)

  expect_true(any(grepl("^[[:space:]]+rhdf5$", nix)))
  expect_true(any(grepl(
    "buildInputs = [ BPCells ] ++ rpkgs ++ system_packages;",
    nix,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "buildInputs = [ BPCells rpkgs system_packages ];",
    nix,
    fixed = TRUE
  )))
})

test_that("the package vignette guides users without citing pilot rankings", {
  repo <- normalizePath(file.path("..", "..", "..", ".."))
  article <- readLines(
    file.path(repo, "vignettes", "expression_backend_benchmark.Rmd"),
    warn = FALSE
  )
  text <- paste(article, collapse = "\n")

  expect_match(text, "Choosing an expression backend")
  expect_false(grepl("Exploratory pilot results", text, fixed = TRUE))
  expect_false(grepl("105x", text, fixed = TRUE))
  expect_false(grepl("tests/bench/run_sweep.sh", text, fixed = TRUE))
  expect_match(text, "validated immutable publication run")
})
