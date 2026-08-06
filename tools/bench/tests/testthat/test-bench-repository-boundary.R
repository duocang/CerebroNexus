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

  gitignore <- readLines(file.path(repo, ".gitignore"), warn = FALSE)
  expect_true("!tools/bench/result/runs/**/figures/*.png" %in% gitignore)
  expect_true("!tools/bench/result/runs/**/figures/*.pdf" %in% gitignore)
  expect_true("!tools/bench/result/runs/**/figures/*.svg" %in% gitignore)
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

  recommendation <- regexpr(
    "Short answer: start with `h5`",
    text,
    fixed = TRUE
  )[1]
  overview <- regexpr("# Overview", text, fixed = TRUE)[1]
  expect_gt(recommendation, 0L)
  expect_lt(recommendation, overview)
  expect_match(
    text,
    "Use `embedded` when you need one portable file",
    fixed = TRUE
  )
  expect_match(text, "Use `bpcells` when export speed", fixed = TRUE)
})

test_that("the complete package guide mirrors current publication figures", {
  repo <- normalizePath(file.path("..", "..", "..", ".."))
  result_root <- file.path(repo, "tools", "bench", "result")
  run_id <- readLines(file.path(result_root, "CURRENT"), warn = FALSE)
  expect_length(run_id, 1L)

  figures <- paste0(
    "expression_backend_benchmark_",
    c(
      "overview",
      "observed_scaling",
      "repeats",
      "query_latency",
      "pareto",
      "correctness"
    ),
    ".png"
  )
  evidence <- file.path(result_root, "runs", run_id, "figures", figures)
  vignette <- file.path(
    repo,
    "vignettes",
    "figures",
    "expression_backend_benchmark",
    figures
  )

  expect_true(all(file.exists(evidence)))
  expect_true(all(file.exists(vignette)))
  expect_identical(
    unname(tools::md5sum(vignette)),
    unname(tools::md5sum(evidence))
  )

  article <- paste(
    readLines(
      file.path(repo, "vignettes", "expression_backend_benchmark.Rmd"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expected_references <- paste(
    "figures",
    "expression_backend_benchmark",
    figures,
    sep = "/"
  )
  expect_match(article, run_id, fixed = TRUE)
  expect_true(all(vapply(
    expected_references,
    grepl,
    logical(1),
    x = article,
    fixed = TRUE
  )))

  readme <- paste(
    readLines(file.path(repo, "README.md"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(readme, "expression_backend_benchmark")

  gitignore <- readLines(file.path(repo, ".gitignore"), warn = FALSE)
  expect_true("/.superpowers/" %in% gitignore)
})
