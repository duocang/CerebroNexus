test_that("Ren backend comparison is narrow, isolated, and correctness gated", {
  candidates <- c(
    file.path("tests", "bench"),
    file.path("..", "bench"),
    file.path("..", "..", "tests", "bench")
  )
  bench_root <- candidates[dir.exists(candidates)][1L]
  skip_if(is.na(bench_root), "benchmark scripts are unavailable")
  script <- readLines(
    file.path(bench_root, "benchmark_ren_expression_backends.R"),
    warn = FALSE
  )
  source <- paste(script, collapse = "\n")
  expect_match(source, 'identical(args[[1L]], "--plan")', fixed = TRUE)
  expect_match(source, 'identical(args[[1L]], "--measure")', fixed = TRUE)
  expect_match(source, "rep(plan$panel, 3L)", fixed = TRUE)
  expect_match(source, "row_ok && block_ok", fixed = TRUE)
  expect_match(source, "independent warm-cache processes", fixed = TRUE)
  expect_no_match(source, "BENCH_PROFILE=publication", fixed = TRUE)
})
