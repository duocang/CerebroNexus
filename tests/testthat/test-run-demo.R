test_that("run-demo loads the worktree and avoids a busy default port", {
  script <- paste(
    readLines(testthat::test_path("..", "..", "run-demo.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(script, "pkgload::load_all\\([[:space:]]*repo_root")
  expect_match(script, "httpuv::randomPort", fixed = TRUE)
  expect_match(script, "port = port", fixed = TRUE)
})
