test_that("run-demo loads the worktree and avoids a busy default port", {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  skip_if_not(
    file.exists(launcher),
    "repository-only demo launcher is not installed with the package"
  )
  script <- paste(
    readLines(launcher, warn = FALSE),
    collapse = "\n"
  )

  expect_match(script, "pkgload::load_all\\([[:space:]]*repo_root")
  expect_match(script, "httpuv::randomPort", fixed = TRUE)
  expect_match(script, "port = port", fixed = TRUE)
})
