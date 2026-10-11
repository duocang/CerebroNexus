source('tests/bench/viewer-followup/brave/stability/run-targeted.R')
result <- testthat::test_file('tests/testthat/test-viewer-dataset-context.R',reporter='summary')
stopifnot(!any(as.data.frame(result)$failed > 0),!any(as.data.frame(result)$error))
