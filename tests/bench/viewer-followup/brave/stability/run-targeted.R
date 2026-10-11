pkgload::load_all('.', quiet = TRUE)
source('tests/testthat/helper-viewer-path.R')
for (file in c('test-trajectory-indexed-columns.R',
               'test-trajectory-merge.R',
               'test-trajectory-selection-valid.R',
               'test-linked-selection-output-lifecycle.R',
               'test-trajectory-selection-demand.R')) {
  path <- file.path('tests/testthat', file)
  stopifnot(file.exists(path))
  result <- testthat::test_file(path, reporter = 'summary')
  stopifnot(!any(as.data.frame(result)$failed > 0),
            !any(as.data.frame(result)$error))
}
