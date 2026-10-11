args <- commandArgs(TRUE)
pkgload::load_all(args[[1]],quiet=TRUE)
testthat::test_local(args[[1]],filter="gene-expression-panel-modes|gene-expression-color-transport|expression-rgb-dataset-switch|single-expression-incremental|gene-expression-interaction-performance", reporter="summary", stop_on_failure=TRUE)
