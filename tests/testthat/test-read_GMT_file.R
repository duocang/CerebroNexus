## Guards the GMT parser against the blank-line regression: readLines() does not
## skip empty rows the way readr::read_delim() (the previous parser) did, so a
## GMT file ending in a newline would otherwise yield a phantom all-NA gene set.

test_that(".read_GMT_file skips blank lines (no phantom NA gene set)", {
  read_gmt <- getFromNamespace(".read_GMT_file", "CerebroNexus")

  gmt_path <- system.file(
    "extdata",
    "example_gene_set.gmt",
    package = "CerebroNexus"
  )
  if (!nzchar(gmt_path)) {
    gmt_path <- testthat::test_path("../../inst/extdata/example_gene_set.gmt")
  }

  res <- read_gmt(gmt_path)

  ## The bundled file holds 2 gene sets and ends in a trailing newline; the
  ## blank line must NOT become a third, all-NA set.
  expect_length(res$genesets, 2L)
  expect_length(res$geneset.names, 2L)
  expect_false(any(is.na(res$geneset.names)))
  expect_false(any(vapply(res$genesets, function(g) all(is.na(g)), logical(1))))
})

test_that(".read_GMT_file drops interior and trailing blank lines", {
  read_gmt <- getFromNamespace(".read_GMT_file", "CerebroNexus")

  tmp <- tempfile(fileext = ".gmt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "SET_A\tdesc A\tGENE1\tGENE2",
      "",
      "SET_B\tdesc B\tGENE3",
      "   ",
      ""
    ),
    tmp
  )

  res <- read_gmt(tmp)

  expect_equal(res$geneset.names, c("SET_A", "SET_B"))
  expect_equal(res$genesets[[1]], c("GENE1", "GENE2"))
  expect_equal(res$genesets[[2]], "GENE3")
})
