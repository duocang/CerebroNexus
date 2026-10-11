## Read old production bodies into a temporary directory; never replace checkout files.
pkgload::load_all(quiet = TRUE)
old <- tempfile("pr7-baseline-")
dir.create(old)
for (name in c("UI_expression_in_selected_cells.R", "UI_table_of_selected_cells.R",
               "obj_projection_expression_levels.R", "UI_expression_by_group.R")) {
  writeLines(system2("git", c("show", paste0("2dbd6668:inst/viewer/gene_expression/", name)), stdout = TRUE), file.path(old, name))
}
options(gene.selection.source = old, gene.lifecycle.source = old)
testthat::test_file("tests/testthat/test-gene-selection-canonical.R", reporter = "summary")
testthat::test_file("tests/testthat/test-gene-progress-lifecycle.R", reporter = "summary")
