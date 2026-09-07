builder_test_inst_path <- function(...) {
  relative <- file.path(...)
  path <- testthat::test_path("..", "..", "inst", relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  path
}

builder_profile_inst_path <- builder_test_inst_path
builder_table_inst_path <- builder_test_inst_path
builder_content_immune_inst_path <- builder_test_inst_path
builder_content_spatial_inst_path <- builder_test_inst_path
builder_spatial_test_inst_path <- builder_test_inst_path
spatial_contract_inst_path <- builder_test_inst_path
