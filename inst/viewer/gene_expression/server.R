##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##
## The scatter controls Overview and Gene expression share.
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/overview/scatter_parameters.R"
  ),
  local = TRUE
)
files_to_load <- list.files(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/gene_expression"),
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)

for (i in files_to_load) {
  source(i, local = TRUE)
}
