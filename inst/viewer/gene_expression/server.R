##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##
files_to_load <- list.files(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/gene_expression"),
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)
## shiny_server.R already loads the colour scales shared with Spatial.
files_to_load <- files_to_load[basename(files_to_load) != "func_color_scale.R"]

for (i in files_to_load) {
  source(i, local = TRUE)
}
