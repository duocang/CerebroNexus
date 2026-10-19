##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
## The content contract and func_spatial_helpers.R are already in the session
## scope (shiny_server.R sources them once for every page).
files_to_load <- list.files(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/spatial"),
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)
files_to_load <- files_to_load[
  basename(files_to_load) != "func_spatial_helpers.R"
]

for (i in files_to_load) {
  source(i, local = TRUE)
}
