##----------------------------------------------------------------------------##
## Tab: Load data
##----------------------------------------------------------------------------##
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/load_data/select_file.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/load_data/sample_info.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/load_data/preferences.R"
  ),
  local = TRUE
)
