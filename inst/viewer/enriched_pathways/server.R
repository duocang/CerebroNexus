##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##----------------------------------------------------------------------------##

source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/enriched_pathways/select_content.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/enriched_pathways/table.R"
  ),
  local = TRUE
)
