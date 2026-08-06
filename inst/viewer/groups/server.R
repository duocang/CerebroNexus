##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/groups/select_group.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/groups/composition.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/groups/expression_metrics.R"
  ),
  local = TRUE
)
