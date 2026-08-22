##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

## Shared Canvas projection engine loaded once app-wide (see shiny_UI.R); this
## tab only supplies thin data-specific wrappers.
js_code_trajectory_projection <- cerebro_read_file(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/js_projection_update_plot.js"
  )
)

tab_trajectory <- tabItem(
  tabName = "trajectory",
  shinyjs::inlineCSS(
    "
    #trajectory_details_selected_cells_table .table th {
      text-align: center;
    }
    #states_by_group_table .table th {
      text-align: center;
    }
    "
  ),
  shinyjs::extendShinyjs(
    text = js_code_trajectory_projection,
    functions = c(
      "trajectoryUpdatePlot2DContinuous",
      "trajectoryUpdatePlot2DCategorical",
      "trajectoryClearSelection",
      "trajectoryZoomToSelection"
    )
  ),
  uiOutput("trajectory_projection_UI"),
  uiOutput("trajectory_selected_cells_table_UI"),
  uiOutput("trajectory_distribution_along_pseudotime_UI"),
  uiOutput("trajectory_states_by_group_UI"),
  uiOutput("trajectory_expression_metrics_UI")
)
