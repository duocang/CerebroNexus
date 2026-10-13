##----------------------------------------------------------------------------##
## Tab: Load data
##----------------------------------------------------------------------------##
tab_load_data <- tabItem(
  tabName = "loadData",
  tags$h1("Data info"),
  ## Dataset selection is global; this page contains its metadata and upload UI.
  uiOutput("load_data_select_file_UI"),
  h3("Sample information"),
  fluidRow(
    class = "cerebro-dataset-stats",
    column(4, valueBoxOutput("load_data_number_of_cells", width = NULL)),
    column(4, valueBoxOutput("load_data_organism", width = NULL)),
    column(4, valueBoxOutput("load_data_date_of_export", width = NULL))
  ),
  ## Low-frequency advanced settings — visually de-emphasised and sunk to the
  ## bottom (see .cerebro-advanced in www/custom.css).
  div(class = "cerebro-advanced", uiOutput("preferences_options"))
)
