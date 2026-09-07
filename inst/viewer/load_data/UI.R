##----------------------------------------------------------------------------##
## Tab: Load data
##----------------------------------------------------------------------------##
tab_load_data <- tabItem(
  tabName = "loadData",
  ## Keep the status beside the dataset controls so it remains visible when a
  ## user switches a configured dataset after the initial load.
  uiOutput("load_data_select_file_UI"),
  div(
    id = "cerebro-dataset-stage",
    class = "cerebro-dataset-stage is-loading",
    `aria-busy` = "true",
    div(
      id = "cerebro-dataset-loading",
      class = "cerebro-dataset-loading",
      role = "status",
      `aria-live` = "polite",
      `data-output-id` = "load_data_number_of_cells",
      icon("spinner", class = "fa-spin"),
      div(
        tags$strong("Loading dataset..."),
        tags$p("Large datasets may take a while. The app is still running.")
      )
    ),
    uiOutput("load_data_sample_info_UI")
  ),
  ## Order reflects priority: pick a dataset, see its stats, then the low-frequency
  ## preferences sink to the bottom of the page.
  ## Low-frequency advanced settings — visually de-emphasised and sunk to the
  ## bottom (see .cerebro-advanced in www/custom.css).
  div(class = "cerebro-advanced", uiOutput("preferences_options"))
)
