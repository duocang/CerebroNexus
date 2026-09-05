tab_trekker <- tabItem(
  tabName = "trekker",
  div(
    class = "trekker-page",
    cerebroVizPageHeader(
      "Trekker",
      "trekker_viz_info",
      "Explore official locations, run QC, marker genes, spatial genes, and source files."
    ),
    uiOutput("trekker_summary"),
    fluidRow(
      class = "cerebro-viz-row cerebro-viz-top-layout",
      column(
        width = 12,
        class = "cerebro-viz-toolbar-col",
        div(
          class = "cerebro-viz-toolbar",
          div(
            class = "cerebro-viz-primary",
            uiOutput("trekker_main_parameters_ui")
          ),
          cerebroToolbarActions(
            cerebroSettingsButton("trekker_more_button", "cv-more")
          ),
          cerebroSelectionStatus(
            "trekker_projection",
            "trekker_number_of_selected_cells",
            portable = FALSE
          )
        )
      ),
      column(
        width = 12,
        class = "cerebro-viz-col",
        cerebroCellViewOutput("trekker_projection"),
        div(id = "trekker_shared_insights")
      )
    ),
    fluidRow(column(width = 12, uiOutput("trekker_cell_inspector"))),
    fluidRow(
      column(
        width = 12,
        tabBox(
          width = 12,
          id = "trekker_analysis_tabs",
          tabPanel(
            "Run QC",
            uiOutput("trekker_metrics_cards"),
            DT::DTOutput("trekker_metrics_table")
          ),
          tabPanel(
            "Cluster markers",
            uiOutput("trekker_markers_empty"),
            DT::DTOutput("trekker_markers_table")
          ),
          tabPanel(
            "Spatial genes",
            uiOutput("trekker_moran_empty"),
            DT::DTOutput("trekker_moran_table")
          ),
          tabPanel("Official report", uiOutput("trekker_report")),
          tabPanel("Files & provenance", uiOutput("trekker_files"))
        )
      )
    )
  )
)
