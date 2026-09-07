tab_trekker <- tabItem(
  tabName = "trekker",
  div(
    class = "trekker-page",
    div(
      class = "tk-page-head",
      div(
        class = "tk-page-title",
        cerebroVizPageHeader(
          "Trekker",
          "trekker_viz_info",
          "Explore positioning evidence, locations, QC, spatial biology, and files."
        )
      ),
      uiOutput("trekker_summary")
    ),
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
            portable = FALSE,
            extra_actions = uiOutput("trekker_selection_export")
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
        div(
          class = "tk-analysis-tabs",
          tabBox(
            width = 12,
            id = "trekker_analysis_tabs",
            tabPanel(
              "Overview",
              uiOutput("trekker_run_context"),
              uiOutput("trekker_positioning_filter_controls"),
              div(
                class = "tk-chart-panel tk-wide-chart",
                tags$h4("Spatial RNA quality"),
                uiOutput("trekker_qc_filter_controls"),
                uiOutput("trekker_spatial_qc_empty"),
                uiOutput("trekker_spatial_preview_note"),
                plotly::plotlyOutput(
                  "trekker_spatial_qc_plot",
                  height = "380px"
                )
              ),
              div(
                class = "tk-chart-grid",
                div(
                  class = "tk-chart-panel",
                  tags$h4("Positioning funnel"),
                  uiOutput("trekker_positioning_funnel_empty"),
                  plotly::plotlyOutput(
                    "trekker_positioning_funnel",
                    height = "280px"
                  )
                ),
                div(
                  class = "tk-chart-panel",
                  tags$h4("Assigned spatial locations"),
                  uiOutput("trekker_position_distribution_empty"),
                  plotly::plotlyOutput(
                    "trekker_position_distribution",
                    height = "280px"
                  )
                )
              ),
              uiOutput("trekker_metrics_cards"),
              div(
                class = "tk-chart-panel tk-wide-chart",
                tags$h4("Run QC details"),
                uiOutput("trekker_run_qc_details_empty"),
                plotly::plotlyOutput(
                  "trekker_run_qc_details_plot",
                  height = "620px"
                )
              ),
              div(
                class = "tk-chart-panel tk-cohort-qc",
                tags$h4("Active-cohort QC"),
                uiOutput("trekker_cohort_qc_empty"),
                plotly::plotlyOutput(
                  "trekker_cohort_qc_plot",
                  height = "340px"
                )
              ),
              DT::DTOutput("trekker_metrics_table")
            ),
            tabPanel(
              "Cluster markers",
              div(
                class = "tk-chart-panel tk-wide-chart",
                tags$h4("All-cluster marker profile"),
                uiOutput("trekker_marker_dot_empty"),
                plotly::plotlyOutput(
                  "trekker_marker_dot_plot",
                  height = "560px"
                )
              ),
              tags$h4("Selected-cluster markers"),
              uiOutput("trekker_marker_controls"),
              uiOutput("trekker_markers_empty"),
              plotly::plotlyOutput("trekker_marker_plot", height = "360px"),
              DT::DTOutput("trekker_markers_table")
            ),
            tabPanel(
              "Spatial genes",
              div(
                class = "tk-chart-panel tk-wide-chart",
                tags$h4("Gene evidence"),
                uiOutput("trekker_gene_evidence_controls"),
                uiOutput("trekker_gene_evidence_empty"),
                plotly::plotlyOutput(
                  "trekker_gene_evidence_maps",
                  height = "500px"
                ),
                plotly::plotlyOutput(
                  "trekker_gene_evidence_distribution",
                  height = "320px"
                ),
                DT::DTOutput("trekker_gene_evidence_table")
              ),
              div(
                class = "tk-chart-panel tk-wide-chart",
                tags$h4("Multi-gene spatial expression"),
                uiOutput("trekker_multigene_controls"),
                uiOutput("trekker_multigene_empty"),
                plotly::plotlyOutput(
                  "trekker_multigene_plot",
                  height = "480px"
                )
              ),
              div(
                class = "tk-chart-panel tk-wide-chart",
                tags$h4("Top spatial genes"),
                uiOutput("trekker_spatial_gallery_empty"),
                uiOutput("trekker_spatial_gallery_note"),
                plotly::plotlyOutput(
                  "trekker_spatial_gene_gallery",
                  height = "600px"
                )
              ),
              tags$h4("Official Moran ranking"),
              uiOutput("trekker_moran_empty"),
              plotly::plotlyOutput("trekker_moran_plot", height = "360px"),
              tags$h4("Marker × spatial evidence"),
              uiOutput("trekker_marker_moran_empty"),
              plotly::plotlyOutput(
                "trekker_marker_moran_plot",
                height = "420px"
              ),
              DT::DTOutput("trekker_moran_table")
            ),
            tabPanel(
              "Neighbourhood",
              uiOutput("trekker_neighbourhood_controls"),
              uiOutput("trekker_neighbourhood_note"),
              div(
                class = "tk-chart-grid",
                plotly::plotlyOutput(
                  "trekker_adjacency_plot",
                  height = "460px"
                ),
                plotly::plotlyOutput(
                  "trekker_neighbour_composition",
                  height = "460px"
                )
              ),
              div(
                class = "tk-chart-panel tk-wide-chart",
                tags$h4("Cluster spatial profiles"),
                uiOutput("trekker_spatial_profile_note"),
                plotly::plotlyOutput(
                  "trekker_spatial_profile_plot",
                  height = "420px"
                ),
                DT::DTOutput("trekker_spatial_profile_table")
              ),
              div(
                class = "tk-chart-grid",
                div(
                  class = "tk-chart-panel",
                  tags$h4("Local neighbourhood"),
                  plotly::plotlyOutput(
                    "trekker_local_neighbourhood_plot",
                    height = "460px"
                  )
                ),
                div(
                  class = "tk-chart-panel",
                  tags$h4("UMAP-layout vs physical neighbourhood overlap"),
                  tags$p(
                    class = "tk-note",
                    paste(
                      "Exploratory 2-D layout agreement; this is not",
                      "high-dimensional transcriptomic agreement."
                    )
                  ),
                  plotly::plotlyOutput(
                    "trekker_knn_overlap_plot",
                    height = "460px"
                  )
                )
              )
            ),
            tabPanel(
              "Sections",
              uiOutput("trekker_sections_empty"),
              div(
                class = "tk-chart-grid",
                plotly::plotlyOutput(
                  "trekker_section_composition",
                  height = "440px"
                ),
                plotly::plotlyOutput(
                  "trekker_section_qc",
                  height = "440px"
                )
              )
            ),
            tabPanel(
              "Files",
              uiOutput("trekker_report"),
              uiOutput("trekker_files"),
              tags$h4("Data integrity"),
              DT::DTOutput("trekker_audit_table")
            )
          )
        )
      )
    )
  )
)
