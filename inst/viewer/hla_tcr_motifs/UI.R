##----------------------------------------------------------------------------##
## Tab: HLA & TCR Motifs
##
## A standalone top-level page (peer to Immune Repertoire) that rebuilds the
## CDR3 Hamming-1 motif network and layers donor-level HLA context onto it.
##
## Subtitle is a hard constraint from the design: everything on this page is
## exploratory HLA CONTEXT and association, never inferred restriction.
##
## Primary analysis choices stay above the visualization. Secondary analysis,
## display, and evidence controls use the same fixed settings drawer as the
## other specialist pages.
##----------------------------------------------------------------------------##

hlaMotifModebar <- function() {
  button <- function(
    action,
    label,
    icon_name,
    active = FALSE,
    disabled = FALSE
  ) {
    tags$button(
      type = "button",
      class = paste(
        "hla-mb-btn",
        if (active) "is-on" else NULL,
        if (disabled) "hla-mb-btn--off" else NULL
      ),
      `data-act` = action,
      `data-tip` = label,
      `aria-label` = label,
      disabled = if (disabled) "disabled" else NULL,
      icon(icon_name)
    )
  }
  tags$div(
    class = "hla-modebar",
    id = "hla-modebar",
    button("box", "Box select", "vector-square"),
    button("lasso", "Lasso select", "draw-polygon", active = TRUE),
    button("pan", "Pan", "up-down-left-right"),
    button("zoomin", "Zoom in", "search-plus"),
    button("zoomout", "Zoom out", "search-minus"),
    button("zsel", "Zoom to selection", "crop-simple", disabled = TRUE),
    button("reset", "Reset view", "house"),
    button("clear", "Clear selection", "eraser", disabled = TRUE),
    button("download", "Download PNG", "download")
  )
}

tab_hla_tcr_motifs <- tabItem(
  tabName = "hla_tcr_motifs",
  cerebroVizPageHeader(
    "HLA & TCR Motifs",
    "hla_visualizations_info",
    "Exploratory HLA context and association — not inferred restriction."
  ),
  fluidRow(
    class = "cerebro-viz-row cerebro-viz-top-layout",
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-toolbar-col",
      div(
        class = "cerebro-viz-toolbar",
        div(
          class = "cerebro-viz-primary",
          uiOutput("hla_parameters_ui")
        ),
        cerebroToolbarActions(
          cerebroSettingsButton("hla_more_button", "hla_more"),
          cerebroShareButton("hla_motif_network")
        ),
        cerebroSettingsDrawer(
          "hla_more",
          cerebroSettingsSection(
            "Appearance",
            uiOutput("hla_additional_params_ui"),
            cerebroInfoButton("hla_additional_parameters_info")
          ),
          cerebroSettingsSection(
            "Analysis",
            uiOutput("hla_more_parameters_ui")
          ),
          cerebroSettingsSection(
            "Evidence status",
            div(
              class = "cerebro-settings-full",
              uiOutput("hla_status_ui")
            ),
            cerebroInfoButton("hla_status_info")
          )
        ),
        cerebroSelectionStatus(
          "hla_motif_network",
          "hla_selected_count",
          client_actions = FALSE,
          portable = FALSE,
          extra_actions = actionButton(
            "hla_motif_network_focus_selection",
            tagList(icon("crop-simple"), tags$span("Focus")),
            class = paste(
              "btn btn-xs btn-default btn-breathing",
              "cerebro-selection-action-focus"
            ),
            `aria-pressed` = "false"
          )
        )
      )
    ),
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-col",
      shiny::tagAppendAttributes(
        tabsetPanel(
          id = "hla_tabs",
          tabPanel(
            "Motif Network",
            # The legend remains a full-width row above the plot. The custom
            # modebar floats inside the plot so long legends cannot collide with
            # it. visNetwork's own green navigation buttons are disabled in
            # visualizations.R for consistency with the other visualizations.
            tags$div(
              class = "hla-motif-tab",
              uiOutput("hla_legend_ui", class = "hla-legend-row"),
              # Fill the viewport instead of a hardcoded 640px: the wrapper is
              # sized to (viewport - its live top - a bottom gap) by
              # fill_height.js, and the network renders at height:100% inside it.
              # The legend above is a sibling, so when it wraps the wrapper's top
              # moves and the height re-measures itself. See www/fill_height.js.
              tags$div(
                class = "hla-plot-wrap",
                hlaMotifModebar(),
                tags$div(
                  class = "cerebro-fill",
                  shinycssloaders::withSpinner(
                    visNetwork::visNetworkOutput(
                      "hla_plot_motifNetwork",
                      height = "100%"
                    )
                  )
                ),
                uiOutput(
                  "hla_motif_network_composition",
                  class = "cerebro-selection-composition-slot"
                )
              )
            ),
            uiOutput("hla_motif_note"),
            # A picture cannot be recomputed or audited; the tables and their
            # manifest can. See output$hla_export_analysis.
            downloadButton(
              "hla_export_analysis",
              "Download analysis (tables + manifest)",
              class = "btn-sm"
            )
          ),
          tabPanel(
            "Network data",
            # Rendered server-side: the second grain is one row per OBSERVATION
            # UNIT, which is a cell only when the data set says so. A bulk
            # repertoire's rows are analysis units, so the label has to follow
            # the declared unit rather than being hard-coded here.
            uiOutput("hla_table_grain_ui"),
            DT::dataTableOutput("hla_network_table"),
            br(),
            downloadButton(
              "hla_network_download",
              "Download CSV",
              class = "btn-sm"
            )
          ),
          tabPanel(
            "HLA Associations",
            uiOutput("hla_associations_ui")
          ),
          tabPanel(
            "Data & QC",
            uiOutput("hla_data_qc_ui")
          )
        ),
        class = "cerebro-analysis-tabs"
      )
    )
  )
)
