##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
output[["spatial_projection_UI"]] <- renderUI({
  tagList(
    cerebroVizPageHeader(
      "Spatial",
      "spatial_projection_info",
      "Explore spatial organization, expression, and tissue context."
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
            uiOutput("spatial_projection_main_parameters_UI")
          ),
          cerebroSettingsButton(
            "spatial_projection_more_button",
            "spatial_projection_more"
          ),
          cerebroSettingsDrawer(
            "spatial_projection_more",
            cerebroSettingsSection(
              "Appearance",
              tagList(
                uiOutput("spatial_projection_scatter_parameters_UI"),
                uiOutput("spatial_projection_show_group_label_UI"),
                checkboxInput(
                  "spatial_projection_keep_square",
                  "Keep plots square",
                  value = FALSE
                ),
                uiOutput("spatial_projection_point_border_UI"),
                uiOutput("spatial_projection_show_region_outline_UI")
              ),
              cerebroInfoButton(
                "spatial_projection_additional_parameters_info"
              )
            ),
            cerebroSettingsSection(
              "Data",
              uiOutput("spatial_projection_data_parameters_UI")
            ),
            cerebroSettingsSection(
              "Background image",
              div(
                class = "spatial-image-controls",
                uiOutput("spatial_projection_background_selector_UI"),
                uiOutput("spatial_projection_background_parameters_UI")
              )
            ),
            cerebroSettingsSection(
              "Group filters",
              uiOutput("spatial_projection_group_filters_UI"),
              cerebroInfoButton("spatial_projection_group_filters_info")
            )
          ),
          cerebroSelectionStatus(
            "spatial_projection",
            "spatial_number_of_selected_cells"
          )
        )
      ),
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-col",
        div(
          class = "spatial-viz-surface",
          ## Spatial autocorrelation stays visible without consuming plot height.
          conditionalPanel(
            condition = "input.spatial_projection_plot_type == 'ImageFeaturePlot'",
            tags$div(
              class = "spatial-moran-overlay",
              tags$strong("Moran's I:"),
              textOutput("spatial_projection_morans_i", inline = TRUE),
              actionLink(
                "spatial_projection_morans_i_info",
                label = NULL,
                icon = icon("circle-info"),
                title = "What is Moran's I?",
                style = "color: #999;"
              )
            )
          ),
          cerebroCellViewOutput("spatial_projection")
        )
      )
    )
  )
})
