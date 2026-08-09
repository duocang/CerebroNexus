##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
output[["spatial_projection_UI"]] <- renderUI({
  grid_style <- tags$style(HTML(
    paste0(
      ".spatial-projection-grid { display: grid; ",
      "grid-template-columns: repeat(auto-fit, ",
      "minmax(min(100%, 20rem), 1fr)); gap: 8px; }",
      ".spatial-projection-item > .col-sm-12 { ",
      "padding-left: 8px; padding-right: 8px; }"
    )
  ))
  fluidRow(
    class = "cerebro-viz-row",
    ## selections and parameters
    column(
      width = 3,
      offset = 0,
      class = "cerebro-param-col",
      tags$div(
        id = "spatial_main_parameters_wrapper",
        cerebroBox(
          title = tagList(
            "Main parameters",
            cerebroInfoButton("spatial_projection_main_parameters_info")
          ),
          uiOutput("spatial_projection_main_parameters_UI")
        )
      ),
      tags$div(
        id = "spatial_additional_parameters_wrapper",
        cerebroBox(
          title = tagList(
            "Additional parameters",
            cerebroInfoButton("spatial_projection_additional_parameters_info")
          ),
          uiOutput("spatial_projection_additional_parameters_UI"),
          collapsed = TRUE
        )
      ),
      cerebroBox(
        title = tagList(
          "Group filters",
          cerebroInfoButton("spatial_projection_group_filters_info")
        ),
        uiOutput("spatial_projection_group_filters_UI"),
        collapsed = TRUE
      )
    ),
    ## plot
    column(
      width = 9,
      offset = 0,
      class = "cerebro-viz-col",
      grid_style,
      tags$div(
        class = "spatial-projection-toolbar",
        tags$h4(
          style = "display:inline-block;margin:0 8px 12px 0;",
          "Dimensional reduction"
        ),
        cerebroInfoButton("spatial_projection_info"),
        shinyWidgets::dropdownButton(
          tags$div(
            style = "color: black !important;",
            uiOutput("spatial_projection_show_group_label_UI"),
            uiOutput("spatial_projection_point_border_UI"),
            uiOutput("spatial_projection_scales_UI")
          ),
          circle = FALSE,
          icon = icon("cog"),
          inline = TRUE,
          size = "xs"
        )
      ),
      tags$div(
        class = "spatial-projection-grid-container cerebro-projection-gate",
        uiOutput("spatial_projection_grid_UI")
      )
    )
  )
})
