##----------------------------------------------------------------------------##
## Tab: Preferences
##----------------------------------------------------------------------------##

##
output[["preferences_options"]] <- renderUI({
  preference <- function(id, label, value, help) {
    control <- checkboxInput(id, label, value)
    control <- htmltools::tagQuery(control)$find("input")$addAttrs(
      role = "switch", `aria-describedby` = paste0(id, "_help")
    )$allTags()
    div(
      class = "cerebro-preference", title = help,
      control,
      tags$span(id = paste0(id, "_help"), class = "sr-only", help)
    )
  }
  tagList(
    div(
      class = "cerebro-preferences-heading",
      tags$span("Preferences"),
      tags$span(id = "cerebro-renderer-status", class = "cerebro-renderer-status",
        tabindex = "0", role = "status", `aria-live` = "polite",
        title = "Automatic GPU acceleration when supported; otherwise Canvas.", "Auto")
    ),
    preference(
      "webgl_checkbox",
      "GPU acceleration", TRUE,
      "Automatically uses WebGPU or WebGL for supported plots. Turn off to use CPU rendering. Small or unsupported cell views may use Canvas."
    ),
    preference(
      "hover_info_in_projections_checkbox",
      "Cell details", isTRUE(Cerebro.options[['projections_show_hover_info']]),
      "Show cell details on hover in Projection, Gene expression, Spatial and trajectory plots. Linked views has its own linked hover control."
    )
  )
})

##----------------------------------------------------------------------------##
## Observe WebGL on?
##----------------------------------------------------------------------------##
observeEvent(input[["webgl_checkbox"]], {
  preferences[["use_webgl"]] <- input[["webgl_checkbox"]]
  session$sendCustomMessage("cerebro_gpu_acceleration", list(
    enabled = isTRUE(input[["webgl_checkbox"]])
  ))
})

##----------------------------------------------------------------------------##
## Observe hover on?
##----------------------------------------------------------------------------##

observeEvent(input[["hover_info_in_projections_checkbox"]], {
  preferences[["show_hover_info_in_projections"]] <- input[[
    "hover_info_in_projections_checkbox"
  ]]
  print(glue::glue(
    "[{Sys.time()}] Show hover info status: {preferences[['show_hover_info_in_projections']]}"
  ))
})
