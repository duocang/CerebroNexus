##----------------------------------------------------------------------------##
## Tab: Preferences
##----------------------------------------------------------------------------##

##
output[["preferences_options"]] <- renderUI({
  tagList(
    h3(
      "Preferences",
      tags$span(class = "cerebro-advanced-tag", "advanced")
    ),
    tags$p(
      class = "cerebro-advanced-hint",
      "Optional performance settings — the defaults work well for most users."
    ),
    checkboxInput(
      "webgl_checkbox",
      label = "Use WebGL rendering",
      value = TRUE
    ),
    helpText("Improves performance but may not work in every browser."),
    checkboxInput(
      "hover_info_in_projections_checkbox",
      label = "Show cell details on hover",
      value = Cerebro.options[['projections_show_hover_info']]
    ),
    helpText("Shows additional cell metadata and may increase plotting time.")
  )
})

##----------------------------------------------------------------------------##
## Observe WebGL on?
##----------------------------------------------------------------------------##
observeEvent(input[["webgl_checkbox"]], {
  preferences[["use_webgl"]] <- input[["webgl_checkbox"]]
  print(glue::glue("[{Sys.time()}] WebGL status: {preferences[['use_webgl']]}"))
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
