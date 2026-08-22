##----------------------------------------------------------------------------##
## Select group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["groups_select_group_UI"]] <- renderUI({
  tagList(
    div(
      class = "cerebro-compact-select",
      tags$h3("Choose a grouping variable:"),
      selectInput(
        "groups_selected_group",
        label = NULL,
        choices = getGroups(),
        width = "100%"
      )
    )
  )
})
