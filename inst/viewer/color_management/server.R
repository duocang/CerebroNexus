##----------------------------------------------------------------------------##
## Tab: Color management
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with color selection boxes for each level in each grouping
## variable.
##----------------------------------------------------------------------------##

output[["color_assignments_UI"]] <- renderUI({
  color_card <- function(group_name, levels, info_id) {
    color_inputs <- lapply(levels, function(level) {
      shiny::tagAppendAttributes(
        colourpicker::colourInput(
          inputId = paste0(
            "color_",
            group_name,
            "_",
            gsub(level, pattern = "[^[:alnum:]]", replacement = "_")
          ),
          label = level,
          value = reactive_colors()[[group_name]][level],
          showColour = "both",
          closeOnClick = TRUE,
          width = "100%"
        ),
        class = "cerebro-color-row"
      )
    })

    shiny::tagAppendAttributes(
      box(
        title = tagList(
          boxTitle(group_name),
          tags$span(
            class = "cerebro-color-count",
            paste(length(levels), "colours")
          ),
          cerebroInfoButton(
            info_id,
            onclick = paste0(
              "Shiny.setInputValue('color_assignments_info', this.id, ",
              "{priority: 'event'});"
            )
          )
        ),
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        collapsible = TRUE,
        tags$div(class = "cerebro-color-list", color_inputs)
      ),
      class = "cerebro-color-card"
    )
  }

  fluidRow(
    tagList({
      group_list <- list()
      groups <- getGroups()
      for (group_index in seq_along(groups)) {
        group_name <- groups[[group_index]]
        group_list[[group_name]] <- color_card(
          group_name,
          getGroupLevels(group_name),
          paste0("color_assignments_info_group_", group_index)
        )
      }

      ## if there are columns with cell cycle info, add color selection elements
      ## also for those
      if (length(getCellCycle()) > 0) {
        cell_cycle_columns <- getCellCycle()
        for (column_index in seq_along(cell_cycle_columns)) {
          column <- cell_cycle_columns[[column_index]]
          group_list[[column]] <- color_card(
            column,
            unique(as.character(getMetaData()[[column]])),
            paste0("color_assignments_info_cycle_", column_index)
          )
        }
      }

      group_list
    })
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["color_assignments_info"]], {
  showModal(
    modalDialog(
      color_assignments_info[["text"]],
      title = color_assignments_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

color_assignments_info <- list(
  title = "Colors for groups",
  text = p(
    "Using this interface, you can assign new colors to each of the groups which will then be used across all tabs in CerebroNexus."
  )
)
