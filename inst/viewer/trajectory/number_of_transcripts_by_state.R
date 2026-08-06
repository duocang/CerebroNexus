##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Number of transcripts by state.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_nUMI_by_state_UI"]] <- renderUI({
  req(trajectory_selection_ok())

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Number of transcripts by state"),
        cerebroInfoButton("states_nUMI_info")
      ),
      plotly::plotlyOutput("states_nUMI_plot")
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["states_nUMI_plot"]] <- plotly::renderPlotly({
  ##
  req(trajectory_selection_ok())

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  ##
  state_colors <- setNames(
    cerebro_group_colors(length(levels(trajectory_data[["meta"]]$state))),
    levels(trajectory_data[["meta"]]$state)
  )

  ##
  mergeTrajectoryWithMetaData(trajectory_data) %>%
    dplyr::filter(!is.na(pseudotime)) %>%
    plotly::plot_ly(
      x = ~state,
      y = ~nUMI,
      type = "violin",
      box = list(
        visible = TRUE
      ),
      meanline = list(
        visible = TRUE
      ),
      color = ~state,
      colors = state_colors,
      source = "subset",
      showlegend = FALSE,
      hoverinfo = "y",
      marker = list(
        size = 5
      )
    ) %>%
    plotly::layout(
      title = "",
      xaxis = cerebro_plotly_axis(title = "", mirror = FALSE),
      yaxis = cerebro_plotly_axis(
        title = "Number of transcripts",
        mirror = FALSE,
        hoverformat = ".0f"
      ),
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      dragmode = "select",
      hovermode = "compare"
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["states_nUMI_info"]], {
  showModal(
    modalDialog(
      states_nUMI_info[["text"]],
      title = states_nUMI_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

states_nUMI_info <- list(
  title = "Number of transcripts by state",
  text = p(
    "Violin plot of the number of transcripts (UMIs) found in each state."
  )
)
