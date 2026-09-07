##----------------------------------------------------------------------------##
## Tab: Trajectory — projection plot.
##
## Rendered by the shared cell-view Canvas engine. The trajectory path is sent
## as coordinate-space line segments alongside the cell payload.

##----------------------------------------------------------------------------##
## Reactive that prepares the cells + trajectory-line data for the current
## parameters (filtering, subsetting, hover, colours). One source of truth so
## the coordinates sent to the plot match those used for selection and hover.
##----------------------------------------------------------------------------##
trajectory_projection_job <- cerebro_async_latest_value(
  session,
  cerebro_async_source_call
)

trajectory_projection_request <- reactive({
  req(
    trajectory_selection_ok(),
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]],
    !is.null(input[["trajectory_projection_point_border"]]),
    !is.null(input[["trajectory_projection_keep_square"]])
  )

  trajectory_data <- trajectory_data_reactive()

  cells_df <- mergeTrajectoryWithMetaData(trajectory_data) %>%
    as.data.frame()

  groups <- getGroups()
  group_filters <- stats::setNames(
    lapply(groups, function(group) {
      selected <- input[[paste0("trajectory_projection_group_filter_", group)]]
      if (is.null(selected)) getGroupLevels(group) else selected
    }),
    groups
  )
  trajectory_edges <- trajectory_data[["edges"]]
  percentage <- input[["trajectory_percentage_cells_to_show"]]
  display <- list(
    color_variable = input[["trajectory_point_color"]],
    point_size = input[["trajectory_point_size"]],
    point_opacity = input[["trajectory_point_opacity"]],
    group_labels = isTRUE(input[["trajectory_projection_group_labels"]]),
    draw_border = isTRUE(input[["trajectory_projection_point_border"]]),
    keep_square = isTRUE(input[["trajectory_projection_keep_square"]])
  )
  list(
    key = cerebro_async_cache_key(
      available_crb_files$selected,
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]],
      percentage,
      group_filters,
      display
    ),
    args = list(
      root = file.path(
        Cerebro.options[["cerebro_root"]],
        "viewer",
        "trajectory"
      ),
      files = "async_workers.R",
      function_name = "trajectory_prepare_projection",
      args = list(
        cells_df = cells_df,
        trajectory_edges = trajectory_edges,
        group_filters = group_filters,
        percentage = percentage,
        display = display,
        line_color = cerebro_plotly_theme()$title,
        hover_groups = groups
      )
    )
  )
})

## Debounce the request so dragging a slider (point size / opacity /
## "% of cells") coalesces its rapid-fire input events into a single redraw
## and cancel any superseded daemon computation.
trajectory_projection_request <- debounceAfterFirst(
  trajectory_projection_request,
  200
)

trajectory_projection_prepared <- reactive({
  request <- trajectory_projection_request()
  trajectory_projection_job$invoke(request$key, request$args)
  trajectory_projection_job$result()
})

##----------------------------------------------------------------------------##
## Axis-reset state, mirroring the overview / gene-expression projections.
##
## Resetting axes on every render would re-run autorange on any parameter change
## (colour variable, point size, group filter) and the axes would visibly snap —
## on top of the reveal/resize settle this reads as a jump. So reset only when
## the *trajectory itself* changes (method/name); every other re-render holds the
## current range. Reset back to FALSE after each push so a subsequent parameter
## change does not autorange.
##----------------------------------------------------------------------------##
trajectory_projection_parameters_other <- reactiveValues(
  reset_axes = TRUE
)

observeEvent(
  {
    input[["trajectory_selected_method"]]
    input[["trajectory_selected_name"]]
  },
  {
    trajectory_projection_parameters_other[["reset_axes"]] <- TRUE
  }
)

##----------------------------------------------------------------------------##
## Observer that pushes the prepared data to the shared JS renderer.
##----------------------------------------------------------------------------##
observeEvent(
  list(
    trajectory_projection_prepared(),
    input[["trajectory_projection_render_request"]]
  ),
  {
    prepared <- trajectory_projection_prepared()
    req(prepared)

    ## resolve current reset_axes, then clear it so only a trajectory switch (not
    ## a colour / point-size tweak) triggers the next autorange.
    reset_axes_now <- isolate(
      trajectory_projection_parameters_other[["reset_axes"]]
    )
    trajectory_projection_parameters_other[["reset_axes"]] <- FALSE

    cells_df <- prepared[["cells_df"]]
    color_variable <- prepared[["color_variable"]]
    if (
      identical(color_variable, "state") &&
        is.numeric(cells_df[[color_variable]])
    ) {
      cells_df[[color_variable]] <- factor(cells_df[[color_variable]])
    }
    ## The projection coordinates are the DR_1 / DR_2 columns contributed by the
    ## trajectory meta (mergeTrajectoryWithMetaData appends them after the cell
    ## metadata, so they are NOT columns 1/2).
    coordinates <- list(cells_df[["DR_1"]], cells_df[["DR_2"]])
    color_input <- cells_df[[color_variable]]
    selection_keys <- if ("cell_barcode" %in% colnames(cells_df)) {
      as.character(cells_df[["cell_barcode"]])
    } else {
      rownames(cells_df)
    }

    point_line <- if (prepared[["draw_border"]]) {
      list(color = cerebro_plotly_theme()$axis, width = 1)
    } else {
      list()
    }

    color_assignments <- if (nrow(cells_df) == 0L) {
      character(0)
    } else {
      NULL
    }
    if (nrow(cells_df) > 0L && !is.numeric(color_input)) {
      color_assignments <- assignColorsToGroups(cells_df, color_variable)
      ## Fall back to the default colourset if the variable is not pre-assigned.
      if (is.null(color_assignments)) {
        levels_here <- unique(as.character(color_input))
        color_assignments <- stats::setNames(
          cerebro_group_colors(length(levels_here)),
          levels_here
        )
      }
    }

    payload <- cerebroCellViewScatterPayload(
      coordinates = coordinates,
      color = color_input,
      color_variable = color_variable,
      selection_keys = selection_keys,
      point_size = prepared[["point_size"]],
      point_opacity = prepared[["point_opacity"]],
      group_labels = prepared[["group_labels"]],
      keep_square = prepared[["keep_square"]],
      point_line = point_line,
      reset_axes = reset_axes_now,
      color_assignments = color_assignments,
      hover_info = prepared[["hover_info"]],
      space_label = input[["trajectory_selected_name"]]
    )
    cerebroCellViewRender(
      "trajectory_projection",
      payload[["meta"]],
      payload[["data"]],
      payload[["hover"]],
      extra = list(shapes = prepared[["trajectory_lines"]])
    )
  }
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_info[["text"]],
      title = trajectory_projection_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_info <- list(
  title = "Trajectory",
  text = p(
    "This plot shows cells projected into trajectory space, coloured by the specified meta info, e.g. sample or cluster. The path of the trajectory is shown as a black line. Specific to this analysis, every cell has a 'pseudotime' and a transcriptional 'state' which corresponds to its position along the trajectory path."
  )
)

##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (from the persistent selection).
##----------------------------------------------------------------------------##
trajectory_projection_selected_cells <- reactive({
  req(trajectory_selection_ok())

  ## The selection is held persistently on the JS side (shared
  ## cell_views.js) and pushed here as {x, y, ids} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes.
  ## The identifier matches how the selected-cells table keys cells
  ## (paste0 of the two projection coordinates with '-').
  sel <- input[["trajectory_projection_persistent_selection"]]
  if (is.null(sel) || is.null(sel[["x"]]) || length(sel[["x"]]) == 0) {
    return(NULL)
  }
  selection <- data.frame(
    x = as.numeric(sel[["x"]]),
    y = as.numeric(sel[["y"]]),
    identifier = paste0(as.numeric(sel[["x"]]), '-', as.numeric(sel[["y"]])),
    stringsAsFactors = FALSE
  )
  if (length(sel[["ids"]]) == nrow(selection)) {
    selection[["selection_key"]] <- as.character(sel[["ids"]])
  }

  ## Drop cells whose group is currently hidden via the legend, so the count and
  ## the selected-cells panels reflect only visible groups (shared helper in
  ## utility_functions.R). Coordinates come from the trajectory's DR_1 / DR_2,
  ## keyed the same way as the selection and the selected-cells table.
  hidden_groups <- input[["trajectory_projection_hidden_groups"]]
  if (length(hidden_groups) > 0) {
    color_variable <- input[["trajectory_point_color"]]
    trajectory_data <- getTrajectory(
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]]
    )
    metadata <- mergeTrajectoryWithMetaData(trajectory_data) %>%
      dplyr::mutate(
        identifier = paste0(DR_1, '-', DR_2),
        selection_key = as.character(cell_barcode)
      )
    selection <- filterSelectionByHiddenGroups(
      selection,
      metadata,
      color_variable,
      hidden_groups
    )
    if (is.null(selection) || nrow(selection) == 0) {
      return(NULL)
    }
  }

  selection
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["trajectory_number_of_selected_cells"]] <- renderUI({
  cerebroSelectionSummary(
    trajectory_projection_selected_cells(),
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]]
  )
})

output[["trajectory_projection_composition"]] <- renderUI({
  cerebroSelectionSummary(
    trajectory_projection_selected_cells(),
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]],
    composition = TRUE
  )
})

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##
