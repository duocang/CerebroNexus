##----------------------------------------------------------------------------##
## Table for details of selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with toggle switches (for automatic number formatting and
## coloring) and table.
##----------------------------------------------------------------------------##
output[["expression_details_selected_cells_UI"]] <- renderUI({
  req(
    expression_projection_summary_ready(),
    expression_selection_present()
  )
  req(expression_summary_data())
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Details of selected cells"),
        cerebroInfoButton("expression_details_selected_cells_info")
      ),
      tagList(
        shinyWidgets::materialSwitch(
          inputId = "expression_details_selected_cells_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "expression_details_selected_cells_color_highlighting",
          label = "Highlight values with colours:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        DT::dataTableOutput("expression_details_selected_cells")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["expression_details_selected_cells"]] <- DT::renderDataTable({
  ## The browser lists a selection it reported by row.
  req(!viewerClientSelection("expression_projection", "table"))
  req(
    expression_projection_summary_ready(),
    expression_projection_data(),
    expression_projection_coordinates(),
    expression_summary_data(),
    expression_projection_selected_cells()
  )
  summary <- expression_summary_data()
  ## Columns are built for the selected rows only, not for every cell.
  rows <- expression_projection_selected_rows()
  if (!length(rows)) {
    return(prepareEmptyTable(dplyr::slice(
      viewerProjectionFirstFrameMetadata(),
      0
    )))
  }
  cells <- expression_projection_cells_to_show()[rows]
  cells_df <- bind_cols(
    expression_projection_coordinates()[rows, , drop = FALSE],
    viewerProjectionFirstFrameMetadata()[cells, , drop = FALSE]
  )
  if (identical(summary$kind, "mean")) {
    expression_columns <- "expression_level"
    cells_df[[expression_columns]] <- summary$series[[1]]$values[rows]
  } else {
    expression_columns <- make.unique(vapply(
      summary$series,
      `[[`,
      character(1),
      "label"
    ))
    for (i in seq_along(summary$series)) {
      cells_df[[expression_columns[[i]]]] <- summary$series[[i]]$values[rows]
    }
  }
  cells_df <- cells_df %>%
    dplyr::rename(X1 = 1, X2 = 2)
  ## Read canonical identity only for a requested, nonempty selection. The
  ## compact first-frame cell_barcode column may contain numeric placeholders.
  cells_df[["cell_barcode"]] <- viewerCellBarcodesAt(cells)
  cells_df <- cells_df %>%
    dplyr::select(
      -dplyr::any_of(c(
        "X1",
        "X2",
        "identifier",
        "selection_key"
      ))
    ) %>%
    dplyr::select(
      dplyr::any_of("cell_barcode"),
      dplyr::all_of(expression_columns),
      everything()
    )
  prettifyTable(
    cells_df,
    filter = list(position = "top", clear = TRUE),
    dom = "Brtlip",
    show_buttons = TRUE,
    number_formatting = input[[
      "expression_details_selected_cells_number_formatting"
    ]],
    color_highlighting = input[[
      "expression_details_selected_cells_color_highlighting"
    ]],
    hide_long_columns = TRUE,
    download_file_name = "expression_details_of_selected_cells"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "expression_details_selected_cells_info",
  expression_details_selected_cells_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_details_selected_cells_info <- list(
  title = "Details of selected cells",
  text = HTML(
    "
    Table containing expression values and meta data for cells selected with the box or lasso tool. Mean expression mode adds one averaged expression column; Separate panels and RGB modes add one column per gene or populated channel. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.
    <h4>Options</h4>
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <b>Highlight values with colours</b><br>
    Similar to the automatic formatting option, when active, CerebroNexus will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colours and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colours and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
