##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_additional_parameters_UI"]] <- renderUI({
  viewer_requested_dataset_context()
  appearance <- isolate(current_scatter_defaults())

  tagList(
    viewerLimitedSlider(
      "overview_projection_point_size",
      label = "Point size",
      limits = preferences[["cell_point_size"]],
      value = appearance$point_size
    ),
    viewerLimitedSlider(
      "overview_projection_point_opacity",
      label = "Point opacity",
      limits = preferences[["cell_point_opacity"]],
      value = appearance$point_opacity
    )
  )
})

output[["overview_projection_data_parameters_UI"]] <- renderUI({
  viewer_requested_dataset_context()
  appearance <- isolate(current_scatter_defaults())

  tagList(
    viewerLimitedSlider(
      "overview_projection_percentage_cells_to_show",
      label = "Show % of cells",
      limits = preferences[["cell_percentage_cells_to_show"]],
      value = appearance$percentage_cells_to_show
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "overview_projection_data_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "overview_projection_additional_parameters_info",
  overview_projection_additional_parameters_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
overview_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)
