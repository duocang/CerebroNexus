##----------------------------------------------------------------------------##
## Point size / opacity and "Show % of cells" controls of a scatter page, and
## their info box. Overview and Gene expression both source this file.
## `data_control()` adds a control before the "% of cells" slider and
## `info_item` a line before the others in the info box.
##----------------------------------------------------------------------------##
viewerScatterParametersUI <- function(
  prefix,
  info_title,
  data_control = function() NULL,
  info_item = ""
) {
  appearance_id <- paste0(prefix, "_additional_parameters_UI")
  data_id <- paste0(prefix, "_data_parameters_UI")
  output[[appearance_id]] <- renderUI({
    viewer_requested_dataset_context()
    appearance <- isolate(current_scatter_defaults())

    tagList(
      viewerLimitedSlider(
        paste0(prefix, "_point_size"),
        label = "Point size",
        limits = preferences[["cell_point_size"]],
        value = appearance$point_size
      ),
      viewerLimitedSlider(
        paste0(prefix, "_point_opacity"),
        label = "Point opacity",
        limits = preferences[["cell_point_opacity"]],
        value = appearance$point_opacity
      )
    )
  })
  output[[data_id]] <- renderUI({
    viewer_requested_dataset_context()
    appearance <- isolate(current_scatter_defaults())

    tagList(
      data_control(),
      viewerLimitedSlider(
        paste0(prefix, "_percentage_cells_to_show"),
        label = "Show % of cells",
        limits = preferences[["cell_percentage_cells_to_show"]],
        value = appearance$percentage_cells_to_show
      )
    )
  })
  ## Keep the controls available while the settings drawer is hidden.
  outputOptions(output, appearance_id, suspendWhenHidden = FALSE)
  outputOptions(output, data_id, suspendWhenHidden = FALSE)

  cerebroRegisterInfo(
    input,
    paste0(prefix, "_additional_parameters_info"),
    list(
      title = info_title,
      text = HTML(paste0(
        "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>",
        info_item,
        "
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
      ))
    )
  )
}
