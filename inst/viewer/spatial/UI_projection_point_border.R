##----------------------------------------------------------------------------##
## UI elements with switch to draw border around cells.
##----------------------------------------------------------------------------##
output[["spatial_projection_point_border_UI"]] <- renderUI({
  req(input[["spatial_projection_more_render_request"]])
  checkboxInput(
    inputId = "spatial_projection_point_border",
    label = "Draw border around cells",
    value = isolate(spatial_projection_appearance$draw_border)
  )
})

output[["spatial_projection_keep_square_UI"]] <- renderUI({
  req(input[["spatial_projection_more_render_request"]])
  checkboxInput(
    inputId = "spatial_projection_keep_square",
    label = "Keep plots square",
    value = isolate(spatial_projection_appearance$keep_square)
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "spatial_projection_point_border_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "spatial_projection_keep_square_UI",
  suspendWhenHidden = FALSE
)
