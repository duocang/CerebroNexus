##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
## Hidden Settings inputs bind after the first frame. Keep their defaults in
## server state so that mounting the drawer cannot invalidate the million-cell
## data reactive. Labels, borders and square layout are browser-only appearance
## patches; region outlines remain reactive because they require hull geometry.
spatial_projection_appearance <- reactiveValues(
  group_labels = TRUE,
  draw_border = FALSE,
  keep_square = FALSE
)
spatial_projection_region_outlines <- reactiveVal(FALSE)

spatial_update_appearance <- function(name, value) {
  if (identical(spatial_projection_appearance[[name]], value)) {
    return(FALSE)
  }
  spatial_projection_appearance[[name]] <- value
  TRUE
}

spatial_send_appearance <- function(values) {
  identity <- viewerDatasetIdentity()
  req(identity$fingerprint)
  session$sendCustomMessage(
    "cell_view_appearance",
    list(
      id = "spatial_projection",
      dataset_fingerprint = identity$fingerprint,
      values = values
    )
  )
}

observeEvent(input[["spatial_projection_group_labels"]], {
  value <- isTRUE(input[["spatial_projection_group_labels"]])
  if (spatial_update_appearance("group_labels", value)) {
    spatial_send_appearance(list(group_labels = value))
  }
}, ignoreNULL = TRUE)

observeEvent(input[["spatial_projection_point_border"]], {
  value <- isTRUE(input[["spatial_projection_point_border"]])
  if (spatial_update_appearance("draw_border", value)) {
    spatial_send_appearance(list(draw_border = value))
  }
}, ignoreNULL = TRUE)

observeEvent(input[["spatial_projection_keep_square"]], {
  value <- isTRUE(input[["spatial_projection_keep_square"]])
  if (spatial_update_appearance("keep_square", value)) {
    spatial_send_appearance(list(keep_square = value))
  }
}, ignoreNULL = TRUE)

observeEvent(input[["spatial_projection_show_region_outlines"]], {
  value <- isTRUE(input[["spatial_projection_show_region_outlines"]])
  if (!identical(spatial_projection_region_outlines(), value)) {
    spatial_projection_region_outlines(value)
  }
}, ignoreNULL = TRUE)

spatial_projection_parameters_plot <- reactive({
  req(
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    input[["spatial_projection_plot_type"]],
    input[["spatial_projection_point_size"]],
    input[["spatial_projection_point_opacity"]],
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  plot_type <- input[["spatial_projection_plot_type"]]
  color_variable <- NULL
  feature_to_display <- NULL

  if (plot_type == "ImageDimPlot") {
    color_variable <- input[["spatial_projection_point_color"]]
    req(
      color_variable,
      color_variable %in% colnames(viewerProjectionFirstFrameMetadata())
    )
  } else if (plot_type == "ImageFeaturePlot") {
    feature_to_display <- input[["spatial_projection_feature_to_display"]]
    req(feature_to_display)
    color_variable <- feature_to_display
  } else if (plot_type == "Co-expression (RGB)") {
    ## One gene per channel; any channel may be empty. Require at least one so
    ## the render has something to colour by.
    coexpr_genes <- list(
      r = input[["spatial_projection_coexpr_r"]],
      g = input[["spatial_projection_coexpr_g"]],
      b = input[["spatial_projection_coexpr_b"]]
    )
    blank <- function(x) is.null(x) || !nzchar(x)
    req(
      !(blank(coexpr_genes$r) && blank(coexpr_genes$g) && blank(coexpr_genes$b))
    )
    ## The cell colour comes from the RGB blend, not a metadata column, but the
    ## downstream code still indexes metadata[[color_variable]] — give it a valid
    ## placeholder column so that lookup can't fail on a NULL name.
    color_variable <- "cell_index"
  }

  spatial_data <- getSpatialData(input[["spatial_projection_to_display"]])
  n_dimensions <- ncol(spatial_data$coordinates)
  spatial_name <- input[["spatial_projection_to_display"]]
  dataset <- spatial_dataset_name(
    if (exists("available_crb_files")) available_crb_files$files else NULL,
    if (exists("available_crb_files")) available_crb_files$selected else NULL
  )
  embedded_images <- embedded_spatial_images(spatial_data)
  configured_background_images <- configured_spatial_images(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name
  )
  background_choices <- spatial_background_choices(
    embedded_images,
    configured_background_images
  )
  background_image <- normalize_spatial_background_choice(
    input[["spatial_projection_background_image"]],
    background_choices
  )
  background_descriptor <- resolve_spatial_background(
    background_image,
    embedded_images,
    configured_background_images
  )
  background_identity <- spatial_background_identity(
    dataset,
    spatial_name,
    background_descriptor
  )
  image_label <- if (is.null(background_descriptor)) {
    NULL
  } else {
    background_descriptor$label
  }
  background_preset <- spatialImagePreset(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name,
    image_label
  )
  ## Interaction changes travel through the decoupled background observer. Only
  ## isolate the current value here so the first payload starts at the preset
  ## even if the dynamic control has not been created yet.
  background_opacity <- isolate(
    input[["spatial_projection_background_opacity"]]
  )
  if (is.null(background_opacity)) {
    background_opacity <- background_preset$opacity
  }

  parameters <- list(
    projection = input[["spatial_projection_to_display"]],
    n_dimensions = n_dimensions,
    color_variable = color_variable,
    plot_type = plot_type,
    feature_to_display = feature_to_display,
    coexpr_r = input[["spatial_projection_coexpr_r"]],
    coexpr_g = input[["spatial_projection_coexpr_g"]],
    coexpr_b = input[["spatial_projection_coexpr_b"]],
    point_size = input[["spatial_projection_point_size"]],
    point_opacity = input[["spatial_projection_point_opacity"]],
    draw_border = isolate(spatial_projection_appearance$draw_border),
    group_labels = isolate(spatial_projection_appearance$group_labels),
    keep_square = isolate(spatial_projection_appearance$keep_square),
    show_region_outlines = spatial_projection_region_outlines(),
    x_range = NULL,
    y_range = NULL,
    background_image = background_image,
    background_descriptor = background_descriptor,
    background_identity = background_identity,
    background_image_allowlist = vapply(
      configured_background_images,
      `[[`,
      character(1),
      "path"
    ),
    background_flip_x = background_preset$flipX,
    background_flip_y = background_preset$flipY,
    background_scale_x = background_preset$scaleX,
    background_scale_y = background_preset$scaleY,
    background_offset_x = background_preset$offsetX,
    background_offset_y = background_preset$offsetY,
    background_rotation = background_preset$rotation,
    background_opacity = background_opacity,
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

##
spatial_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['spatial_projection_to_display']], {
  spatial_projection_parameters_other[['reset_axes']] <- TRUE
})
