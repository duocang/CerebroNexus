##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
spatial_projection_parameters_plot <- reactive({
  req(
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    input[["spatial_projection_plot_type"]],
    input[["spatial_projection_point_size"]],
    input[["spatial_projection_point_opacity"]],
    !is.null(input[["spatial_projection_point_border"]]),
    !is.null(input[["spatial_projection_keep_square"]]),
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  plot_type <- input[["spatial_projection_plot_type"]]
  color_variable <- NULL
  feature_to_display <- NULL
  split_by <- ""
  spatial_name <- input[["spatial_projection_to_display"]]
  spatial_data <- getSpatialData(spatial_name)
  selected_roi <- input[["spatial_projection_roi"]] %||% "__all__"
  roi_value <- spatial_roi_value(selected_roi)

  if (plot_type == "ImageDimPlot") {
    color_variable <- input[["spatial_projection_point_color"]]
    req(color_variable, color_variable %in% colnames(getMetaData()))
    requested_split <- input[["spatial_projection_split_by"]] %||% ""
    split_cells <- rownames(spatial_data$coordinates) %||% character()
    selected_sample <- input[["spatial_projection_sample"]] %||% ""
    if (nzchar(selected_sample)) {
      sample_facet <- spatial_metadata_facet(
        getMetaData(),
        split_cells,
        c("sample", "sample_id", "orig.ident")
      )
      split_cells <- names(sample_facet$by_cell)[
        !is.na(sample_facet$by_cell) &
          sample_facet$by_cell == selected_sample
      ]
    }
    split_columns <- spatial_split_columns(
      getMetaData(),
      split_cells,
      getGroups()
    )
    if (requested_split %in% split_columns) {
      split_by <- requested_split
    }
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
    color_variable <- colnames(getMetaData())[1]
  }

  separate_roi_values <- character()
  if (identical(selected_roi, "__separate__")) {
    roi_facet <- spatial_metadata_facet(
      getMetaData(),
      rownames(spatial_data$coordinates) %||% character(),
      c("sample_roi", "roi", "roi_id", "region_of_interest")
    )
    if (!is.null(roi_facet$field)) {
      split_by <- roi_facet$field
    }
    separate_roi_values <- roi_facet$values
  }

  n_dimensions <- ncol(spatial_data$coordinates)
  dataset <- spatial_dataset_name(
    if (exists("available_crb_files")) available_crb_files$files else NULL,
    if (exists("available_crb_files")) available_crb_files$selected else NULL
  )
  roi_backgrounds <- list()
  if (identical(selected_roi, "__separate__")) {
    groups <- spatial_roi_background_groups(
      spatial_data,
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      spatial_name,
      separate_roi_values
    )
    selections <- spatial_roi_background_selections(
      groups,
      input[["spatial_projection_roi_background_images"]]
    )
    roi_backgrounds <- Map(
      function(group, selected) {
        descriptor <- resolve_spatial_background(
          selected,
          group$embedded,
          group$external
        )
        identity <- spatial_background_identity(
          dataset,
          spatial_name,
          descriptor
        )
        if (!is.null(identity)) {
          identity$roi <- group$roi
        }
        list(
          descriptor = descriptor,
          identity = identity,
          preset = spatial_background_preset(
            if (exists("Cerebro.options")) Cerebro.options else NULL,
            dataset,
            spatial_name,
            descriptor
          ),
          image_allowlist = vapply(
            group$external,
            `[[`,
            character(1),
            "path"
          )
        )
      },
      groups,
      selections
    )
    names(roi_backgrounds) <- names(groups)
    embedded_images <- list()
    configured_background_images <- list()
    background_image <- "none"
    background_descriptor <- NULL
  } else {
    embedded_images <- embedded_spatial_images(spatial_data, roi_value)
    configured_background_images <- configured_spatial_images(
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      spatial_name,
      roi_value
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
  }
  background_identity <- spatial_background_identity(
    dataset,
    spatial_name,
    background_descriptor
  )
  background_preset <- spatial_background_preset(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name,
    background_descriptor
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
    roi_selection = selected_roi,
    roi_mode = if (identical(selected_roi, "__separate__")) {
      "separate"
    } else if (spatial_roi_is_specific(selected_roi)) {
      "single"
    } else {
      "all"
    },
    n_dimensions = n_dimensions,
    color_variable = color_variable,
    plot_type = plot_type,
    split_by = split_by,
    roi_order = separate_roi_values,
    feature_to_display = feature_to_display,
    coexpr_r = input[["spatial_projection_coexpr_r"]],
    coexpr_g = input[["spatial_projection_coexpr_g"]],
    coexpr_b = input[["spatial_projection_coexpr_b"]],
    point_size = input[["spatial_projection_point_size"]],
    point_opacity = input[["spatial_projection_point_opacity"]],
    draw_border = input[["spatial_projection_point_border"]],
    group_labels = isTRUE(input[["spatial_projection_group_labels"]]),
    keep_square = isTRUE(input[["spatial_projection_keep_square"]]),
    show_region_outlines = isTRUE(
      input[["spatial_projection_show_region_outlines"]]
    ),
    show_cell_boundaries = isTRUE(
      input[["spatial_projection_show_cell_boundaries"]]
    ),
    show_molecules = isTRUE(input[["spatial_projection_show_molecules"]]),
    molecule_gene = input[["spatial_projection_molecule_gene"]],
    x_range = NULL,
    y_range = NULL,
    background_image = background_image,
    background_descriptor = background_descriptor,
    roi_backgrounds = roi_backgrounds,
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
