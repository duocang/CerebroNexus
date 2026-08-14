## Spatial/Trekker alignment observers kept out of app.R so the workbench has
## one explicit boundary: bounded coordinate models enter, canonical alignment
## records leave. The Seurat object and local upload paths never enter state.

builder_spatial_alignment_server <- function(
  input,
  output,
  session,
  current,
  entry_of,
  worker,
  enqueue,
  commit_images,
  alignment_preview,
  spatial_coords,
  encode_image = builder_encode_image
) {
  raw_image <- shiny::reactiveVal(NULL)
  draft <- shiny::reactiveVal(NULL)
  display_record <- shiny::reactiveVal(NULL)
  baseline <- shiny::reactiveVal(NULL)
  coordinate_draft <- shiny::reactiveVal(list(rotation_degrees = 0, scale = 1))
  active_section <- shiny::reactiveVal(NULL)
  active_image <- shiny::reactiveVal(NULL)
  pending_section <- shiny::reactiveVal(NULL)
  pending_image <- shiny::reactiveVal(NULL)
  pending_upload <- shiny::reactiveVal(NULL)
  preview_contract <- shiny::reactiveVal(NULL)
  expected_controls <- shiny::reactiveVal(NULL)
  image_collection_cache <- new.env(parent = emptyenv())

  output[["enhance-has_image"]] <- shiny::reactive(!is.null(draft()))
  output[["enhance-add_image_label"]] <- shiny::renderUI({
    if (is.null(draft())) {
      "+ Add image"
    } else {
      "+ Add another image"
    }
  })
  output[["enhance-has_multiple_images"]] <- shiny::reactive({
    entry <- entry_of(current())
    section <- active_section()
    !is.null(entry) &&
      !is.null(section) &&
      length(image_labels_for(entry, section)) > 1L
  })
  output[["enhance-has_coordinate_frame"]] <- shiny::reactive({
    section <- active_section()
    !is.null(section) && identical(kind_for(section), "spatial")
  })
  shiny::outputOptions(
    output,
    "enhance-has_image",
    suspendWhenHidden = FALSE
  )
  shiny::outputOptions(
    output,
    "enhance-has_multiple_images",
    suspendWhenHidden = FALSE
  )
  shiny::outputOptions(
    output,
    "enhance-has_coordinate_frame",
    suspendWhenHidden = FALSE
  )

  sections_for <- function(entry) {
    extras <- entry$profile$extras %||% list()
    has_trekker <- any(vapply(
      extras,
      function(item) {
        identical(item$key %||% "", "trekker") && isTRUE(item$found)
      },
      logical(1)
    ))
    unique(c(
      entry$profile$images %||% character(),
      if (has_trekker) "trekker" else character()
    ))
  }
  kind_for <- function(section) {
    if (identical(section, "trekker")) "trekker" else "spatial"
  }
  collection_for <- function(entry) {
    stored <- builder_image_collection_normalize(
      entry$settings$images %||% list()
    )
    if (
      exists("dataset", image_collection_cache, inherits = FALSE) &&
        identical(image_collection_cache$dataset, entry$id)
    ) {
      return(builder_image_collection_normalize(utils::modifyList(
        stored,
        image_collection_cache$images
      )))
    }
    stored
  }
  image_labels_for <- function(entry, section) {
    names(collection_for(entry)[[section]] %||% list()) %||% character()
  }
  update_image_choices <- function(entry, section, selected = NULL) {
    choices <- image_labels_for(entry, section)
    shiny::updateSelectInput(
      session,
      "enhance-active_image",
      choices = choices,
      selected = selected %||%
        if (length(choices)) choices[[1L]] else character()
    )
    invisible(choices)
  }
  commit_section <- function(entry, section, value, label = active_image()) {
    images <- collection_for(entry)
    if (is.null(label) && !is.null(value)) {
      label <- builder_safe_file_name(
        value$source$name %||% "Tissue image",
        fallback = "Tissue image"
      )
    }
    if (!is.null(label)) {
      if (is.null(value)) {
        images <- builder_image_collection_remove(images, section, label)
      } else {
        images[[section]][[label]] <- value
      }
    }
    commit_images(entry, images)
    image_collection_cache$dataset <- entry$id
    image_collection_cache$images <- images
    image_collection_cache$known_sections <- unique(c(
      image_collection_cache$known_sections %||% character(),
      section,
      names(images)
    ))
    invisible(images)
  }
  coordinate_transforms_for <- function(entry) {
    transforms <- entry$settings$spatial_coordinate_transforms %||% list()
    if (is.null(names(transforms))) {
      return(list())
    }
    transforms <- lapply(transforms, function(transform) {
      if (is.list(transform)) {
        transform$scale <- 1
      }
      transform
    })
    transforms
  }
  coordinate_spec_for <- function(entry, section) {
    if (!identical(kind_for(section), "spatial")) {
      return(list(rotation_degrees = 0, scale = 1))
    }
    stored <- coordinate_transforms_for(entry)[[section]]
    .spx_coordinate_transform_spec_normalize(
      stored,
      context = paste0("spatial_coordinate_transforms$", section)
    )
  }
  coordinate_preview_transforms <- function(entry, section) {
    transforms <- coordinate_transforms_for(entry)
    if (!identical(kind_for(section), "spatial")) {
      transforms[[section]] <- NULL
    }
    transforms
  }
  preview_contract_for <- function(entry, section) {
    list(
      dataset = entry$id,
      snapshot_identity = .builder_worker_identity(entry$snapshot),
      section = section,
      default_projection = entry$settings$default_projection %||% NULL,
      group = entry$settings$default_group %||% NULL,
      assay = entry$settings$assay %||% NULL,
      layer = entry$settings$layer %||% "data",
      coordinate_transforms = coordinate_preview_transforms(entry, section)
    )
  }
  request_preview <- function(entry, section) {
    alignment_preview(NULL)
    queued <- enqueue(list(
      kind = "spatial_preview",
      id = entry$id,
      section = section,
      default_projection = entry$settings$default_projection %||% NULL,
      group = entry$settings$default_group %||% NULL,
      assay = entry$settings$assay %||% NULL,
      layer = entry$settings$layer %||% "data",
      coordinate_transforms = coordinate_preview_transforms(entry, section),
      replaces = "spatial_alignment",
      note = paste0("Loading paired views for ", section, "…")
    ))
    if (isTRUE(queued)) {
      preview_contract(preview_contract_for(entry, section))
    }
    invisible(queued)
  }
  update_controls <- function(record = NULL, bounds = NULL) {
    parameters <- if (is.null(record)) {
      builder_alignment_defaults()
    } else {
      .builder_alignment_parameters(record)
    }
    expected_controls(parameters)
    span_x <- if (.builder_alignment_valid_bounds(bounds)) {
      bounds$xmax - bounds$xmin
    } else {
      1
    }
    span_y <- if (.builder_alignment_valid_bounds(bounds)) {
      bounds$ymax - bounds$ymin
    } else {
      1
    }
    nice <- function(value) max(signif(value, 2), .Machine$double.eps)
    ids <- c(
      "enhance-img_dx",
      "enhance-img_dy",
      "enhance-img_scale",
      "enhance-img_rotate",
      "enhance-image_flip_x",
      "enhance-image_flip_y",
      "enhance-image_opacity",
      "enhance-point_opacity",
      "enhance-point_size"
    )
    invisible(lapply(ids, function(id) shiny::freezeReactiveValue(input, id)))
    shiny::updateSliderInput(
      session,
      "enhance-img_dx",
      min = -nice(span_x),
      max = nice(span_x),
      value = parameters$dx,
      step = nice(span_x / 200)
    )
    shiny::updateSliderInput(
      session,
      "enhance-img_dy",
      min = -nice(span_y),
      max = nice(span_y),
      value = parameters$dy,
      step = nice(span_y / 200)
    )
    shiny::updateSliderInput(
      session,
      "enhance-img_scale",
      value = parameters$scale
    )
    shiny::updateSliderInput(
      session,
      "enhance-img_rotate",
      value = parameters$rotation
    )
    shiny::updateCheckboxInput(
      session,
      "enhance-image_flip_x",
      value = parameters$flip_x
    )
    shiny::updateCheckboxInput(
      session,
      "enhance-image_flip_y",
      value = parameters$flip_y
    )
    shiny::updateSliderInput(
      session,
      "enhance-image_opacity",
      value = parameters$image_opacity * 100
    )
    shiny::updateSliderInput(
      session,
      "enhance-point_opacity",
      value = parameters$point_opacity * 100
    )
    shiny::updateSliderInput(
      session,
      "enhance-point_size",
      value = parameters$point_size
    )
  }
  restore <- function(entry, section, label = active_image()) {
    stored <- if (is.null(label)) {
      NULL
    } else {
      builder_alignment_normalize(
        collection_for(entry)[[section]][[label]],
        section_id = section,
        section_kind = kind_for(section)
      )
    }
    draft(stored)
    display_record(stored)
    baseline(if (!is.null(stored) && isTRUE(stored$saved)) stored else NULL)
    if (is.null(stored)) {
      raw_image(NULL)
    } else {
      image <- builder_read_image_uri(stored$source_uri)
      if (!is.null(image$error)) {
        raw_image(NULL)
        shiny::showNotification(image$error, type = "error", duration = 8)
      } else {
        raw_image(image)
      }
    }
    update_controls(stored, alignment_preview()$bounds %||% NULL)
  }
  restore_coordinate_controls <- function(entry, section) {
    spec <- coordinate_spec_for(entry, section)
    coordinate_draft(spec)
    shiny::freezeReactiveValue(input, "enhance-coordinate_rotation")
    shiny::updateSliderInput(
      session,
      "enhance-coordinate_rotation",
      value = spec$rotation_degrees
    )
  }
  switch_to <- function(entry, section, label = NULL) {
    pending_upload(NULL)
    active_section(section)
    shiny::updateSelectInput(
      session,
      "enhance-active_section",
      selected = section
    )
    ## Saving/restoring alignment replaces the outer Configure UI. The client
    ## remembers this authoritative value across that redraw and re-applies it
    ## to the replacement select without generating a second server event.
    session$sendCustomMessage(
      "builder_spatial_section_state",
      list(value = section)
    )
    labels <- update_image_choices(entry, section, selected = label)
    label <- if (!is.null(label) && label %in% labels) {
      label
    } else {
      if (length(labels)) labels[[1L]] else NULL
    }
    active_image(label)
    restore(entry, section, label)
    restore_coordinate_controls(entry, section)
    request_preview(entry, section)
  }

  shiny::observeEvent(current(), {
    raw_image(NULL)
    draft(NULL)
    display_record(NULL)
    baseline(NULL)
    coordinate_draft(list(rotation_degrees = 0, scale = 1))
    alignment_preview(NULL)
    spatial_coords(NULL)
    preview_contract(NULL)
    pending_upload(NULL)
    pending_section(NULL)
    pending_image(NULL)
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    if (is.null(entry)) {
      rm(list = ls(image_collection_cache), envir = image_collection_cache)
    } else if (
      !exists("dataset", image_collection_cache, inherits = FALSE) ||
        !identical(image_collection_cache$dataset, entry$id)
    ) {
      image_collection_cache$dataset <- entry$id
      image_collection_cache$images <- builder_image_collection_normalize(
        entry$settings$images %||% list()
      )
    }
    sections <- if (is.null(entry)) character() else sections_for(entry)
    if (!length(sections)) {
      active_section(NULL)
      active_image(NULL)
      return()
    }
    switch_to(entry, sections[[1L]])
  })

  shiny::observe({
    current_worker <- worker()
    id <- current()
    section <- active_section()
    if (is.null(current_worker) || is.null(id) || is.null(section)) {
      return()
    }
    entry <- entry_of(id)
    if (is.null(entry)) {
      return()
    }
    contract <- preview_contract_for(entry, section)
    if (identical(contract, shiny::isolate(preview_contract()))) {
      return()
    }
    request_preview(entry, section)
  })

  parameters <- shiny::reactive({
    current_draft <- draft()
    defaults <- if (is.null(current_draft)) {
      builder_alignment_defaults()
    } else {
      current_draft
    }
    .builder_alignment_parameters(list(
      dx = input[["enhance-img_dx"]] %||% defaults$dx,
      dy = input[["enhance-img_dy"]] %||% defaults$dy,
      scale = input[["enhance-img_scale"]] %||% defaults$scale,
      rotation = input[["enhance-img_rotate"]] %||% defaults$rotation,
      flip_x = input[["enhance-image_flip_x"]] %||% defaults$flip_x,
      flip_y = input[["enhance-image_flip_y"]] %||% defaults$flip_y,
      image_opacity = (input[["enhance-image_opacity"]] %||%
        (defaults$image_opacity * 100)) /
        100,
      point_opacity = (input[["enhance-point_opacity"]] %||%
        (defaults$point_opacity * 100)) /
        100,
      point_size = input[["enhance-point_size"]] %||% defaults$point_size
    ))
  })
  coordinate_parameters <- shiny::reactive({
    current_spec <- coordinate_draft()
    .spx_coordinate_transform_spec_normalize(
      list(
        rotation_degrees = input[["enhance-coordinate_rotation"]] %||%
          current_spec$rotation_degrees,
        scale = 1
      ),
      context = "Coordinate frame"
    )
  })
  shiny::observe({
    rotation <- input[["enhance-coordinate_rotation"]]
    if (is.null(rotation)) {
      return()
    }
    section <- active_section()
    if (is.null(section) || !identical(kind_for(section), "spatial")) {
      return()
    }
    coordinate_draft(coordinate_parameters())
  })
  finalize_record <- function(current_draft, image, preview = NULL) {
    if (is.null(image) || is.null(current_draft)) {
      return(NULL)
    }
    record_parameters <- .builder_alignment_parameters(current_draft)
    current_encoded <- encode_image(
      image$array,
      max_px = 1400,
      flip_y = record_parameters$flip_y,
      flip_x = record_parameters$flip_x,
      rotate = record_parameters$rotation
    )
    if (!is.null(current_encoded$error)) {
      return(current_encoded)
    }
    record <- builder_alignment_record(
      source = current_draft$source,
      source_uri = current_draft$source_uri,
      uri = current_encoded$uri,
      base_bounds = current_draft$base_bounds,
      parameters = record_parameters,
      image_geometry = current_encoded,
      saved = FALSE,
      section = list(
        id = current_draft$section_id,
        kind = current_draft$section_kind
      )
    )
    facts <- intersect(
      names(current_encoded),
      c(
        "bytes",
        "width",
        "height",
        "source_width",
        "source_height",
        "extent_width",
        "extent_height",
        "display_width",
        "display_height"
      )
    )
    record[facts] <- current_encoded[facts]
    if (isTRUE(preview$available)) {
      display_frame <- builder_alignment_apply_coordinate_provenance(
        preview$spatial,
        preview$coordinate_transform %||% NULL
      )
      cover <- builder_bounds_cover(
        record$bounds,
        list(display_frame$x, display_frame$y)
      )
      record$outside <- cover$outside
      record$total <- cover$total
    } else {
      record$outside <- current_draft$outside %||% NA_integer_
      record$total <- current_draft$total %||% NA_integer_
    }
    record
  }
  finalize_current_record <- function() {
    image <- shiny::isolate(raw_image())
    current_draft <- shiny::isolate(draft())
    preview <- shiny::isolate(alignment_preview())
    if (!isTRUE(preview$available)) {
      return(NULL)
    }
    current_draft[names(shiny::isolate(parameters()))] <-
      shiny::isolate(parameters())
    finalize_record(current_draft, image, preview)
  }

  shiny::observeEvent(input[["enhance-active_section"]], {
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    section <- input[["enhance-active_section"]]
    previous <- shiny::isolate(active_section())
    if (is.null(entry) || !nzchar(section) || identical(section, previous)) {
      return()
    }
    current_draft <- shiny::isolate(draft())
    if (!is.null(current_draft) && !isTRUE(current_draft$saved)) {
      pending_section(section)
      shiny::updateSelectInput(
        session,
        "enhance-active_section",
        selected = previous
      )
      shiny::showModal(shiny::modalDialog(
        title = "Save alignment changes?",
        shiny::p(paste0(
          "The tissue image for “",
          previous,
          "” has unsaved alignment changes."
        )),
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::actionButton("enhance-alignment_switch_cancel", "Cancel"),
          shiny::actionButton(
            "enhance-alignment_switch_discard",
            "Discard changes",
            class = "btn btn-remove-soft"
          ),
          shiny::actionButton(
            "enhance-alignment_switch_save",
            "Save and continue",
            class = "btn btn-action"
          )
        )
      ))
      return()
    }
    switch_to(entry, section)
  })

  shiny::observeEvent(input[["enhance-active_image"]], {
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    label <- input[["enhance-active_image"]]
    previous <- shiny::isolate(active_image())
    section <- shiny::isolate(active_section())
    if (
      is.null(entry) ||
        is.null(section) ||
        !nzchar(label) ||
        identical(label, previous)
    ) {
      return()
    }
    current_draft <- shiny::isolate(draft())
    if (!is.null(current_draft) && !isTRUE(current_draft$saved)) {
      pending_image(label)
      shiny::updateSelectInput(
        session,
        "enhance-active_image",
        selected = previous
      )
      shiny::showModal(shiny::modalDialog(
        title = "Save alignment changes?",
        shiny::p(paste0(
          "The image “",
          previous,
          "” in “",
          section,
          "” has unsaved alignment changes."
        )),
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::actionButton("enhance-alignment_switch_cancel", "Cancel"),
          shiny::actionButton(
            "enhance-alignment_switch_discard",
            "Discard changes",
            class = "btn btn-remove-soft"
          ),
          shiny::actionButton(
            "enhance-alignment_switch_save",
            "Save and continue",
            class = "btn btn-action"
          )
        )
      ))
      return()
    }
    active_image(label)
    restore(entry, section, label)
  })

  attach_upload <- function(upload, preview, label = NULL) {
    filename <- basename(as.character(upload$name[[1L]]))
    proposed_label <- trimws(
      label %||%
        builder_safe_file_name(
          filename,
          fallback = "Tissue image"
        )
    )
    entry <- entry_of(current())
    section <- active_section()
    existing <- image_labels_for(entry, section)
    if (!nzchar(proposed_label) || proposed_label %in% existing) {
      pending_upload(list(
        upload = upload,
        dataset = current(),
        snapshot_identity = .builder_worker_identity(entry$snapshot),
        section = section,
        preview = preview,
        awaiting_label = TRUE
      ))
      shiny::showModal(shiny::modalDialog(
        title = "Name this image",
        shiny::textInput(
          "enhance-new_image_label",
          "Image label",
          value = if (nzchar(proposed_label)) proposed_label else ""
        ),
        shiny::p(
          class = "hint",
          "Image labels must be unique within this section."
        ),
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::actionButton("enhance-add_image_cancel", "Cancel"),
          shiny::actionButton(
            "enhance-add_image_confirm",
            "Add image",
            class = "btn btn-action"
          )
        )
      ))
      return(invisible(FALSE))
    }
    image <- builder_read_image(upload$datapath[[1L]], filename = filename)
    if (!is.null(image$error)) {
      shiny::showNotification(image$error, type = "error", duration = 8)
      return(invisible(FALSE))
    }
    image_encoded <- encode_image(image$array, max_px = 1400)
    if (!is.null(image_encoded$error)) {
      shiny::showNotification(image_encoded$error, type = "error", duration = 8)
      return(invisible(FALSE))
    }
    previous_label <- active_image()
    previous <- if (is.null(previous_label)) {
      NULL
    } else {
      builder_alignment_normalize(
        collection_for(entry)[[section]][[previous_label]],
        section,
        preview$section$kind
      )
    }
    baseline(
      if (!is.null(previous) && isTRUE(previous$saved)) previous else NULL
    )
    record <- builder_alignment_record(
      source = list(
        name = filename,
        type = if ("type" %in% names(upload)) {
          as.character(upload$type[[1L]] %||% "")
        } else {
          ""
        },
        size = if ("size" %in% names(upload)) {
          suppressWarnings(as.numeric(upload$size[[1L]]))
        } else {
          NA_real_
        }
      ),
      source_uri = image_encoded$uri,
      uri = image_encoded$uri,
      base_bounds = builder_alignment_fit_bounds(
        preview$bounds,
        c(
          width = image_encoded$source_width,
          height = image_encoded$source_height
        )
      ),
      parameters = builder_alignment_defaults(),
      saved = FALSE,
      section = preview$section
    )
    facts <- intersect(
      names(image_encoded),
      c(
        "bytes",
        "width",
        "height",
        "source_width",
        "source_height",
        "extent_width",
        "extent_height",
        "display_width",
        "display_height"
      )
    )
    record[facts] <- image_encoded[facts]
    raw_image(image)
    draft(record)
    display_record(record)
    active_image(proposed_label)
    update_controls(record, preview$bounds)
    committed_images <- commit_section(
      entry,
      section,
      record,
      label = proposed_label
    )
    entry$settings$images <- committed_images
    update_image_choices(entry, section, selected = proposed_label)
    invisible(TRUE)
  }

  shiny::observeEvent(input[["enhance-tissue_image_file"]], {
    upload <- input[["enhance-tissue_image_file"]]
    if (
      !is.data.frame(upload) ||
        !nrow(upload) ||
        !all(c("name", "datapath") %in% names(upload))
    ) {
      return()
    }
    entry <- entry_of(current())
    if (is.null(entry)) {
      return()
    }
    pending_upload(list(
      upload = upload,
      dataset = current(),
      snapshot_identity = .builder_worker_identity(entry$snapshot),
      section = active_section()
    ))
  })

  shiny::observe({
    pending <- pending_upload()
    if (is.null(pending)) {
      return()
    }
    if (isTRUE(pending$awaiting_label)) {
      return()
    }
    id <- current()
    section <- active_section()
    entry <- if (is.null(id)) NULL else entry_of(id)
    if (
      is.null(entry) ||
        !identical(pending$dataset, id) ||
        !identical(
          pending$snapshot_identity,
          .builder_worker_identity(entry$snapshot)
        ) ||
        !identical(pending$section, section)
    ) {
      pending_upload(NULL)
      return()
    }
    preview <- alignment_preview()
    if (is.null(preview)) {
      return()
    }
    if (
      !isTRUE(preview$available) ||
        !.builder_alignment_valid_bounds(preview$bounds)
    ) {
      pending_upload(NULL)
      shiny::showNotification(
        preview$message %||%
          "The spatial preview is not available for this tissue section.",
        type = "error",
        duration = 8
      )
      return()
    }
    pending_upload(NULL)
    attach_upload(pending$upload, preview)
  })

  shiny::observeEvent(input[["enhance-add_image_confirm"]], {
    pending <- shiny::isolate(pending_upload())
    if (is.null(pending) || !isTRUE(pending$awaiting_label)) {
      return()
    }
    label <- trimws(input[["enhance-new_image_label"]] %||% "")
    entry <- entry_of(current())
    if (
      is.null(entry) ||
        !identical(pending$dataset, current()) ||
        !identical(pending$section, active_section()) ||
        !identical(
          pending$snapshot_identity,
          .builder_worker_identity(entry$snapshot)
        ) ||
        !nzchar(label) ||
        label %in% image_labels_for(entry, active_section())
    ) {
      shiny::showNotification(
        "Image labels must be non-empty and unique within this section.",
        type = "error",
        duration = 5
      )
      return()
    }
    pending_upload(NULL)
    shiny::removeModal()
    attach_upload(pending$upload, pending$preview, label = label)
  })
  shiny::observeEvent(input[["enhance-add_image_cancel"]], {
    pending_upload(NULL)
    shiny::removeModal()
  })

  shiny::observeEvent(alignment_preview(), {
    preview <- alignment_preview()
    if (isTRUE(preview$available)) {
      update_controls(draft(), preview$bounds)
    }
  })

  colors <- shiny::reactive({
    preview <- alignment_preview()
    id <- current()
    entry <- if (is.null(id)) NULL else entry_of(id)
    if (!isTRUE(preview$available) || is.null(entry)) {
      return(character())
    }
    levels <- unique(as.character(preview$spatial$group))
    group <- entry$settings$default_group %||% ""
    overrides <- builder_settings_color_overrides(entry$settings)[[group]]
    builder_level_colors(
      levels,
      entry$settings$palette %||% "cerebro",
      overrides
    )
  })
  output[["enhance-alignment_spatial_plot"]] <- plotly::renderPlotly({
    preview <- alignment_preview()
    shiny::req(isTRUE(preview$available))
    record <- display_record()
    if (!is.null(record$error)) {
      record <- NULL
    }
    appearance <- .builder_alignment_parameters(record %||% list())
    display_frame <- builder_alignment_apply_coordinate_provenance(
      preview$spatial,
      preview$coordinate_transform %||% NULL
    )
    builder_alignment_plot(
      display_frame,
      colors(),
      image_uri = record$uri %||% NULL,
      image_bounds = record$bounds %||% NULL,
      image_preview = record,
      coordinate_frame = preview$coordinate_frame %||% NULL,
      coordinate_transform = preview$coordinate_transform %||% NULL,
      image_opacity = appearance$image_opacity,
      point_opacity = appearance$point_opacity,
      point_size = appearance$point_size
    )
  })

  output[["enhance-alignment_legend"]] <- shiny::renderUI({
    preview <- alignment_preview()
    shiny::req(isTRUE(preview$available))
    builder_alignment_legend_ui(preview$spatial, colors())
  })
  output[["enhance-alignment_projection_label"]] <- shiny::renderUI({
    preview <- alignment_preview()
    if (!isTRUE(preview$available)) {
      return(shiny::span(class = "hint", preview$message %||% "Loading…"))
    }
    shiny::span(class = "hint", paste("Projection:", preview$projection_name))
  })
  output[["enhance-alignment_spatial_label"]] <- shiny::renderUI({
    preview <- alignment_preview()
    if (!isTRUE(preview$available)) {
      return(NULL)
    }
    shiny::span(
      class = "hint",
      paste0(
        preview$section$unit,
        if (isTRUE(preview$capped)) " · bounded preview" else ""
      )
    )
  })
  output[["enhance-alignment_status"]] <- shiny::renderUI({
    preview <- alignment_preview()
    current_draft <- draft()
    if (!is.null(pending_upload()) && is.null(current_draft)) {
      return(shiny::div(
        class = "notice",
        "Image selected. Finishing the spatial preview…"
      ))
    }
    if (is.null(preview)) {
      return(shiny::div(class = "notice", "Loading paired cell views…"))
    }
    if (!isTRUE(preview$available)) {
      return(shiny::div(class = "notice bad", preview$message))
    }
    if (is.null(current_draft)) {
      return(NULL)
    }
    builder_tissue_image_file_ui("enhance", current_draft)
  })

  mark_unsaved <- function() {
    current_draft <- shiny::isolate(draft())
    if (is.null(current_draft)) {
      return(invisible(FALSE))
    }
    next_parameters <- shiny::isolate(parameters())
    current_parameters <- .builder_alignment_parameters(current_draft)
    if (
      isTRUE(all.equal(
        next_parameters,
        current_parameters,
        check.attributes = FALSE
      ))
    ) {
      return(invisible(FALSE))
    }
    expected_controls(NULL)
    if (isTRUE(current_draft$saved)) {
      baseline(current_draft)
    }
    next_record <- current_draft
    next_record[names(next_parameters)] <- next_parameters
    next_record$saved <- FALSE
    draft(next_record)
    entry <- shiny::isolate(entry_of(current()))
    section <- shiny::isolate(active_section())
    label <- shiny::isolate(active_image())
    if (!is.null(entry) && !is.null(section) && !is.null(label)) {
      images <- collection_for(entry)
      images[[section]][[label]] <- next_record
      image_collection_cache$dataset <- entry$id
      image_collection_cache$images <- images
      image_collection_cache$known_sections <- unique(c(
        image_collection_cache$known_sections %||% character(),
        section,
        names(images)
      ))
    }
    invisible(TRUE)
  }
  shiny::observeEvent(
    list(
      input[["enhance-img_dx"]],
      input[["enhance-img_dy"]],
      input[["enhance-img_scale"]],
      input[["enhance-img_rotate"]],
      input[["enhance-image_flip_x"]],
      input[["enhance-image_flip_y"]],
      input[["enhance-image_opacity"]],
      input[["enhance-point_opacity"]],
      input[["enhance-point_size"]]
    ),
    mark_unsaved(),
    ignoreInit = TRUE
  )

  save_current <- function(notify = TRUE) {
    entry <- shiny::isolate(entry_of(current()))
    section <- shiny::isolate(active_section())
    record <- finalize_current_record()
    if (is.null(entry) || is.null(section) || is.null(record)) {
      return(FALSE)
    }
    if (!is.null(record$error)) {
      shiny::showNotification(record$error, type = "error", duration = 8)
      return(FALSE)
    }
    outside <- builder_alignment_outside_count(record)
    if (is.na(outside)) {
      shiny::showNotification(
        "Image-coverage diagnostics are invalid. Re-open this section and align it again before saving.",
        type = "error",
        duration = 8
      )
      return(FALSE)
    }
    if (outside > 0L) {
      shiny::showNotification(
        paste0(
          outside,
          if (outside == 1L) " cell is" else " cells are",
          " outside the image bounds. Adjust the alignment before saving."
        ),
        type = "error",
        duration = 8
      )
      return(FALSE)
    }
    record$saved <- TRUE
    commit_section(entry, section, record)
    draft(record)
    display_record(record)
    baseline(record)
    if (isTRUE(notify)) {
      shiny::showNotification(
        paste0("Alignment saved for “", section, "”."),
        type = "message",
        duration = 4
      )
    }
    TRUE
  }
  shiny::observeEvent(input[["enhance-apply_align"]], save_current())
  save_coordinate_transform <- function(reset = FALSE) {
    entry <- shiny::isolate(entry_of(current()))
    section <- shiny::isolate(active_section())
    if (
      is.null(entry) ||
        is.null(section) ||
        !identical(kind_for(section), "spatial")
    ) {
      return(invisible(FALSE))
    }
    transforms <- coordinate_transforms_for(entry)
    previous <- transforms[[section]]
    spec <- shiny::isolate(coordinate_parameters())
    identity <- identical(spec$rotation_degrees, 0) && identical(spec$scale, 1)
    if (isTRUE(reset) || identity) {
      transforms[[section]] <- NULL
      spec <- list(rotation_degrees = 0, scale = 1)
    } else {
      transforms[[section]] <- spec
    }
    changed <- !identical(previous, transforms[[section]])
    coordinate_draft(spec)
    if (isTRUE(reset)) {
      shiny::freezeReactiveValue(input, "enhance-coordinate_rotation")
      shiny::updateSliderInput(
        session,
        "enhance-coordinate_rotation",
        value = 0
      )
    }
    if (!changed) {
      return(invisible(FALSE))
    }
    entry$settings$spatial_coordinate_transforms <- transforms
    images <- builder_image_collection_mark_section_unsaved(
      collection_for(entry),
      section
    )
    entry$settings$images <- images
    commit_images(entry, images)
    image_collection_cache$dataset <- entry$id
    image_collection_cache$images <- images
    image_collection_cache$known_sections <- unique(c(
      image_collection_cache$known_sections %||% character(),
      section,
      names(images)
    ))
    current_label <- active_image()
    if (
      !is.null(current_label) && !is.null(images[[section]][[current_label]])
    ) {
      invalidated <- images[[section]][[current_label]]
      draft(invalidated)
      display_record(invalidated)
      baseline(NULL)
    }
    request_preview(entry, section)
    shiny::showNotification(
      if (isTRUE(reset) || identity) {
        paste0("Coordinates reset for “", section, "”.")
      } else {
        paste0("Coordinate transform saved for “", section, "”.")
      },
      type = "message",
      duration = 4
    )
    invisible(TRUE)
  }
  shiny::observeEvent(
    input[["enhance-save_coordinate_transform"]],
    save_coordinate_transform()
  )
  shiny::observeEvent(
    input[["enhance-reset_coordinate_transform"]],
    save_coordinate_transform(reset = TRUE)
  )
  shiny::observeEvent(input[["enhance-reset_align"]], {
    current_draft <- draft()
    if (is.null(current_draft)) {
      return()
    }
    reset <- builder_alignment_reset(current_draft)
    draft(reset)
    display_record(reset)
    update_controls(reset, alignment_preview()$bounds %||% NULL)
    commit_section(entry_of(current()), active_section(), reset)
  })
  show_remove_image <- function() {
    entry <- entry_of(current())
    section <- active_section()
    label <- active_image()
    if (is.null(entry) || is.null(section) || is.null(label)) {
      return(invisible(FALSE))
    }
    shiny::showModal(shiny::modalDialog(
      title = "Remove image?",
      shiny::p(paste0(
        "Remove “",
        label,
        "” from this Builder session? The uploaded source file is not deleted."
      )),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          "enhance-remove_image_confirm",
          "Remove image",
          class = "btn btn-remove-soft"
        )
      )
    ))
    invisible(TRUE)
  }
  shiny::observeEvent(input[["enhance-drop_image"]], show_remove_image())
  shiny::observeEvent(input[["enhance-remove_image_confirm"]], {
    entry <- entry_of(current())
    section <- active_section()
    label <- active_image()
    if (is.null(entry) || is.null(section) || is.null(label)) {
      return()
    }
    labels <- image_labels_for(entry, section)
    position <- match(label, labels)
    commit_section(entry, section, NULL, label = label)
    entry <- entry_of(current())
    remaining <- image_labels_for(entry, section)
    next_label <- if (length(remaining)) {
      remaining[[min(position, length(remaining))]]
    } else {
      NULL
    }
    active_image(next_label)
    update_image_choices(entry, section, selected = next_label)
    restore(entry, section, next_label)
    shiny::removeModal()
  })
  shiny::observeEvent(input[["enhance-rename_image"]], {
    label <- active_image()
    if (is.null(label)) {
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Rename image",
      shiny::textInput("enhance-renamed_image_label", "Image label", label),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          "enhance-rename_image_confirm",
          "Rename image",
          class = "btn btn-action"
        )
      )
    ))
  })
  shiny::observeEvent(input[["enhance-rename_image_confirm"]], {
    entry <- entry_of(current())
    section <- active_section()
    label <- active_image()
    renamed <- trimws(input[["enhance-renamed_image_label"]] %||% "")
    images <- tryCatch(
      builder_image_collection_rename(
        collection_for(entry),
        section,
        label,
        renamed
      ),
      error = function(error) error
    )
    if (inherits(images, "condition")) {
      shiny::showNotification(
        conditionMessage(images),
        type = "error",
        duration = 5
      )
      return()
    }
    committed <- commit_images(entry, images)
    if (is.list(committed) && !is.null(committed$settings$images)) {
      images <- builder_image_collection_normalize(committed$settings$images)
    }
    active_image(renamed)
    image_collection_cache$dataset <- entry$id
    image_collection_cache$images <- images
    entry$settings$images <- images
    update_image_choices(entry, section, selected = renamed)
    shiny::removeModal()
  })
  apply_to_all <- function() {
    if (!save_current(notify = FALSE)) {
      return()
    }
    entry <- entry_of(current())
    source_section <- active_section()
    label <- active_image()
    images <- builder_alignment_apply_transform_to_matching_label(
      collection_for(entry),
      source_section,
      label
    )
    for (section in setdiff(names(images), source_section)) {
      record <- images[[section]][[label]]
      if (is.null(record)) {
        next
      }
      image <- builder_read_image_uri(record$source_uri)
      if (!is.null(image$error)) {
        shiny::showNotification(image$error, type = "error", duration = 8)
        return(invisible(FALSE))
      }
      record <- finalize_record(record, image)
      if (!is.null(record$error)) {
        shiny::showNotification(
          record$error,
          type = "error",
          duration = 8
        )
        return(invisible(FALSE))
      }
      record$saved <- TRUE
      images[[section]][[label]] <- record
    }
    commit_images(entry, images)
    draft(images[[source_section]][[label]])
    display_record(images[[source_section]][[label]])
    baseline(images[[source_section]][[label]])
    count <- sum(vapply(
      images,
      function(section) {
        label %in% names(section)
      },
      logical(1)
    ))
    shiny::showNotification(
      paste0("Applied “", label, "” transform to ", count, " section(s)."),
      type = "message",
      duration = 5
    )
    invisible(TRUE)
  }
  shiny::observeEvent(input[["enhance-apply_align_all"]], {
    entry <- entry_of(current())
    label <- active_image()
    images <- collection_for(entry)
    image_count <- sum(vapply(
      images,
      function(section) {
        !is.null(label) && label %in% names(section)
      },
      logical(1)
    ))
    if (is.null(draft()) || image_count < 1L) {
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Apply transform to matching image label?",
      shiny::p(paste0(
        "Only images named “",
        label,
        "” in other sections will receive this transform."
      )),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          "enhance-confirm_apply_align_all",
          "Apply to matching images",
          class = "btn btn-action"
        )
      )
    ))
  })
  shiny::observeEvent(input[["enhance-confirm_apply_align_all"]], {
    shiny::removeModal()
    apply_to_all()
  })

  discard_current <- function() {
    entry <- shiny::isolate(entry_of(current()))
    section <- shiny::isolate(active_section())
    old <- shiny::isolate(baseline())
    commit_section(entry, section, old)
    draft(old)
    display_record(old)
  }
  shiny::observeEvent(input[["enhance-alignment_switch_save"]], {
    section_target <- pending_section()
    image_target <- pending_image()
    if (
      (!is.null(section_target) || !is.null(image_target)) &&
        save_current(notify = FALSE)
    ) {
      shiny::removeModal()
      pending_section(NULL)
      pending_image(NULL)
      if (!is.null(section_target)) {
        switch_to(entry_of(current()), section_target)
      } else {
        active_image(image_target)
        shiny::updateSelectInput(
          session,
          "enhance-active_image",
          selected = image_target
        )
        restore(entry_of(current()), active_section(), image_target)
      }
    }
  })
  shiny::observeEvent(input[["enhance-alignment_switch_discard"]], {
    section_target <- pending_section()
    image_target <- pending_image()
    if (!is.null(section_target) || !is.null(image_target)) {
      discard_current()
      shiny::removeModal()
      pending_section(NULL)
      pending_image(NULL)
      if (!is.null(section_target)) {
        switch_to(entry_of(current()), section_target)
      } else {
        active_image(image_target)
        shiny::updateSelectInput(
          session,
          "enhance-active_image",
          selected = image_target
        )
        restore(entry_of(current()), active_section(), image_target)
      }
    }
  })
  shiny::observeEvent(input[["enhance-alignment_switch_cancel"]], {
    shiny::removeModal()
    pending_section(NULL)
    pending_image(NULL)
  })

  list(
    active_section = active_section,
    active_image = active_image,
    pending_image = pending_image,
    draft = draft,
    pending_upload = pending_upload,
    raw_image = raw_image,
    has_unsaved = shiny::reactive({
      current_draft <- draft()
      !is.null(current_draft) && !isTRUE(current_draft$saved)
    }),
    current_record = finalize_current_record
  )
}
