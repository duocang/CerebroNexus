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
  spatial_coords
) {
  raw_image <- shiny::reactiveVal(NULL)
  draft <- shiny::reactiveVal(NULL)
  baseline <- shiny::reactiveVal(NULL)
  active_section <- shiny::reactiveVal(NULL)
  pending_section <- shiny::reactiveVal(NULL)
  pending_upload <- shiny::reactiveVal(NULL)
  selected_cells <- shiny::reactiveVal(character())
  preview_contract <- shiny::reactiveVal(NULL)

  output[["enhance-has_image"]] <- shiny::reactive(!is.null(draft()))
  shiny::outputOptions(
    output,
    "enhance-has_image",
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
  commit_section <- function(entry, section, value) {
    images <- entry$settings$images %||% list()
    images[[section]] <- value
    commit_images(entry, images)
  }
  preview_contract_for <- function(entry, section) {
    list(
      dataset = entry$id,
      snapshot_identity = .builder_worker_identity(entry$snapshot),
      section = section,
      default_projection = entry$settings$default_projection %||% NULL,
      group = entry$settings$default_group %||% NULL
    )
  }
  request_preview <- function(entry, section) {
    alignment_preview(NULL)
    selected_cells(character())
    queued <- enqueue(list(
      kind = "spatial_preview",
      id = entry$id,
      section = section,
      default_projection = entry$settings$default_projection %||% NULL,
      group = entry$settings$default_group %||% NULL,
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
  restore <- function(entry, section) {
    stored <- builder_alignment_normalize(
      entry$settings$images[[section]],
      section_id = section,
      section_kind = kind_for(section)
    )
    draft(stored)
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
  switch_to <- function(entry, section) {
    pending_upload(NULL)
    active_section(section)
    shiny::updateSelectInput(
      session,
      "enhance-active_section",
      selected = section
    )
    restore(entry, section)
    request_preview(entry, section)
  }

  shiny::observeEvent(current(), {
    raw_image(NULL)
    draft(NULL)
    baseline(NULL)
    alignment_preview(NULL)
    spatial_coords(NULL)
    preview_contract(NULL)
    pending_upload(NULL)
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    sections <- if (is.null(entry)) character() else sections_for(entry)
    if (!length(sections)) {
      active_section(NULL)
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
  point_appearance <- shiny::reactive({
    current_draft <- draft()
    defaults <- if (is.null(current_draft)) {
      builder_alignment_defaults()
    } else {
      current_draft
    }
    list(
      opacity = (input[["enhance-point_opacity"]] %||%
        (defaults$point_opacity * 100)) /
        100,
      size = input[["enhance-point_size"]] %||% defaults$point_size
    )
  })
  orientation <- shiny::reactive({
    current_draft <- draft()
    defaults <- if (is.null(current_draft)) {
      builder_alignment_defaults()
    } else {
      current_draft
    }
    list(
      rotation = input[["enhance-img_rotate"]] %||% defaults$rotation,
      flip_x = input[["enhance-image_flip_x"]] %||% defaults$flip_x,
      flip_y = input[["enhance-image_flip_y"]] %||% defaults$flip_y
    )
  })
  encoded <- shiny::reactive({
    image <- raw_image()
    if (is.null(image)) {
      return(NULL)
    }
    transform <- orientation()
    builder_encode_image(
      image$array,
      max_px = 1400,
      flip_y = transform$flip_y,
      flip_x = transform$flip_x,
      rotate = transform$rotation
    )
  })
  current_record <- shiny::reactive({
    current_draft <- draft()
    current_encoded <- encoded()
    preview <- alignment_preview()
    if (
      is.null(current_draft) ||
        is.null(current_encoded) ||
        !isTRUE(preview$available)
    ) {
      return(NULL)
    }
    if (!is.null(current_encoded$error)) {
      return(current_encoded)
    }
    record <- builder_alignment_record(
      source = current_draft$source,
      source_uri = current_draft$source_uri,
      uri = current_encoded$uri,
      base_bounds = current_draft$base_bounds,
      parameters = parameters(),
      saved = FALSE,
      section = list(id = active_section(), kind = preview$section$kind)
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
    cover <- builder_bounds_cover(
      record$bounds,
      list(preview$spatial$x, preview$spatial$y)
    )
    record$outside <- cover$outside
    record$total <- cover$total
    record
  })

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

  attach_upload <- function(upload, preview) {
    filename <- basename(as.character(upload$name[[1L]]))
    image <- builder_read_image(upload$datapath[[1L]], filename = filename)
    if (!is.null(image$error)) {
      shiny::showNotification(image$error, type = "error", duration = 8)
      return(invisible(FALSE))
    }
    image_encoded <- builder_encode_image(image$array, max_px = 1400)
    if (!is.null(image_encoded$error)) {
      shiny::showNotification(image_encoded$error, type = "error", duration = 8)
      return(invisible(FALSE))
    }
    entry <- entry_of(current())
    section <- active_section()
    previous <- builder_alignment_normalize(
      entry$settings$images[[section]],
      section,
      preview$section$kind
    )
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
    update_controls(record, preview$bounds)
    commit_section(entry, section, record)
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
  output[["enhance-alignment_transcriptome_plot"]] <- plotly::renderPlotly({
    preview <- alignment_preview()
    shiny::req(isTRUE(preview$available))
    appearance <- point_appearance()
    builder_alignment_plot(
      preview$transcriptome,
      colors(),
      source = "alignment_transcriptome",
      selected_cells = selected_cells(),
      point_opacity = appearance$opacity,
      point_size = appearance$size
    )
  })
  output[["enhance-alignment_spatial_plot"]] <- plotly::renderPlotly({
    preview <- alignment_preview()
    shiny::req(isTRUE(preview$available))
    record <- current_record()
    if (!is.null(record$error)) {
      record <- NULL
    }
    appearance <- point_appearance()
    builder_alignment_plot(
      preview$spatial,
      colors(),
      source = "alignment_spatial",
      selected_cells = selected_cells(),
      image_uri = record$uri %||% NULL,
      image_bounds = record$bounds %||% NULL,
      image_opacity = parameters()$image_opacity,
      point_opacity = appearance$opacity,
      point_size = appearance$size
    )
  })
  register_selection <- function(event_name, source) {
    event <- shiny::reactive({
      suppressWarnings(plotly::event_data(
        event_name,
        source = source,
        priority = "event"
      ))
    })
    shiny::observeEvent(
      event(),
      {
        selected_cells(builder_alignment_event_cells(event()))
      },
      ignoreNULL = FALSE
    )
  }
  selection_registered <- FALSE
  shiny::observeEvent(
    alignment_preview(),
    {
      if (selection_registered || !isTRUE(alignment_preview()$available)) {
        return()
      }
      selection_registered <<- TRUE
      session$onFlushed(
        function() {
          register_selection("plotly_click", "alignment_transcriptome")
          register_selection("plotly_selected", "alignment_transcriptome")
          register_selection("plotly_deselect", "alignment_transcriptome")
          register_selection("plotly_click", "alignment_spatial")
          register_selection("plotly_selected", "alignment_spatial")
          register_selection("plotly_deselect", "alignment_spatial")
        },
        once = TRUE
      )
    },
    ignoreInit = FALSE
  )

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
      return(shiny::div(
        class = "notice",
        "Points-only spatial view. Add a tissue image only when one is available."
      ))
    }
    builder_tissue_image_file_ui("enhance", current_draft)
  })

  mark_unsaved <- function() {
    current_draft <- shiny::isolate(draft())
    if (is.null(current_draft) || !isTRUE(current_draft$saved)) {
      return(invisible(FALSE))
    }
    next_record <- shiny::isolate(current_record())
    if (is.null(next_record) || !is.null(next_record$error)) {
      return(invisible(FALSE))
    }
    baseline(current_draft)
    next_record$saved <- FALSE
    draft(next_record)
    commit_section(
      shiny::isolate(entry_of(current())),
      shiny::isolate(active_section()),
      next_record
    )
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
    record <- shiny::isolate(current_record())
    if (is.null(entry) || is.null(section) || is.null(record)) {
      return(FALSE)
    }
    if (!is.null(record$error)) {
      shiny::showNotification(record$error, type = "error", duration = 8)
      return(FALSE)
    }
    record$saved <- TRUE
    commit_section(entry, section, record)
    draft(record)
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
  shiny::observeEvent(input[["enhance-reset_align"]], {
    current_draft <- draft()
    if (is.null(current_draft)) {
      return()
    }
    reset <- builder_alignment_reset(current_draft)
    draft(reset)
    update_controls(reset, alignment_preview()$bounds %||% NULL)
    commit_section(entry_of(current()), active_section(), reset)
  })
  shiny::observeEvent(input[["enhance-drop_image"]], {
    entry <- entry_of(current())
    section <- active_section()
    if (is.null(entry) || is.null(section)) {
      return()
    }
    commit_section(entry, section, NULL)
    raw_image(NULL)
    draft(NULL)
    baseline(NULL)
    update_controls(NULL, alignment_preview()$bounds %||% NULL)
  })
  apply_to_all <- function() {
    if (!save_current(notify = FALSE)) {
      return()
    }
    entry <- entry_of(current())
    source_section <- active_section()
    images <- builder_alignment_apply_transform_to_all(
      entry$settings$images %||% list(),
      source_section
    )
    for (section in names(images)) {
      record <- builder_alignment_normalize(
        images[[section]],
        section,
        kind_for(section)
      )
      image <- builder_read_image_uri(record$source_uri)
      if (!is.null(image$error)) {
        next
      }
      image_encoded <- builder_encode_image(
        image$array,
        max_px = 1400,
        flip_y = record$flip_y,
        flip_x = record$flip_x,
        rotate = record$rotation
      )
      if (!is.null(image_encoded$error)) {
        next
      }
      record$uri <- image_encoded$uri
      record$saved <- TRUE
      images[[section]] <- record
    }
    commit_images(entry, images)
    draft(images[[source_section]])
    baseline(images[[source_section]])
    shiny::showNotification(
      paste0("Applied this transform to ", length(images), " section(s)."),
      type = "message",
      duration = 5
    )
    invisible(TRUE)
  }
  shiny::observeEvent(input[["enhance-apply_align_all"]], {
    entry <- entry_of(current())
    image_count <- length(entry$settings$images %||% list())
    if (is.null(draft()) || image_count < 1L) {
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Apply transform to all image-bearing sections?",
      shiny::p(
        "Each section keeps its own image. Position, scale, rotation, flips, and appearance will be copied."
      ),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          "enhance-confirm_apply_align_all",
          "Apply to all sections",
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
  }
  shiny::observeEvent(input[["enhance-alignment_switch_save"]], {
    target <- pending_section()
    if (!is.null(target) && save_current(notify = FALSE)) {
      shiny::removeModal()
      pending_section(NULL)
      switch_to(entry_of(current()), target)
    }
  })
  shiny::observeEvent(input[["enhance-alignment_switch_discard"]], {
    target <- pending_section()
    if (!is.null(target)) {
      discard_current()
      shiny::removeModal()
      pending_section(NULL)
      switch_to(entry_of(current()), target)
    }
  })
  shiny::observeEvent(input[["enhance-alignment_switch_cancel"]], {
    shiny::removeModal()
    pending_section(NULL)
  })

  list(
    active_section = active_section,
    draft = draft,
    pending_upload = pending_upload,
    raw_image = raw_image,
    current_record = current_record
  )
}
