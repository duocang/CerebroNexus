## Spatial/Trekker alignment observers kept out of app.R so the workbench has
## one explicit boundary: bounded coordinate models enter, canonical alignment
## records leave. The Seurat object and local upload paths never enter state.

builder_spatial_preview_cache_key <- function(id, section) {
  stopifnot(
    is.character(id),
    length(id) == 1L,
    !is.na(id),
    nzchar(id),
    is.character(section),
    length(section) == 1L,
    !is.na(section),
    nzchar(section)
  )
  paste(id, section, sep = "::")
}

builder_preview_cache_drop_dataset <- function(cache, id) {
  if (
    !is.list(cache) ||
      !is.character(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !nzchar(id)
  ) {
    return(cache)
  }
  cache[[id]] <- NULL
  cache
}

builder_spatial_preview_cache_drop_dataset <- function(cache, id) {
  if (
    !is.list(cache) ||
      !is.character(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !nzchar(id)
  ) {
    return(cache)
  }
  prefix <- paste0(id, "::")
  remove <- startsWith(names(cache) %||% character(), prefix)
  cache[!remove]
}

builder_spatial_preview_cache_hit <- function(cache, key, contract) {
  record <- cache[[key]]
  is.list(record) && identical(record$contract, contract)
}

builder_spatial_preview_cache_begin <- function(cache, key, contract) {
  cache[[key]] <- list(
    contract = contract,
    frames = list(),
    status = "pending"
  )
  cache
}

builder_spatial_preview_cache_frames <- function(cache, key) {
  record <- cache[[key]]
  if (is.list(record) && is.list(record$frames)) record$frames else list()
}

builder_spatial_preview_cache_store_if_match <- function(
  cache,
  key,
  contract,
  frames
) {
  if (!builder_spatial_preview_cache_hit(cache, key, contract)) {
    return(cache)
  }
  cache[[key]]$frames <- frames
  cache[[key]]$status <- "ready"
  cache
}

builder_spatial_preview_cache_drop_if_match <- function(
  cache,
  key,
  contract
) {
  if (!builder_spatial_preview_cache_hit(cache, key, contract)) {
    return(cache)
  }
  cache[[key]] <- NULL
  cache
}

builder_spatial_preview_failure <- function(cache, payload) {
  key <- payload$preview_cache_key
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    key <- builder_spatial_preview_cache_key(payload$id, payload$section)
  }
  matched <- if (is.list(payload$preview_contract)) {
    builder_spatial_preview_cache_hit(cache, key, payload$preview_contract)
  } else {
    !is.null(cache[[key]])
  }
  if (is.list(payload$preview_contract)) {
    cache <- builder_spatial_preview_cache_drop_if_match(
      cache,
      key,
      payload$preview_contract
    )
  } else {
    cache[[key]] <- NULL
  }
  list(
    cache = cache,
    matched = matched,
    message = list(
      dataset = payload$id,
      state = "error",
      section = payload$section,
      switch_token = payload$switch_token
    )
  )
}

builder_spatial_alignment_server <- function(
  input,
  output,
  session,
  current,
  entry_of,
  entries = NULL,
  worker,
  enqueue,
  commit_images,
  alignment_preview,
  spatial_previews = shiny::reactiveVal(list()),
  spatial_coords
) {
  stopifnot(is.function(spatial_previews))
  raw_image <- shiny::reactiveVal(NULL)
  draft <- shiny::reactiveVal(NULL)
  coordinate_draft <- shiny::reactiveVal(list(rotation_degrees = 0, scale = 1))
  coordinate_baseline <- shiny::reactiveVal(list(
    rotation_degrees = 0,
    scale = 1
  ))
  point_appearance_baseline <- shiny::reactiveVal(NULL)
  point_appearance_input_ready <- shiny::reactiveVal(FALSE)
  coordinate_session_drafts <- shiny::reactiveVal(list())
  active_dataset <- shiny::reactiveVal(NULL)
  active_section <- shiny::reactiveVal(NULL)
  active_image <- shiny::reactiveVal(NULL)
  active_switch_dataset <- shiny::reactiveVal(NULL)
  active_switch_token <- shiny::reactiveVal(NULL)
  pending_project_selection <- shiny::reactiveVal(NULL)
  pending_upload <- shiny::reactiveVal(NULL)
  preview_contract <- shiny::reactiveVal(NULL)
  expected_controls <- shiny::reactiveVal(NULL)
  canvas_generation <- shiny::reactiveVal(0L)
  canvas_reset_token <- shiny::reactiveVal(0L)
  canvas_contract <- shiny::reactiveVal(NULL)
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
    for (section in names(transforms)) {
      if (is.list(transforms[[section]])) {
        transforms[[section]]$scale <- 1
      }
    }
    transforms
  }
  point_appearance_for <- function(entry, section, record = NULL) {
    defaults <- builder_alignment_defaults()
    from_record <- if (is.null(record)) {
      NULL
    } else {
      .builder_alignment_parameters(record)
    }
    if (!is.null(from_record)) {
      return(from_record[c("point_opacity", "point_size")])
    }
    stored <- entry$settings$spatial_point_appearance[[section]] %||% list()
    list(
      point_opacity = stored$point_opacity %||% defaults$point_opacity,
      point_size = stored$point_size %||% defaults$point_size
    )
  }
  coordinate_spec_for <- function(entry, section) {
    if (!identical(kind_for(section), "spatial")) {
      return(list(rotation_degrees = 0, scale = 1))
    }
    session_record <- builder_coordinate_drafts_get(
      coordinate_session_drafts(),
      entry$id,
      section
    )
    if (
      !is.null(session_record) &&
        identical(
          session_record$snapshot_identity,
          .builder_worker_identity(entry$snapshot)
        )
    ) {
      return(session_record$spec)
    }
    stored <- coordinate_transforms_for(entry)[[section]]
    .spx_coordinate_transform_spec_normalize(
      stored,
      context = paste0("spatial_coordinate_transforms$", section)
    )
  }
  preview_contract_for <- function(entry, section) {
    list(
      dataset = entry$id,
      snapshot_identity = .builder_worker_identity(entry$snapshot),
      section = section,
      default_projection = entry$settings$default_projection %||% NULL,
      group = entry$settings$default_group %||% NULL,
      assay = entry$settings$assay %||% NULL,
      layer = entry$settings$layer %||% "data"
    )
  }
  switch_token_for <- function(dataset) {
    if (identical(shiny::isolate(active_switch_dataset()), dataset)) {
      shiny::isolate(active_switch_token())
    } else {
      NULL
    }
  }
  send_switch_state <- function(dataset, state, section = NULL, token = NULL) {
    session$sendCustomMessage(
      "builder_dataset_switch_state",
      list(
        dataset = dataset,
        state = state,
        section = section,
        switch_token = token %||% switch_token_for(dataset)
      )
    )
  }
  finish_switch <- function(dataset, section = NULL) {
    token <- switch_token_for(dataset)
    if (!is.null(token)) {
      send_switch_state(dataset, "ready", section, token)
    }
    if (identical(shiny::isolate(active_switch_dataset()), dataset)) {
      active_switch_dataset(NULL)
      active_switch_token(NULL)
    }
    invisible(TRUE)
  }
  fail_preview_switch <- function(
    dataset,
    section = NULL,
    request_token = NULL
  ) {
    token <- switch_token_for(dataset) %||% request_token
    if (!is.null(token)) {
      send_switch_state(dataset, "error", section, token)
    }
    if (identical(shiny::isolate(active_switch_dataset()), dataset)) {
      active_switch_dataset(NULL)
      active_switch_token(NULL)
    }
    invisible(TRUE)
  }
  request_preview <- function(entry, section) {
    contract <- preview_contract_for(entry, section)
    cache_key <- builder_spatial_preview_cache_key(entry$id, section)
    cache <- shiny::isolate(spatial_previews())
    record <- cache[[cache_key]] %||% NULL
    token <- switch_token_for(entry$id)
    if (builder_spatial_preview_cache_hit(cache, cache_key, contract)) {
      preview_contract(contract)
      send_switch_state(entry$id, "spatial", section, token)
      if (identical(record$status, "ready")) {
        value <- builder_spatial_preview_cache_frames(cache, cache_key)
        alignment_preview(value)
        if (isTRUE(value$available)) {
          spatial_coords(list(
            x = value$spatial$x,
            y = value$spatial$y,
            sx = value$spatial$x,
            sy = value$spatial$y
          ))
        }
      }
      return(invisible(TRUE))
    }
    queued <- enqueue(list(
      kind = "spatial_preview",
      id = entry$id,
      section = section,
      preview_cache_key = cache_key,
      preview_contract = contract,
      switch_token = token,
      default_projection = entry$settings$default_projection %||% NULL,
      group = entry$settings$default_group %||% NULL,
      assay = entry$settings$assay %||% NULL,
      layer = entry$settings$layer %||% "data",
      replaces = "spatial_alignment",
      note = paste0("Loading paired views for ", section, "…")
    ))
    if (isTRUE(queued)) {
      spatial_previews(builder_spatial_preview_cache_begin(
        cache,
        cache_key,
        contract
      ))
      preview_contract(contract)
      send_switch_state(entry$id, "spatial", section, token)
    } else {
      send_switch_state(entry$id, "error", section, token)
    }
    invisible(queued)
  }
  update_controls <- function(
    record = NULL,
    bounds = NULL,
    point_appearance = NULL
  ) {
    parameters <- if (is.null(record)) {
      builder_alignment_defaults()
    } else {
      .builder_alignment_parameters(record)
    }
    if (!is.null(point_appearance)) {
      parameters[c("point_opacity", "point_size")] <- point_appearance
    }
    expected_controls(parameters)
    ranges <- builder_alignment_control_ranges(record, bounds)
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
    ids <- setdiff(ids, c("enhance-point_opacity", "enhance-point_size"))
    invisible(lapply(ids, function(id) shiny::freezeReactiveValue(input, id)))
    shiny::updateSliderInput(
      session,
      "enhance-img_dx",
      min = ranges$dx$min,
      max = ranges$dx$max,
      value = parameters$dx,
      step = ranges$dx$step
    )
    shiny::updateSliderInput(
      session,
      "enhance-img_dy",
      min = ranges$dy$min,
      max = ranges$dy$max,
      value = parameters$dy,
      step = ranges$dy$step
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
    if (is.null(stored)) {
      raw_image(NULL)
    } else {
      image <- builder_read_image_uri(stored$source_uri)
      if (!is.null(image$error)) {
        raw_image(NULL)
        shiny::showNotification(image$error, type = "error", duration = 8)
      } else {
        raw_image(list(
          array = image$array,
          width = stored$source_width %||% image$width,
          height = stored$source_height %||% image$height,
          source_dimensions = c(
            width = stored$source_width %||% image$width,
            height = stored$source_height %||% image$height
          )
        ))
      }
    }
    appearance <- point_appearance_for(entry, section, stored)
    point_appearance_baseline(appearance)
    point_appearance_input_ready(FALSE)
    update_controls(
      stored,
      alignment_preview()$bounds %||% NULL,
      appearance
    )
  }
  restore_coordinate_controls <- function(entry, section) {
    spec <- coordinate_spec_for(entry, section)
    coordinate_draft(spec)
    coordinate_baseline(spec)
    shiny::freezeReactiveValue(input, "enhance-coordinate_rotation")
    shiny::updateSliderInput(
      session,
      "enhance-coordinate_rotation",
      value = spec$rotation_degrees
    )
  }
  switch_to <- function(entry, section, label = NULL) {
    pending_upload(NULL)
    active_dataset(entry$id)
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

  project_selection <- shiny::reactive({
    id <- current()
    section <- active_section()
    if (is.null(id) || is.null(section)) {
      return(NULL)
    }
    list(
      dataset = id,
      section = section,
      image = active_image() %||% NULL
    )
  })
  restore_project_selection <- function(selection) {
    valid <- is.list(selection) &&
      builder_has_text(selection$dataset) &&
      builder_has_text(selection$section) &&
      (is.null(selection$image) || builder_has_text(selection$image))
    if (!valid) {
      pending_project_selection(NULL)
      return(invisible(FALSE))
    }
    selection <- list(
      dataset = as.character(selection$dataset),
      section = as.character(selection$section),
      image = if (is.null(selection$image)) {
        NULL
      } else {
        as.character(selection$image)
      }
    )
    pending_project_selection(selection)
    id <- shiny::isolate(current())
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    if (is.null(entry) || !identical(id, selection$dataset)) {
      return(invisible(TRUE))
    }
    if (identical(entry$load_state %||% "loaded", "artifact_ready")) {
      return(invisible(TRUE))
    }
    sections <- sections_for(entry)
    if (!length(sections)) {
      return(invisible(FALSE))
    }
    section <- if (selection$section %in% sections) {
      selection$section
    } else {
      sections[[1L]]
    }
    labels <- image_labels_for(entry, section)
    label <- if (!is.null(selection$image) && selection$image %in% labels) {
      selection$image
    } else if (length(labels)) {
      labels[[1L]]
    } else {
      NULL
    }
    switch_to(entry, section, label)
    pending_project_selection(NULL)
    invisible(TRUE)
  }

  restore_project_settings <- function(datasets) {
    datasets <- unique(as.character(datasets %||% character()))
    if (!length(datasets)) {
      return(invisible(FALSE))
    }
    pending <- shiny::isolate(coordinate_session_drafts())
    for (dataset in datasets) {
      pending <- builder_coordinate_drafts_drop(pending, dataset)
    }
    coordinate_session_drafts(pending)
    id <- shiny::isolate(current())
    if (is.null(id) || !id %in% datasets) {
      return(invisible(FALSE))
    }
    entry <- shiny::isolate(entry_of(id))
    if (is.null(entry)) {
      return(invisible(FALSE))
    }
    sections <- sections_for(entry)
    if (!length(sections)) {
      return(invisible(FALSE))
    }
    image_collection_cache$dataset <- entry$id
    image_collection_cache$images <- builder_image_collection_normalize(
      entry$settings$images %||% list()
    )
    image_collection_cache$known_sections <- names(
      image_collection_cache$images
    ) %||%
      character()
    section <- shiny::isolate(active_section())
    if (is.null(section) || !section %in% sections) {
      section <- sections[[1L]]
    }
    ## The raw source is initialized before its saved Project settings are
    ## merged. Retire that initialization scene before restoring the saved
    ## controls so a delayed browser event cannot reinstate its default 0°
    ## coordinate draft.
    canvas_generation(shiny::isolate(canvas_generation()) + 1L)
    canvas_reset_token(shiny::isolate(canvas_reset_token()) + 1L)
    switch_to(entry, section, shiny::isolate(active_image()))
    invisible(TRUE)
  }

  shiny::observeEvent(current(), {
    id <- current()
    previous_section <- shiny::isolate(active_section())
    previous_image <- shiny::isolate(active_image())
    preserve_active <- identical(
      shiny::isolate(active_dataset()),
      id
    )
    session$sendCustomMessage("builder_spatial_canvas_clear", list())
    raw_image(NULL)
    draft(NULL)
    coordinate_draft(list(rotation_degrees = 0, scale = 1))
    coordinate_baseline(list(rotation_degrees = 0, scale = 1))
    point_appearance_baseline(NULL)
    point_appearance_input_ready(FALSE)
    alignment_preview(NULL)
    spatial_coords(NULL)
    preview_contract(NULL)
    canvas_contract(NULL)
    pending_upload(NULL)
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    artifact_ready <- !is.null(entry) &&
      identical(
        entry$load_state %||% "loaded",
        "artifact_ready"
      )
    if (is.null(entry) || artifact_ready) {
      rm(list = ls(image_collection_cache), envir = image_collection_cache)
      active_dataset(NULL)
      active_section(NULL)
      active_image(NULL)
      if (artifact_ready) {
        finish_switch(entry$id)
        return()
      }
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
      active_dataset(NULL)
      active_section(NULL)
      active_image(NULL)
      finish_switch(entry$id)
      return()
    }
    selection <- shiny::isolate(pending_project_selection())
    if (is.list(selection) && identical(selection$dataset, entry$id)) {
      section <- if (selection$section %in% sections) {
        selection$section
      } else {
        sections[[1L]]
      }
      labels <- image_labels_for(entry, section)
      label <- if (!is.null(selection$image) && selection$image %in% labels) {
        selection$image
      } else if (length(labels)) {
        labels[[1L]]
      } else {
        NULL
      }
      switch_to(entry, section, label)
      pending_project_selection(NULL)
    } else if (preserve_active && previous_section %in% sections) {
      switch_to(entry, previous_section, previous_image)
    } else {
      switch_to(entry, sections[[1L]])
    }
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
  store_coordinate_draft <- function(
    spec,
    dataset,
    section,
    snapshot_identity,
    sequence = NULL,
    force = FALSE,
    generation = NULL
  ) {
    entry <- shiny::isolate(entry_of(dataset))
    if (
      is.null(entry) ||
        !section %in% sections_for(entry) ||
        !identical(kind_for(section), "spatial") ||
        !identical(
          snapshot_identity,
          .builder_worker_identity(entry$snapshot)
        )
    ) {
      return(invisible(FALSE))
    }
    if (
      !is.null(generation) &&
        (!is.numeric(generation) ||
          length(generation) != 1L ||
          is.na(generation) ||
          !is.finite(generation) ||
          as.numeric(generation) !=
            as.numeric(
              shiny::isolate(canvas_generation())
            ))
    ) {
      return(invisible(FALSE))
    }
    stored <- tryCatch(
      builder_coordinate_drafts_put(
        shiny::isolate(coordinate_session_drafts()),
        dataset = dataset,
        snapshot_identity = snapshot_identity,
        section = section,
        spec = spec,
        sequence = sequence,
        force = force
      ),
      error = function(error) NULL
    )
    if (is.null(stored) || !isTRUE(stored$accepted)) {
      return(invisible(FALSE))
    }
    coordinate_session_drafts(stored$drafts)
    is_active <- identical(dataset, shiny::isolate(current())) &&
      identical(section, shiny::isolate(active_section()))
    if (is_active) {
      coordinate_draft(stored$record$spec)
      coordinate_baseline(stored$record$spec)
    }
    invisible(TRUE)
  }

  shiny::observeEvent(
    input[["builder_spatial_coordinate_draft"]],
    {
      event <- input[["builder_spatial_coordinate_draft"]]
      if (!is.list(event)) {
        return()
      }
      spec <- list(
        rotation_degrees = event$rotationDegrees,
        scale = 1
      )
      store_coordinate_draft(
        spec = spec,
        dataset = event$dataset,
        section = event$section,
        snapshot_identity = event$snapshotIdentity,
        sequence = event$sequence,
        generation = event$generation %||% NULL
      )
    },
    ignoreInit = TRUE
  )

  if (is.function(entries)) {
    shiny::observe({
      all_entries <- entries()
      identities <- lapply(all_entries, function(entry) {
        list(
          id = entry$id,
          snapshot_identity = .builder_worker_identity(entry$snapshot)
        )
      })
      current_drafts <- shiny::isolate(coordinate_session_drafts())
      pruned <- builder_coordinate_drafts_prune(current_drafts, identities)
      if (!identical(pruned$drafts, current_drafts)) {
        coordinate_session_drafts(pruned$drafts)
      }
    })
  }
  point_appearance <- shiny::reactive({
    current_draft <- draft()
    defaults <- if (is.null(current_draft)) {
      entry <- entry_of(current())
      section <- active_section()
      if (is.null(entry) || is.null(section)) {
        builder_alignment_defaults()
      } else {
        utils::modifyList(
          builder_alignment_defaults(),
          point_appearance_for(entry, section)
        )
      }
    } else {
      current_draft
    }
    restored <- point_appearance_baseline()
    if (!isTRUE(point_appearance_input_ready()) && !is.null(restored)) {
      return(list(
        opacity = restored$point_opacity,
        size = restored$point_size
      ))
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
  encode_current_image <- function() {
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
      rotate = transform$rotation,
      source_dimensions = image$source_dimensions %||%
        c(width = image$width, height = image$height)
    )
  }
  current_record <- function(encode = FALSE) {
    current_draft <- draft()
    preview <- alignment_preview()
    if (
      is.null(current_draft) ||
        !isTRUE(preview$available)
    ) {
      return(NULL)
    }
    current_encoded <- if (isTRUE(encode)) encode_current_image() else NULL
    if (!is.null(current_encoded$error)) {
      return(current_encoded)
    }
    observed <- parameters()
    geometry <- current_encoded %||% current_draft
    record <- builder_alignment_record(
      source = current_draft$source,
      source_uri = current_draft$source_uri,
      uri = current_encoded$uri %||% current_draft$uri,
      base_bounds = current_draft$base_bounds,
      parameters = observed,
      image_geometry = geometry,
      section = list(id = active_section(), kind = preview$section$kind)
    )
    facts <- intersect(
      names(geometry),
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
    record[facts] <- geometry[facts]
    record$source_content_md5 <- current_draft$source_content_md5 %||% NULL
    record
  }

  shiny::observeEvent(input[["enhance-active_section"]], {
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    section <- input[["enhance-active_section"]]
    previous <- shiny::isolate(active_section())
    if (is.null(entry) || !nzchar(section) || identical(section, previous)) {
      return()
    }
    pending_project_selection(NULL)
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
    pending_project_selection(NULL)
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
    image_encoded <- builder_encode_image(
      image$array,
      max_px = 1400,
      retain_normalized_array = TRUE
    )
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
    parameters <- builder_alignment_defaults()
    appearance <- point_appearance_for(entry, section, previous)
    parameters[c("point_opacity", "point_size")] <- appearance
    if (!length(existing)) {
      stored_appearance <- entry$settings$spatial_point_appearance %||% list()
      stored_appearance[[section]] <- NULL
      entry$settings$spatial_point_appearance <- stored_appearance
    }
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
      parameters = parameters,
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
    record$source_content_md5 <- image_encoded$content_md5
    raw_image(list(
      array = image_encoded$normalized_array,
      width = image_encoded$source_width,
      height = image_encoded$source_height,
      source_dimensions = image_encoded$source_dimensions
    ))
    draft(record)
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
      entry <- entry_of(current())
      section <- active_section()
      if (!is.null(entry) && !is.null(section)) {
        appearance <- point_appearance_for(entry, section, draft())
        expected <- shiny::isolate(expected_controls())
        if (
          is.null(draft()) &&
            !is.null(expected) &&
            identical(
              expected[c("point_opacity", "point_size")],
              appearance[c("point_opacity", "point_size")]
            )
        ) {
          return()
        }
        update_controls(
          draft(),
          preview$bounds,
          appearance
        )
      }
    }
  })

  scene_entry_contract <- shiny::reactiveVal(NULL)
  shiny::observe({
    id <- current()
    entry <- if (is.null(id)) NULL else entry_of(id)
    next_contract <- if (is.null(entry)) {
      NULL
    } else {
      group <- entry$settings$default_group %||% ""
      list(
        id = entry$id,
        snapshot_identity = .builder_worker_identity(entry$snapshot),
        default_group = group,
        palette = entry$settings$palette %||% "cerebro",
        color_overrides = builder_settings_color_overrides(
          entry$settings
        )[[group]] %||%
          character(),
        spatial_point_appearance = entry$settings$spatial_point_appearance %||%
          list()
      )
    }
    if (!identical(next_contract, shiny::isolate(scene_entry_contract()))) {
      scene_entry_contract(next_contract)
    }
  })

  colors <- shiny::reactive({
    preview <- alignment_preview()
    contract <- scene_entry_contract()
    if (!isTRUE(preview$available) || is.null(contract)) {
      return(character())
    }
    levels <- unique(as.character(preview$spatial$group))
    builder_level_colors(
      levels,
      contract$palette,
      contract$color_overrides
    )
  })
  shiny::observe({
    preview <- alignment_preview()
    contract <- scene_entry_contract()
    section <- active_section()
    if (
      is.null(preview) ||
        is.null(contract) ||
        is.null(section) ||
        !identical(preview$section$id, section)
    ) {
      return()
    }
    generation <- shiny::isolate(canvas_generation()) + 1L
    canvas_generation(generation)
    identity <- paste(
      contract$id,
      contract$snapshot_identity,
      section,
      kind_for(section),
      active_image() %||% "",
      sep = "::"
    )
    scene <- builder_spatial_canvas_scene(
      preview = preview,
      colors = colors(),
      record = draft(),
      point_appearance = point_appearance_for(
        list(
          settings = list(
            spatial_point_appearance = contract$spatial_point_appearance
          )
        ),
        section,
        draft()
      ),
      coordinate_transform = coordinate_draft(),
      identity = identity,
      generation = generation,
      reset_token = canvas_reset_token(),
      dataset = contract$id,
      snapshot_identity = contract$snapshot_identity,
      section = section
    )
    canvas_contract(scene[c(
      "viewKey",
      "generation",
      "resetToken",
      "dataset",
      "snapshotIdentity",
      "section",
      "controls"
    )])
    session$sendCustomMessage("builder_spatial_canvas_scene", scene)
    finish_switch(contract$id, section)
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

  commit_alignment_controls <- function() {
    current_draft <- shiny::isolate(draft())
    if (is.null(current_draft)) {
      return(invisible(FALSE))
    }
    observed <- shiny::isolate(parameters())
    expected <- shiny::isolate(expected_controls())
    if (
      !is.null(expected) &&
        isTRUE(all.equal(
          observed,
          expected,
          check.attributes = FALSE
        ))
    ) {
      return(invisible(FALSE))
    }
    draft_parameters <- .builder_alignment_parameters(current_draft)
    if (
      isTRUE(all.equal(
        observed,
        draft_parameters,
        check.attributes = FALSE
      ))
    ) {
      return(invisible(FALSE))
    }
    next_record <- current_draft
    parameter_names <- names(builder_alignment_defaults())
    next_record[parameter_names] <- observed[parameter_names]
    oriented_bounds <- builder_alignment_oriented_bounds(
      current_draft$base_bounds,
      observed
    )
    next_record$bounds <- builder_alignment_transform_bounds(
      oriented_bounds,
      observed
    )
    expected_controls(NULL)
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
    commit_alignment_controls(),
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    list(
      input[["enhance-point_opacity"]],
      input[["enhance-point_size"]]
    ),
    {
      if (!is.null(shiny::isolate(draft()))) {
        return()
      }
      entry <- shiny::isolate(entry_of(current()))
      section <- shiny::isolate(active_section())
      if (
        is.null(entry) ||
          is.null(section) ||
          !identical(kind_for(section), "spatial")
      ) {
        return()
      }
      restored <- shiny::isolate(point_appearance_baseline())
      if (is.null(restored)) {
        return()
      }
      appearance <- list(
        opacity = (input[["enhance-point_opacity"]] %||%
          ((restored$point_opacity %||%
            builder_alignment_defaults()$point_opacity) *
            100)) /
          100,
        size = input[["enhance-point_size"]] %||%
          (restored$point_size %||% builder_alignment_defaults()$point_size)
      )
      expected <- shiny::isolate(expected_controls())
      if (!is.null(expected)) {
        if (
          !identical(
            list(
              point_opacity = appearance$opacity,
              point_size = appearance$size
            ),
            expected[c("point_opacity", "point_size")]
          )
        ) {
          default_appearance <- builder_alignment_defaults()[c(
            "point_opacity",
            "point_size"
          )]
          if (
            !isTRUE(shiny::isolate(point_appearance_input_ready())) &&
              identical(
                list(
                  point_opacity = appearance$opacity,
                  point_size = appearance$size
                ),
                default_appearance
              )
          ) {
            return()
          }
          expected_controls(NULL)
        } else {
          expected_controls(NULL)
        }
      }
      point_appearance_input_ready(TRUE)
      stored <- entry$settings$spatial_point_appearance %||% list()
      next_value <- list(
        point_opacity = appearance$opacity,
        point_size = appearance$size
      )
      if (identical(shiny::isolate(point_appearance_baseline()), next_value)) {
        return()
      }
      if (identical(stored[[section]], next_value)) {
        return()
      }
      stored[[section]] <- next_value
      entry$settings$spatial_point_appearance <- stored
      point_appearance_baseline(next_value)
      commit_images(entry, collection_for(entry))
    },
    ignoreInit = TRUE
  )

  materialize_coordinate_drafts <- function(
    dataset = NULL,
    section = NULL,
    notify = TRUE
  ) {
    collect_latest_entries <- function(updated = list()) {
      all <- if (is.function(entries)) {
        shiny::isolate(entries())
      } else {
        updated
      }
      for (dataset_id in names(updated) %||% character()) {
        index <- which(vapply(
          all,
          function(entry) identical(entry$id, dataset_id),
          logical(1)
        ))
        if (length(index) == 1L) {
          all[[index]] <- updated[[dataset_id]]
        } else {
          all[[length(all) + 1L]] <- updated[[dataset_id]]
        }
      }
      all
    }
    pending <- shiny::isolate(coordinate_session_drafts())
    target_datasets <- if (is.null(dataset)) {
      names(pending) %||% character()
    } else {
      as.character(dataset)
    }
    materialized_entries <- list()
    for (dataset_id in target_datasets) {
      records <- pending[[dataset_id]] %||% list()
      if (!is.null(section)) {
        records <- records[intersect(names(records), section)]
      }
      if (!length(records)) {
        next
      }
      entry <- shiny::isolate(entry_of(dataset_id))
      if (is.null(entry)) {
        pending <- builder_coordinate_drafts_drop(pending, dataset_id)
        coordinate_session_drafts(pending)
        next
      }
      snapshot_identity <- .builder_worker_identity(entry$snapshot)
      valid <- vapply(
        records,
        function(record) {
          identical(record$snapshot_identity, snapshot_identity)
        },
        logical(1)
      )
      stale_sections <- names(records)[!valid]
      for (stale_section in stale_sections) {
        pending <- builder_coordinate_drafts_drop(
          pending,
          dataset_id,
          stale_section
        )
      }
      records <- records[valid]
      if (!length(records)) {
        coordinate_session_drafts(pending)
        next
      }
      applied <- tryCatch(
        builder_coordinate_drafts_apply_entry(
          entry,
          records,
          snapshot_identity = snapshot_identity
        ),
        error = function(error) error
      )
      committed <- if (inherits(applied, "condition")) {
        applied
      } else if (isTRUE(applied$changed)) {
        tryCatch(
          commit_images(applied$entry, applied$entry$settings$images),
          error = function(error) error
        )
      } else {
        applied$entry
      }
      if (inherits(committed, "condition") || identical(committed, FALSE)) {
        coordinate_session_drafts(pending)
        if (isTRUE(notify)) {
          message <- if (inherits(committed, "condition")) {
            conditionMessage(committed)
          } else {
            "The coordinate settings could not be saved."
          }
          shiny::showNotification(message, type = "error", duration = 8)
        }
        return(list(
          ok = FALSE,
          entries = materialized_entries,
          all_entries = collect_latest_entries(materialized_entries),
          error = if (inherits(committed, "condition")) {
            conditionMessage(committed)
          } else {
            "Coordinate settings could not be saved."
          }
        ))
      }
      for (materialized_section in names(records)) {
        pending <- builder_coordinate_drafts_drop(
          pending,
          dataset_id,
          materialized_section
        )
      }
      coordinate_session_drafts(pending)
      latest <- shiny::isolate(entry_of(dataset_id)) %||% applied$entry
      materialized_entries[[dataset_id]] <- latest
      if (
        isTRUE(applied$changed) &&
          identical(dataset_id, shiny::isolate(current()))
      ) {
        image_collection_cache$dataset <- dataset_id
        image_collection_cache$images <- applied$entry$settings$images
        image_collection_cache$known_sections <- unique(c(
          image_collection_cache$known_sections %||% character(),
          applied$sections,
          names(applied$entry$settings$images)
        ))
      }
    }
    list(
      ok = TRUE,
      entries = materialized_entries,
      all_entries = collect_latest_entries(materialized_entries),
      error = NULL
    )
  }

  shiny::observeEvent(
    input[["enhance-reset_coordinate_transform"]],
    {
      spec <- list(rotation_degrees = 0, scale = 1)
      defaults <- builder_alignment_defaults()
      appearance <- defaults[c("point_opacity", "point_size")]
      entry <- shiny::isolate(entry_of(current()))
      section <- shiny::isolate(active_section())
      if (is.null(entry) || is.null(section)) {
        return()
      }
      session$sendCustomMessage(
        "builder_coordinate_reset_motion",
        list(
          ids = c(
            "enhance-coordinate_rotation",
            "enhance-point_opacity",
            "enhance-point_size"
          )
        )
      )
      store_coordinate_draft(
        spec = spec,
        dataset = entry$id,
        section = section,
        snapshot_identity = .builder_worker_identity(entry$snapshot),
        force = TRUE
      )
      current_draft <- shiny::isolate(draft())
      if (is.null(current_draft)) {
        stored <- entry$settings$spatial_point_appearance %||% list()
        stored[[section]] <- appearance
        entry$settings$spatial_point_appearance <- stored
        commit_images(entry, collection_for(entry))
      } else {
        current_draft[names(appearance)] <- appearance
        draft(current_draft)
        commit_section(entry, section, current_draft)
      }
      point_appearance_baseline(appearance)
      point_appearance_input_ready(FALSE)
      shiny::freezeReactiveValue(input, "enhance-coordinate_rotation")
      shiny::freezeReactiveValue(input, "enhance-point_opacity")
      shiny::freezeReactiveValue(input, "enhance-point_size")
      shiny::updateSliderInput(
        session,
        "enhance-coordinate_rotation",
        value = 0
      )
      shiny::updateSliderInput(
        session,
        "enhance-point_opacity",
        value = defaults$point_opacity * 100
      )
      shiny::updateSliderInput(
        session,
        "enhance-point_size",
        value = defaults$point_size
      )
      canvas_reset_token(canvas_reset_token() + 1L)
    }
  )
  shiny::observeEvent(input[["enhance-reset_align"]], {
    current_draft <- draft()
    if (is.null(current_draft)) {
      return()
    }
    reset <- builder_alignment_reset(current_draft)
    draft(reset)
    canvas_reset_token(canvas_reset_token() + 1L)
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
    if (length(labels) == 1L) {
      removed <- builder_alignment_normalize(
        collection_for(entry)[[section]][[label]],
        section,
        kind_for(section)
      )
      appearance <- point_appearance_for(entry, section, removed)
      stored_appearance <- entry$settings$spatial_point_appearance %||% list()
      stored_appearance[[section]] <- appearance
      entry$settings$spatial_point_appearance <- stored_appearance
    }
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
    entry <- entry_of(current())
    source_section <- active_section()
    label <- active_image()
    original_images <- collection_for(entry)
    images <- builder_alignment_apply_transform_to_matching_label(
      original_images,
      source_section,
      label
    )
    rendered <- builder_alignment_render_matching_label(
      images,
      original_images,
      label
    )
    images <- rendered$images
    count <- length(rendered$successful_sections)
    failed <- rendered$failed_sections
    if (count > 0L) {
      committed <- commit_images(entry, images)
      if (is.list(committed) && !is.null(committed$settings$images)) {
        images <- builder_image_collection_normalize(committed$settings$images)
      }
      draft(images[[source_section]][[label]])
    }
    if (length(failed)) {
      shiny::showNotification(
        paste0(
          if (count > 0L) {
            paste0(
              "Applied “",
              label,
              "” transform to ",
              count,
              " section(s); "
            )
          } else {
            paste0("Could not apply “", label, "” transform; ")
          },
          length(failed),
          " section(s) were left unchanged: ",
          paste(failed, collapse = ", "),
          "."
        ),
        type = if (count > 0L) "warning" else "error",
        duration = 7
      )
      return(invisible(count > 0L))
    }
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

  request_dataset_switch <- function(target, commit, switch_token = NULL) {
    if (
      !is.character(target) ||
        length(target) != 1L ||
        is.na(target) ||
        !nzchar(target) ||
        !is.function(commit)
    ) {
      return(invisible(FALSE))
    }
    if (
      !is.null(switch_token) &&
        (!is.numeric(switch_token) ||
          length(switch_token) != 1L ||
          is.na(switch_token) ||
          !is.finite(switch_token) ||
          switch_token < 1)
    ) {
      return(invisible(FALSE))
    }
    active_switch_dataset(target)
    active_switch_token(switch_token)
    committed <- !identical(commit(), FALSE)
    if (!committed) {
      send_switch_state(target, "error", token = switch_token)
      active_switch_dataset(NULL)
      active_switch_token(NULL)
    }
    invisible(committed)
  }

  list(
    active_section = active_section,
    active_image = active_image,
    project_selection = project_selection,
    draft = draft,
    point_appearance = point_appearance,
    coordinate_drafts = coordinate_session_drafts,
    canvas_contract = canvas_contract,
    pending_upload = pending_upload,
    raw_image = raw_image,
    request_dataset_switch = request_dataset_switch,
    fail_preview_switch = fail_preview_switch,
    restore_project_settings = restore_project_settings,
    restore_project_selection = restore_project_selection,
    materialize_coordinate_drafts = materialize_coordinate_drafts,
    current_record = current_record
  )
}
