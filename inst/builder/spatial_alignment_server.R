## Spatial/Trekker alignment observers kept out of app.R so the workbench has
## one explicit boundary: bounded coordinate models enter, canonical alignment
## records leave. The Seurat object and image bytes never enter Builder state;
## image records retain only the canonical external source path and metadata.

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
  session_image_dir <- tempfile("cerebro-builder-images-")
  dir.create(
    session_image_dir,
    recursive = TRUE,
    mode = "0700",
    showWarnings = FALSE
  )
  session_image_dir <- normalizePath(
    session_image_dir,
    winslash = "/",
    mustWork = TRUE
  )
  session$onSessionEnded(function() {
    unlink(session_image_dir, recursive = TRUE, force = TRUE)
  })
  session_image_urls <- new.env(parent = emptyenv())
  session_image_url <- function(record) {
    record <- builder_alignment_normalize(record)
    if (is.null(record)) {
      return(NULL)
    }
    path <- tryCatch(
      normalizePath(record$source_path, winslash = "/", mustWork = TRUE),
      error = function(error) NULL
    )
    if (is.null(path) || !isTRUE(file_test("-f", path))) {
      return(NULL)
    }
    md5 <- tryCatch(
      unname(as.character(tools::md5sum(path))),
      error = function(error) NA_character_
    )
    if (
      is.na(md5) ||
        (!is.null(record$source_content_md5) &&
          !identical(record$source_content_md5, md5))
    ) {
      return(NULL)
    }
    cached <- get0(md5, envir = session_image_urls, inherits = FALSE)
    if (!is.null(cached)) {
      return(cached)
    }
    mime <- as.character(record$source$type %||% character())
    if (
      length(mime) != 1L ||
        is.na(mime) ||
        !mime %in% c("image/png", "image/jpeg")
    ) {
      mime <- if (tolower(tools::file_ext(path)) == "png") {
        "image/png"
      } else {
        "image/jpeg"
      }
    }
    data <- list(path = path, mime = mime, md5 = md5)
    url <- session$registerDataObj(
      paste0("builder-image-", md5),
      data,
      function(data, request) {
        current_md5 <- tryCatch(
          unname(as.character(tools::md5sum(data$path))),
          error = function(error) NA_character_
        )
        if (!identical(current_md5, data$md5)) {
          return(shiny::httpResponse(404L, "text/plain", "Not found"))
        }
        bytes <- tryCatch(
          readBin(data$path, what = "raw", n = file.size(data$path)),
          error = function(error) NULL
        )
        if (is.null(bytes)) {
          return(shiny::httpResponse(404L, "text/plain", "Not found"))
        }
        shiny::httpResponse(
          200L,
          data$mime,
          bytes,
          headers = list(
            "Cache-Control" = "private, max-age=31536000, immutable",
            "ETag" = paste0('"', data$md5, '"'),
            "X-Content-Type-Options" = "nosniff"
          )
        )
      }
    )
    assign(md5, url, envir = session_image_urls)
    url
  }
  record_for_canvas <- function(record) {
    if (is.null(record)) {
      return(NULL)
    }
    url <- session_image_url(record)
    if (is.null(url)) {
      return(NULL)
    }
    record$source_uri <- url
    record$uri <- url
    record
  }
  draft <- shiny::reactiveVal(NULL)
  coordinate_draft <- shiny::reactiveVal(list(rotation_degrees = 0, scale = 1))
  coordinate_baseline <- shiny::reactiveVal(list(
    rotation_degrees = 0,
    scale = 1
  ))
  point_appearance_baseline <- shiny::reactiveVal(NULL)
  point_appearance_input_ready <- shiny::reactiveVal(FALSE)
  coordinate_session_drafts <- shiny::reactiveVal(list())
  roi_coordinate_session_drafts <- shiny::reactiveVal(list())
  roi_point_appearance_drafts <- shiny::reactiveVal(list())
  pending_drafts <- shiny::reactive({
    Filter(
      function(value) length(value) > 0L,
      list(
        coordinates = coordinate_session_drafts(),
        roi_coordinates = roi_coordinate_session_drafts(),
        roi_appearance = roi_point_appearance_drafts()
      )
    )
  })
  active_dataset <- shiny::reactiveVal(NULL)
  active_sample <- shiny::reactiveVal(NULL)
  active_section <- shiny::reactiveVal(NULL)
  roi_view <- shiny::reactiveVal("")
  active_roi <- shiny::reactiveVal("")
  active_image <- shiny::reactiveVal(NULL)
  active_switch_dataset <- shiny::reactiveVal(NULL)
  active_switch_token <- shiny::reactiveVal(NULL)
  pending_project_selection <- shiny::reactiveVal(NULL)
  pending_upload <- shiny::reactiveVal(NULL)
  preview_contract <- shiny::reactiveVal(NULL)
  expected_controls <- shiny::reactiveVal(NULL)
  canvas_generation <- shiny::reactiveVal(0L)
  canvas_reset_token <- shiny::reactiveVal(0L)
  canvas_source_keys <- character()
  canvas_contract <- shiny::reactiveVal(NULL)
  canvas_viewports <- shiny::reactiveVal(NULL)
  image_collection_cache <- new.env(parent = emptyenv())

  output[["enhance-has_image"]] <- shiny::reactive(!is.null(draft()))
  output[["enhance-add_image_label"]] <- shiny::renderUI({
    if (is.null(active_image())) {
      "Add image"
    } else {
      "Add another image"
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
    !is.null(section) && builder_spatial_section_is_spatial(kind_for(section))
  })
  output[["enhance-has_rois"]] <- shiny::reactive({
    entry <- entry_of(current())
    section <- active_section()
    scenes <- entry$profile$spatial_scenes %||% list()
    match <- Filter(function(scene) identical(scene$id, section), scenes)
    length(match) && (match[[1L]]$annotations$roi$count %||% 0L) > 0L
  })
  shiny::outputOptions(
    output,
    "enhance-has_image",
    suspendWhenHidden = FALSE
  )
  shiny::outputOptions(output, "enhance-has_rois", suspendWhenHidden = FALSE)
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

  sync_coordinate_control <- function(id, min, max) {
    slider_id <- paste0("enhance-", id)
    number_id <- paste0(slider_id, "_number")
    shiny::observeEvent(
      input[[number_id]],
      {
        value <- suppressWarnings(as.numeric(input[[number_id]]))
        if (length(value) != 1L || is.na(value) || !is.finite(value)) {
          return()
        }
        if (!is.null(min)) {
          value <- pmax(min, value)
        }
        if (!is.null(max)) {
          value <- pmin(max, value)
        }
        if (!isTRUE(all.equal(input[[number_id]], value))) {
          shiny::freezeReactiveValue(input, number_id)
          shiny::updateNumericInput(session, number_id, value = value)
        }
        if (!isTRUE(all.equal(input[[slider_id]], value))) {
          shiny::updateSliderInput(session, slider_id, value = value)
        }
        if (identical(id, "coordinate_rotation")) {
          entry <- shiny::isolate(entry_of(current()))
          section <- shiny::isolate(active_section())
          if (!is.null(entry) && !is.null(section)) {
            store_coordinate_draft(
              spec = list(rotation_degrees = value, scale = 1),
              dataset = entry$id,
              section = section,
              snapshot_identity = .builder_worker_identity(entry$snapshot),
              force = TRUE,
              roi = shiny::isolate(coordinate_roi())
            )
          }
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input[[slider_id]],
      {
        value <- suppressWarnings(as.numeric(input[[slider_id]]))
        if (
          length(value) == 1L &&
            !is.na(value) &&
            is.finite(value) &&
            !isTRUE(all.equal(input[[number_id]], value))
        ) {
          shiny::freezeReactiveValue(input, number_id)
          shiny::updateNumericInput(session, number_id, value = value)
        }
      },
      ignoreInit = TRUE
    )
  }
  sync_coordinate_control("img_scale", 0, 10)
  sync_coordinate_control("img_rotate", -180, 180)
  sync_coordinate_control("image_opacity", 0, 100)
  sync_coordinate_control("coordinate_rotation", -180, 180)
  sync_coordinate_control("point_opacity", 0, 100)
  sync_coordinate_control("point_size", 1, 12)

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
      builder_profile_spatial_reductions(entry$profile),
      entry$profile$images %||% character(),
      if (has_trekker) "trekker" else character()
    ))
  }
  scene_for <- function(entry, section) {
    scenes <- entry$profile$spatial_scenes %||% list()
    match <- Filter(function(scene) identical(scene$id, section), scenes)
    if (length(match)) match[[1L]] else NULL
  }
  samples_for <- function(entry) {
    values <- unique(unlist(
      lapply(
        entry$profile$spatial_scenes %||% list(),
        function(scene) scene$annotations$sample$values %||% character()
      ),
      use.names = FALSE
    ))
    if (length(values)) values else entry$settings$name %||% entry$id
  }
  sections_for_sample <- function(entry, sample) {
    scenes <- entry$profile$spatial_scenes %||% list()
    matched <- vapply(
      scenes,
      function(scene) {
        sample %in% (scene$annotations$sample$values %||% character())
      },
      logical(1)
    )
    scene_sections <- vapply(scenes, `[[`, character(1), "id")
    sections <- unique(c(
      scene_sections[matched],
      setdiff(sections_for(entry), scene_sections)
    ))
    if (length(sections)) sections else sections_for(entry)
  }
  section_labels_for <- function(entry, sections) {
    stats::setNames(
      sections,
      vapply(
        sections,
        function(section) {
          scene <- scene_for(entry, section)
          scene$label %||% section
        },
        character(1)
      )
    )
  }
  roi_field_for <- function(entry, section) {
    scene_for(entry, section)$annotations$roi$field %||% NULL
  }
  roi_values_for <- function(entry, section) {
    scene_for(entry, section)$annotations$roi$values %||% character()
  }
  kind_for <- function(section) {
    id <- shiny::isolate(current())
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    if (identical(section, "trekker")) {
      "trekker"
    } else if (
      !is.null(entry) &&
        section %in% builder_profile_spatial_reductions(entry$profile)
    ) {
      "spatial_reduction"
    } else {
      "spatial"
    }
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
  image_choices_for <- function(
    entry,
    section,
    roi = active_roi(),
    fallback = TRUE
  ) {
    roi <- as.character(roi %||% "")
    if (length(roi) != 1L || is.na(roi)) {
      roi <- ""
    }
    choices <- builder_image_collection_choices(
      collection_for(entry),
      section,
      roi
    )
    if (length(choices) || !nzchar(roi) || !isTRUE(fallback)) {
      return(choices)
    }
    builder_image_collection_choices(collection_for(entry), section, "")
  }
  image_labels_for <- function(entry, section, roi = active_roi()) {
    unname(image_choices_for(entry, section, roi))
  }
  image_selection_for <- function(entry, section, image = NULL) {
    sections <- sections_for(entry)
    if (!length(sections)) {
      return(NULL)
    }
    if (length(section) != 1L || !section %in% sections) {
      section <- sections[[1L]]
    }
    labels <- image_labels_for(entry, section)
    if (is.null(image) || !image %in% labels) {
      image <- if (length(labels)) labels[[1L]] else NULL
    }
    list(section = section, image = image)
  }
  update_image_choices <- function(entry, section, selected = NULL) {
    choices <- image_choices_for(entry, section)
    shiny::updateSelectInput(
      session,
      "enhance-active_image",
      choices = choices,
      selected = selected %||%
        if (length(choices)) unname(choices[[1L]]) else character()
    )
    invisible(unname(choices))
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
  coordinate_roi <- function(view = roi_view(), roi = active_roi()) {
    if (identical(view, "")) "" else as.character(roi %||% "")[[1L]]
  }
  roi_draft_get <- function(drafts, entry, section, roi) {
    drafts[[entry$id]][[section]][[roi]] %||% NULL
  }
  point_appearance_for <- function(
    entry,
    section,
    record = NULL,
    roi = coordinate_roi()
  ) {
    defaults <- builder_alignment_defaults()
    if (nzchar(roi)) {
      stored <- roi_draft_get(
        roi_point_appearance_drafts(),
        entry,
        section,
        roi
      )
      if (!is.null(stored)) {
        return(utils::modifyList(
          defaults[c("point_opacity", "point_size")],
          stored[c("point_opacity", "point_size")]
        ))
      }
      stored <- entry$settings$spatial_roi_settings[[section]][[roi]] %||%
        list()
      return(utils::modifyList(
        defaults[c("point_opacity", "point_size")],
        stored[c("point_opacity", "point_size")]
      ))
    }
    from_record <- if (is.null(record)) {
      NULL
    } else {
      .builder_alignment_parameters(record)
    }
    if (!is.null(from_record)) {
      return(from_record[c("point_opacity", "point_size")])
    }
    if (nzchar(roi)) {
      return(defaults[c("point_opacity", "point_size")])
    }
    stored <- entry$settings$spatial_point_appearance[[section]] %||% list()
    list(
      point_opacity = stored$point_opacity %||% defaults$point_opacity,
      point_size = stored$point_size %||% defaults$point_size
    )
  }
  coordinate_spec_for <- function(entry, section, roi = coordinate_roi()) {
    if (!builder_spatial_section_is_spatial(kind_for(section))) {
      return(list(rotation_degrees = 0, scale = 1))
    }
    if (nzchar(roi)) {
      record <- roi_draft_get(
        roi_coordinate_session_drafts(),
        entry,
        section,
        roi
      )
      stored <- entry$settings$spatial_roi_settings[[section]][[roi]] %||%
        list()
      return(
        record$spec %||%
          list(
            rotation_degrees = stored$rotation_degrees %||% 0,
            scale = 1
          )
      )
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
    roi_field <- roi_field_for(entry, section)
    list(
      dataset = entry$id,
      snapshot_identity = .builder_worker_identity(entry$snapshot),
      section = section,
      default_projection = entry$settings$default_projection %||% NULL,
      group = roi_field %||% entry$settings$default_group %||% NULL,
      roi = if (identical(roi_view(), "__separate__")) "" else active_roi(),
      assay = entry$settings$assay %||% NULL,
      layer = entry$settings$layer %||% "data"
    )
  }
  alignment_bounds_for <- function(
    preview,
    roi = active_roi()
  ) {
    roi <- as.character(roi %||% "")[[1L]]
    viewport <- shiny::isolate(canvas_viewports())
    contract <- shiny::isolate(canvas_contract())
    key <- if (nzchar(roi)) roi else "__section__"
    if (
      is.list(viewport) &&
        is.list(contract) &&
        identical(viewport$viewKey, contract$viewKey) &&
        isTRUE(
          as.integer(viewport$generation) == as.integer(contract$generation)
        ) &&
        .builder_alignment_valid_bounds(viewport$viewports[[key]])
    ) {
      return(viewport$viewports[[key]])
    }
    if (nzchar(roi)) {
      bounds <- preview$roi_bounds[[roi]] %||% preview$bounds
    } else {
      bounds <- preview$coordinate_frame %||% preview$bounds
    }
    bounds
  }

  shiny::observeEvent(
    input[["builder_spatial_viewports"]],
    {
      event <- input[["builder_spatial_viewports"]]
      contract <- shiny::isolate(canvas_contract())
      if (
        !is.list(event) ||
          !is.list(contract) ||
          !identical(event$viewKey, contract$viewKey) ||
          !isTRUE(
            as.integer(event$generation) == as.integer(contract$generation)
          ) ||
          !is.list(event$viewports)
      ) {
        return()
      }
      valid <- vapply(
        event$viewports,
        .builder_alignment_valid_bounds,
        logical(1)
      )
      event$viewports <- event$viewports[valid]
      canvas_viewports(event)
      entry <- shiny::isolate(entry_of(current()))
      section <- contract$section
      if (is.null(entry) || is.null(section)) {
        return()
      }
      images <- collection_for(entry)
      records <- images[[section]] %||% list()
      changed <- FALSE
      for (label in names(records)) {
        roi <- as.character(records[[label]]$roi_value %||% "")[[1L]]
        key <- if (nzchar(roi)) roi else "__section__"
        bounds <- event$viewports[[key]]
        if (
          .builder_alignment_valid_bounds(bounds) &&
            !isTRUE(all.equal(
              records[[label]]$viewport_bounds,
              bounds,
              check.attributes = FALSE
            ))
        ) {
          records[[label]]$viewport_bounds <- bounds
          changed <- TRUE
          if (
            identical(section, shiny::isolate(active_section())) &&
              identical(label, shiny::isolate(active_image()))
          ) {
            draft(records[[label]])
          }
        }
      }
      if (changed) {
        images[[section]] <- records
        commit_images(entry, images)
        image_collection_cache$dataset <- entry$id
        image_collection_cache$images <- images
        image_collection_cache$known_sections <- unique(c(
          image_collection_cache$known_sections %||% character(),
          section,
          names(images)
        ))
      }
    },
    ignoreInit = TRUE
  )
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
      group = contract$group,
      roi = contract$roi,
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
    ids <- c(
      ids,
      paste0(
        c(
          "enhance-img_scale",
          "enhance-img_rotate",
          "enhance-image_opacity",
          "enhance-point_opacity",
          "enhance-point_size"
        ),
        "_number"
      )
    )
    invisible(lapply(ids, function(id) shiny::freezeReactiveValue(input, id)))
    shiny::updateNumericInput(
      session,
      "enhance-img_dx",
      value = parameters$dx,
      step = ranges$dx$step
    )
    shiny::updateNumericInput(
      session,
      "enhance-img_dy",
      value = parameters$dy,
      step = ranges$dy$step
    )
    shiny::updateSliderInput(
      session,
      "enhance-img_scale",
      value = parameters$scale
    )
    shiny::updateNumericInput(
      session,
      "enhance-img_scale_number",
      value = parameters$scale
    )
    shiny::updateSliderInput(
      session,
      "enhance-img_rotate",
      value = parameters$rotation
    )
    shiny::updateNumericInput(
      session,
      "enhance-img_rotate_number",
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
    shiny::updateNumericInput(
      session,
      "enhance-image_opacity_number",
      value = parameters$image_opacity * 100
    )
    shiny::updateSliderInput(
      session,
      "enhance-point_opacity",
      value = parameters$point_opacity * 100
    )
    shiny::updateNumericInput(
      session,
      "enhance-point_opacity_number",
      value = parameters$point_opacity * 100
    )
    shiny::updateSliderInput(
      session,
      "enhance-point_size",
      value = parameters$point_size
    )
    shiny::updateNumericInput(
      session,
      "enhance-point_size_number",
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
    spec <- coordinate_spec_for(entry, section, coordinate_roi())
    coordinate_draft(spec)
    coordinate_baseline(spec)
    shiny::freezeReactiveValue(input, "enhance-coordinate_rotation")
    shiny::updateSliderInput(
      session,
      "enhance-coordinate_rotation",
      value = spec$rotation_degrees
    )
    shiny::updateNumericInput(
      session,
      "enhance-coordinate_rotation_number",
      value = spec$rotation_degrees
    )
  }
  switch_to <- function(entry, section, label = NULL) {
    pending_upload(NULL)
    active_dataset(entry$id)
    samples <- samples_for(entry)
    sample <- active_sample()
    if (
      is.null(sample) ||
        !sample %in% samples ||
        !section %in% sections_for_sample(entry, sample)
    ) {
      candidates <- samples[vapply(
        samples,
        function(value) {
          section %in% sections_for_sample(entry, value)
        },
        logical(1)
      )]
      sample <- if (length(candidates)) candidates[[1L]] else samples[[1L]]
    }
    active_sample(sample)
    shiny::updateSelectInput(
      session,
      "enhance-active_sample",
      choices = stats::setNames(samples, samples),
      selected = sample
    )
    sample_sections <- sections_for_sample(entry, sample)
    active_section(section)
    rois <- roi_values_for(entry, section)
    default_roi_view <- if (length(rois) > 1L) "__separate__" else ""
    roi_view(default_roi_view)
    active_roi(
      if (identical(default_roi_view, "__separate__")) {
        rois[[1L]]
      } else {
        ""
      }
    )
    shiny::updateSelectInput(
      session,
      "enhance-active_section",
      choices = section_labels_for(entry, sample_sections),
      selected = section
    )
    ## Saving/restoring alignment replaces the outer Configure UI. The client
    ## remembers this authoritative value across that redraw and re-applies it
    ## to the replacement select without generating a second server event.
    session$sendCustomMessage(
      "builder_spatial_section_state",
      list(value = section)
    )
    shiny::updateSelectInput(
      session,
      "enhance-active_roi",
      choices = c(
        "All ROIs" = "",
        "Separate ROIs" = "__separate__",
        stats::setNames(rois, rois)
      ),
      selected = default_roi_view
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
    selected <- image_selection_for(
      entry,
      selection$section,
      selection$image
    )
    if (is.null(selected)) {
      return(invisible(FALSE))
    }
    switch_to(entry, selected$section, selected$image)
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
    roi_pending <- shiny::isolate(roi_coordinate_session_drafts())
    appearance_pending <- shiny::isolate(roi_point_appearance_drafts())
    for (dataset in datasets) {
      roi_pending <- builder_roi_drafts_drop(roi_pending, dataset)
      appearance_pending <- builder_roi_drafts_drop(
        appearance_pending,
        dataset
      )
    }
    roi_coordinate_session_drafts(roi_pending)
    roi_point_appearance_drafts(appearance_pending)
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
    canvas_source_keys <<- character()
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
      selected <- image_selection_for(
        entry,
        selection$section,
        selection$image
      )
      switch_to(entry, selected$section, selected$image)
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
    current_draft <- shiny::isolate(draft())
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
  settled_parameters <- shiny::debounce(parameters, millis = 50)
  store_coordinate_draft <- function(
    spec,
    dataset,
    section,
    snapshot_identity,
    sequence = NULL,
    force = FALSE,
    generation = NULL,
    roi = ""
  ) {
    entry <- shiny::isolate(entry_of(dataset))
    if (
      is.null(entry) ||
        !section %in% sections_for(entry) ||
        !builder_spatial_section_is_spatial(kind_for(section)) ||
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
    if (nzchar(roi)) {
      record <- list(
        dataset = dataset,
        snapshot_identity = snapshot_identity,
        section = section,
        roi = roi,
        spec = .spx_coordinate_transform_spec_normalize(
          spec,
          context = "ROI coordinate draft"
        ),
        sequence = as.numeric(sequence %||% 0)
      )
      drafts <- shiny::isolate(roi_coordinate_session_drafts())
      current_record <- drafts[[dataset]][[section]][[roi]] %||% NULL
      if (
        !isTRUE(force) &&
          !is.null(current_record) &&
          record$sequence <= current_record$sequence
      ) {
        return(invisible(FALSE))
      }
      drafts[[dataset]][[section]][[roi]] <- record
      roi_coordinate_session_drafts(drafts)
      coordinate_draft(record$spec)
      coordinate_baseline(record$spec)
      return(invisible(TRUE))
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
        generation = event$generation %||% NULL,
        roi = as.character(event$roi %||% "")[[1L]]
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
      roi_coordinates <- shiny::isolate(roi_coordinate_session_drafts())
      pruned_roi_coordinates <- builder_roi_drafts_prune(
        roi_coordinates,
        identities
      )
      if (!identical(pruned_roi_coordinates, roi_coordinates)) {
        roi_coordinate_session_drafts(pruned_roi_coordinates)
      }
      roi_appearance <- shiny::isolate(roi_point_appearance_drafts())
      pruned_roi_appearance <- builder_roi_drafts_prune(
        roi_appearance,
        identities
      )
      if (!identical(pruned_roi_appearance, roi_appearance)) {
        roi_point_appearance_drafts(pruned_roi_appearance)
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
  current_record <- function() {
    current_draft <- draft()
    preview <- alignment_preview()
    if (
      is.null(current_draft) ||
        !isTRUE(preview$available)
    ) {
      return(NULL)
    }
    observed <- parameters()
    record <- builder_alignment_record(
      source = current_draft$source,
      base_bounds = current_draft$base_bounds,
      parameters = observed,
      section = list(id = active_section(), kind = preview$section$kind),
      source_path = current_draft$source_path %||% NULL,
      project_asset = current_draft$project_asset %||% NULL
    )
    record$roi_field <- current_draft$roi_field %||% NULL
    record$roi_value <- current_draft$roi_value %||% NULL
    facts <- intersect(
      names(current_draft),
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
    record[facts] <- current_draft[facts]
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

  shiny::observeEvent(input[["enhance-active_sample"]], {
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    sample <- as.character(input[["enhance-active_sample"]] %||% "")
    if (
      is.null(entry) ||
        length(sample) != 1L ||
        is.na(sample) ||
        !sample %in% samples_for(entry) ||
        identical(sample, shiny::isolate(active_sample()))
    ) {
      return()
    }
    active_sample(sample)
    sections <- sections_for_sample(entry, sample)
    switch_to(entry, sections[[1L]])
  })

  shiny::observeEvent(input[["enhance-active_image"]], {
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    label <- input[["enhance-active_image"]]
    previous <- shiny::isolate(active_image())
    section <- shiny::isolate(active_section())
    valid_labels <- if (is.null(entry) || is.null(section)) {
      character()
    } else {
      image_labels_for(entry, section)
    }
    if (
      is.null(entry) ||
        is.null(section) ||
        !nzchar(label) ||
        !label %in% valid_labels ||
        identical(label, previous)
    ) {
      return()
    }
    pending_project_selection(NULL)
    active_image(label)
    restore(entry, section, label)
  })

  shiny::observeEvent(input[["enhance-active_roi"]], {
    id <- current()
    entry <- if (is.null(id)) NULL else shiny::isolate(entry_of(id))
    section <- shiny::isolate(active_section())
    view <- as.character(input[["enhance-active_roi"]] %||% "")
    rois <- roi_values_for(entry, section)
    if (
      is.null(entry) ||
        is.null(section) ||
        length(view) != 1L ||
        is.na(view) ||
        identical(view, shiny::isolate(roi_view()))
    ) {
      return()
    }
    roi_view(view)
    if (identical(view, "__separate__")) {
      roi <- shiny::isolate(active_roi())
      if (!roi %in% rois) {
        roi <- if (length(rois)) rois[[1L]] else ""
      }
      active_roi(roi)
    } else {
      active_roi(view)
    }
    labels <- update_image_choices(entry, section)
    active_image(if (length(labels)) labels[[1L]] else NULL)
    restore(entry, section, active_image())
    restore_coordinate_controls(entry, section)
    request_preview(entry, section)
  })

  shiny::observeEvent(
    input[["builder_spatial_roi_select"]],
    {
      if (!identical(shiny::isolate(roi_view()), "__separate__")) {
        return()
      }
      entry <- shiny::isolate(entry_of(current()))
      section <- shiny::isolate(active_section())
      roi <- as.character(input[["builder_spatial_roi_select"]]$roi %||% "")
      if (
        is.null(entry) ||
          is.null(section) ||
          length(roi) != 1L ||
          is.na(roi) ||
          !roi %in% roi_values_for(entry, section) ||
          identical(roi, shiny::isolate(active_roi()))
      ) {
        return()
      }
      active_roi(roi)
      labels <- update_image_choices(entry, section)
      active_image(if (length(labels)) labels[[1L]] else NULL)
      restore(entry, section, active_image())
      restore_coordinate_controls(entry, section)
    },
    ignoreInit = TRUE
  )

  attach_upload <- function(upload, preview, label = NULL) {
    filename <- basename(as.character(upload$name[[1L]]))
    proposed_label <- trimws(
      label %||%
        builder_safe_file_name(
          filename,
          fallback = "Tissue image"
        )
    )
    dataset <- current()
    section <- active_section()
    materialized <- materialize_coordinate_drafts(
      dataset = dataset,
      section = section,
      notify = TRUE
    )
    if (!isTRUE(materialized$ok)) {
      return(invisible(FALSE))
    }
    entry <- materialized$entries[[dataset]] %||% entry_of(dataset)
    selected_roi <- active_roi()
    existing <- names(collection_for(entry)[[section]]) %||% character()
    existing_scope_labels <- names(image_choices_for(
      entry,
      section,
      selected_roi,
      fallback = FALSE
    )) %||%
      character()
    if (!nzchar(proposed_label) || proposed_label %in% existing_scope_labels) {
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
          "Image labels must be unique within the current ROI."
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
    extension <- if (identical(image$mime, "image/png")) "png" else "jpg"
    session_source <- file.path(
      session_image_dir,
      paste0(image$source_content_md5, ".", extension)
    )
    if (
      !file.exists(session_source) &&
        !file.copy(image$source_path, session_source, overwrite = FALSE)
    ) {
      shiny::showNotification(
        "The uploaded image could not be retained for this Builder session.",
        type = "error",
        duration = 8
      )
      return(invisible(FALSE))
    }
    retained <- builder_read_image(session_source, filename = filename)
    if (
      !is.null(retained$error) ||
        !identical(retained$source_content_md5, image$source_content_md5)
    ) {
      shiny::showNotification(
        "The uploaded image failed its integrity check.",
        type = "error",
        duration = 8
      )
      return(invisible(FALSE))
    }
    image <- retained
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
    appearance <- point_appearance_for(
      entry,
      section,
      previous,
      roi = selected_roi
    )
    parameters[c("point_opacity", "point_size")] <- appearance
    fit_bounds <- alignment_bounds_for(
      preview,
      selected_roi
    )
    stored_appearance <- entry$settings$spatial_point_appearance %||% list()
    stored_appearance[[section]] <- appearance
    entry$settings$spatial_point_appearance <- stored_appearance
    record <- builder_alignment_record(
      source = list(
        name = filename,
        type = image$mime,
        size = image$bytes
      ),
      base_bounds = builder_alignment_fit_bounds(
        fit_bounds,
        c(
          width = image$source_width,
          height = image$source_height
        )
      ),
      parameters = parameters,
      section = preview$section,
      source_path = image$source_path
    )
    record$viewport_bounds <- fit_bounds
    if (nzchar(selected_roi)) {
      record$roi_field <- preview$roi$field
      record$roi_value <- selected_roi
    }
    record$image_label <- proposed_label
    facts <- intersect(
      names(image),
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
    record[facts] <- image[facts]
    record$source_content_md5 <- image$source_content_md5
    image_key <- if (!proposed_label %in% existing) {
      proposed_label
    } else {
      utils::tail(make.unique(c(existing, proposed_label)), 1L)
    }
    draft(record)
    active_image(image_key)
    update_controls(record, record$viewport_bounds)
    committed_images <- commit_section(
      entry,
      section,
      record,
      label = image_key
    )
    entry$settings$images <- committed_images
    update_image_choices(entry, section, selected = image_key)
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
        label %in%
          (names(image_choices_for(
            entry,
            active_section(),
            active_roi(),
            fallback = FALSE
          )) %||%
            character())
    ) {
      shiny::showNotification(
        "Image labels must be non-empty and unique within the current ROI.",
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
        rois <- preview$roi$values %||% character()
        shiny::updateSelectInput(
          session,
          "enhance-active_roi",
          choices = c(
            "All ROIs" = "",
            "Separate ROIs" = "__separate__",
            stats::setNames(rois, rois)
          ),
          selected = roi_view()
        )
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
    entry <- if (is.null(contract)) {
      NULL
    } else {
      shiny::isolate(entry_of(contract$id))
    }
    if (
      is.null(preview) ||
        is.null(contract) ||
        is.null(entry) ||
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
      roi_view(),
      active_roi(),
      sep = "::"
    )
    scene <- builder_spatial_canvas_scene(
      preview = preview,
      colors = colors(),
      record = record_for_canvas(draft()),
      point_appearance = point_appearance_for(
        entry,
        section,
        draft()
      ),
      coordinate_transform = coordinate_draft(),
      roi_point_appearance = stats::setNames(
        lapply(preview$roi$values %||% character(), function(roi) {
          labels <- image_labels_for(entry, section, roi)
          record <- if (length(labels)) {
            builder_alignment_normalize(
              collection_for(entry)[[section]][[labels[[1L]]]],
              section,
              kind_for(section)
            )
          } else {
            NULL
          }
          point_appearance_for(entry, section, record, roi)
        }),
        preview$roi$values %||% character()
      ),
      roi_coordinate_transforms = stats::setNames(
        lapply(preview$roi$values %||% character(), function(roi) {
          spec <- coordinate_spec_for(entry, section, roi)
          list(coordinateRotation = spec$rotation_degrees %||% 0)
        }),
        preview$roi$values %||% character()
      ),
      roi_images = stats::setNames(
        lapply(preview$roi$values %||% character(), function(roi) {
          labels <- image_labels_for(entry, section, roi)
          stats::setNames(
            lapply(labels, function(label) {
              record <- builder_alignment_normalize(
                collection_for(entry)[[section]][[label]],
                section,
                kind_for(section)
              )
              record$active <- identical(roi, active_roi()) &&
                identical(label, active_image())
              record_for_canvas(record)
            }),
            labels
          )
        }),
        preview$roi$values %||% character()
      ),
      layout = if (identical(roi_view(), "__separate__")) {
        "separate"
      } else {
        "overlay"
      },
      active_roi = active_roi(),
      identity = identity,
      generation = generation,
      reset_token = canvas_reset_token(),
      dataset = contract$id,
      snapshot_identity = contract$snapshot_identity,
      section = section
    )
    cached_scene <- builder_spatial_canvas_cache_sources(
      scene,
      canvas_source_keys
    )
    canvas_source_keys <<- cached_scene$known
    scene <- cached_scene$scene
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
    NULL
  })
  output[["enhance-image_file"]] <- shiny::renderUI({
    current_draft <- draft()
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
    observed <- shiny::isolate(settled_parameters())
    if (nzchar(coordinate_roi())) {
      observed[c("point_opacity", "point_size")] <-
        .builder_alignment_parameters(current_draft)[c(
          "point_opacity",
          "point_size"
        )]
    }
    expected <- shiny::isolate(expected_controls())
    if (!is.null(expected)) {
      if (
        !isTRUE(all.equal(
          observed,
          expected,
          check.attributes = FALSE
        ))
      ) {
        return(invisible(FALSE))
      }
      expected_controls(NULL)
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
    draft(next_record)
    entry <- shiny::isolate(entry_of(current()))
    section <- shiny::isolate(active_section())
    if (!nzchar(coordinate_roi())) {
      stored <- entry$settings$spatial_point_appearance %||% list()
      stored[[section]] <- observed[c("point_opacity", "point_size")]
      entry$settings$spatial_point_appearance <- stored
    }
    commit_section(entry, section, next_record)
    invisible(TRUE)
  }
  shiny::observeEvent(
    settled_parameters(),
    commit_alignment_controls(),
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    list(
      input[["enhance-point_opacity"]],
      input[["enhance-point_size"]]
    ),
    {
      if (
        !is.null(shiny::isolate(draft())) &&
          !nzchar(coordinate_roi())
      ) {
        return()
      }
      entry <- shiny::isolate(entry_of(current()))
      section <- shiny::isolate(active_section())
      if (
        is.null(entry) ||
          is.null(section) ||
          !builder_spatial_section_is_spatial(kind_for(section))
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
      roi <- coordinate_roi()
      if (nzchar(roi)) {
        drafts <- shiny::isolate(roi_point_appearance_drafts())
        record <- c(
          list(
            dataset = entry$id,
            snapshot_identity = .builder_worker_identity(entry$snapshot),
            section = section,
            roi = roi
          ),
          next_value
        )
        if (identical(drafts[[entry$id]][[section]][[roi]], record)) {
          return()
        }
        drafts[[entry$id]][[section]][[roi]] <- record
        roi_point_appearance_drafts(drafts)
      } else {
        if (identical(stored[[section]], next_value)) {
          return()
        }
        stored[[section]] <- next_value
        entry$settings$spatial_point_appearance <- stored
        commit_images(entry, collection_for(entry))
      }
      point_appearance_baseline(next_value)
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
    roi_pending <- shiny::isolate(roi_coordinate_session_drafts())
    appearance_pending <- shiny::isolate(roi_point_appearance_drafts())
    target_datasets <- if (is.null(dataset)) {
      Reduce(
        union,
        list(
          names(pending) %||% character(),
          names(roi_pending) %||% character(),
          names(appearance_pending) %||% character()
        )
      )
    } else {
      as.character(dataset)
    }
    materialized_entries <- list()
    for (dataset_id in target_datasets) {
      records <- pending[[dataset_id]] %||% list()
      roi_records <- roi_pending[[dataset_id]] %||% list()
      appearance_records <- appearance_pending[[dataset_id]] %||% list()
      if (!is.null(section)) {
        records <- records[intersect(names(records), section)]
        roi_records <- roi_records[intersect(names(roi_records), section)]
        appearance_records <- appearance_records[
          intersect(names(appearance_records), section)
        ]
      }
      if (
        !length(records) && !length(roi_records) && !length(appearance_records)
      ) {
        next
      }
      entry <- shiny::isolate(entry_of(dataset_id))
      if (is.null(entry)) {
        pending <- builder_coordinate_drafts_drop(pending, dataset_id)
        roi_pending <- builder_roi_drafts_drop(roi_pending, dataset_id)
        appearance_pending <- builder_roi_drafts_drop(
          appearance_pending,
          dataset_id
        )
        coordinate_session_drafts(pending)
        roi_coordinate_session_drafts(roi_pending)
        roi_point_appearance_drafts(appearance_pending)
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
      applied_coordinates <- tryCatch(
        builder_coordinate_drafts_apply_entry(
          entry,
          records,
          snapshot_identity = snapshot_identity
        ),
        error = function(error) error
      )
      applied <- if (inherits(applied_coordinates, "condition")) {
        applied_coordinates
      } else {
        tryCatch(
          builder_roi_drafts_apply_entry(
            applied_coordinates$entry,
            roi_records,
            appearance_records,
            snapshot_identity = snapshot_identity
          ),
          error = function(error) error
        )
      }
      changed <- !inherits(applied, "condition") &&
        (isTRUE(applied_coordinates$changed) || isTRUE(applied$changed))
      committed <- if (inherits(applied, "condition")) {
        applied
      } else if (changed) {
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
      for (materialized_section in union(
        names(roi_records),
        names(appearance_records)
      )) {
        roi_pending <- builder_roi_drafts_drop(
          roi_pending,
          dataset_id,
          materialized_section
        )
        appearance_pending <- builder_roi_drafts_drop(
          appearance_pending,
          dataset_id,
          materialized_section
        )
      }
      coordinate_session_drafts(pending)
      roi_coordinate_session_drafts(roi_pending)
      roi_point_appearance_drafts(appearance_pending)
      latest <- shiny::isolate(entry_of(dataset_id)) %||% applied$entry
      materialized_entries[[dataset_id]] <- latest
      if (
        changed &&
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
        force = TRUE,
        roi = coordinate_roi()
      )
      current_draft <- shiny::isolate(draft())
      roi <- coordinate_roi()
      if (nzchar(roi)) {
        drafts <- shiny::isolate(roi_point_appearance_drafts())
        drafts[[entry$id]][[section]][[roi]] <- c(
          list(
            dataset = entry$id,
            snapshot_identity = .builder_worker_identity(entry$snapshot),
            section = section,
            roi = roi
          ),
          appearance
        )
        roi_point_appearance_drafts(drafts)
      } else if (is.null(current_draft)) {
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
      shiny::updateNumericInput(
        session,
        "enhance-coordinate_rotation_number",
        value = 0
      )
      shiny::updateSliderInput(
        session,
        "enhance-point_opacity",
        value = defaults$point_opacity * 100
      )
      shiny::updateNumericInput(
        session,
        "enhance-point_opacity_number",
        value = defaults$point_opacity * 100
      )
      shiny::updateSliderInput(
        session,
        "enhance-point_size",
        value = defaults$point_size
      )
      shiny::updateNumericInput(
        session,
        "enhance-point_size_number",
        value = defaults$point_size
      )
      canvas_reset_token(canvas_reset_token() + 1L)
    }
  )
  shiny::observeEvent(input[["enhance-center_image"]], {
    current_draft <- draft()
    preview <- alignment_preview()
    if (is.null(current_draft) || !isTRUE(preview$available)) {
      return()
    }
    bounds <- alignment_bounds_for(
      preview
    )
    centered <- builder_alignment_center(current_draft, bounds)
    draft(centered)
    canvas_reset_token(canvas_reset_token() + 1L)
    update_controls(centered, bounds)
    commit_section(entry_of(current()), active_section(), centered)
  })
  shiny::observeEvent(input[["enhance-reset_align"]], {
    current_draft <- draft()
    preview <- alignment_preview()
    if (is.null(current_draft) || !isTRUE(preview$available)) {
      return()
    }
    bounds <- alignment_bounds_for(
      preview
    )
    reset <- builder_alignment_reset(current_draft)
    image_dimensions <- c(
      width = reset$source_width %||%
        (reset$base_bounds$xmax - reset$base_bounds$xmin),
      height = reset$source_height %||%
        (reset$base_bounds$ymax - reset$base_bounds$ymin)
    )
    reset$base_bounds <- builder_alignment_fit_bounds(
      bounds,
      image_dimensions
    )
    reset <- builder_alignment_center(reset, bounds)
    draft(reset)
    canvas_reset_token(canvas_reset_token() + 1L)
    update_controls(reset, bounds)
    commit_section(entry_of(current()), active_section(), reset)
  })
  show_remove_image <- function() {
    entry <- entry_of(current())
    section <- active_section()
    label <- active_image()
    if (is.null(entry) || is.null(section) || is.null(label)) {
      return(invisible(FALSE))
    }
    display_label <- collection_for(entry)[[section]][[label]]$image_label %||%
      label
    shiny::showModal(shiny::modalDialog(
      title = "Remove image?",
      shiny::p(paste0(
        "Remove “",
        display_label,
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
    entry <- entry_of(current())
    section <- active_section()
    label <- active_image()
    if (is.null(entry) || is.null(section) || is.null(label)) {
      return()
    }
    display_label <- collection_for(entry)[[section]][[label]]$image_label %||%
      label
    shiny::showModal(shiny::modalDialog(
      title = "Rename image",
      shiny::textInput(
        "enhance-renamed_image_label",
        "Image label",
        display_label
      ),
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
    renamed_key <- attr(images, "renamed_image_key") %||% renamed
    committed <- commit_images(entry, images)
    if (is.list(committed) && !is.null(committed$settings$images)) {
      images <- builder_image_collection_normalize(committed$settings$images)
    }
    active_image(renamed_key)
    image_collection_cache$dataset <- entry$id
    image_collection_cache$images <- images
    entry$settings$images <- images
    update_image_choices(entry, section, selected = renamed_key)
    shiny::removeModal()
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
    active_sample = active_sample,
    active_section = active_section,
    roi_view = roi_view,
    active_roi = active_roi,
    active_image = active_image,
    project_selection = project_selection,
    draft = draft,
    point_appearance = point_appearance,
    coordinate_drafts = coordinate_session_drafts,
    pending_drafts = pending_drafts,
    canvas_contract = canvas_contract,
    pending_upload = pending_upload,
    request_dataset_switch = request_dataset_switch,
    fail_preview_switch = fail_preview_switch,
    restore_project_settings = restore_project_settings,
    restore_project_selection = restore_project_selection,
    materialize_coordinate_drafts = materialize_coordinate_drafts,
    current_record = current_record
  )
}
