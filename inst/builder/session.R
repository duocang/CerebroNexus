##----------------------------------------------------------------------------##
## The worker process that actually holds the Seurat objects.
##
## Reading a real object into the Shiny process has two costs, and the second
## one is worse than it sounds: the memory is obvious, but a multi-GB readRDS
## or a marker-gene run also *blocks* R, and a blocked R process cannot answer
## the browser at all. The page stops responding, the progress bar stops
## moving, and a Stop button would be a lie because there is nothing left to
## service the click.
##
## So the objects live in a separate R process and this file is the only thing
## that talks to it. The Shiny side sees profiles, coordinate samples and log
## lines -- all small -- and never an object.
##
## The worker holds one environment keyed by data set id. Calls are queued one
## at a time; the page polls for the answer instead of waiting for it.
##----------------------------------------------------------------------------##

#' Start the worker, or explain why it cannot start.
builder_session_start <- function(builder_dir) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    return(list(
      error = paste0(
        "The callr package is required for background processing ",
        "(install.packages(\"callr\"))."
      )
    ))
  }
  rs <- try(callr::r_session$new(), silent = TRUE)
  if (inherits(rs, "try-error")) {
    return(list(
      error = paste0(
        "Could not start the background worker: ",
        conditionMessage(attr(rs, "condition"))
      )
    ))
  }
  ## Give the worker the same helpers the page uses, so "what the object looks
  ## like" is decided by one piece of code rather than two that drift.
  setup <- rs$run(
    function(dir) {
      suppressMessages({
        library(CerebroNexus)
      })
      source(file.path(dir, "io.R"))
      source(file.path(dir, "inspect.R"))
      source(file.path(dir, "preview.R"))
      source(file.path(dir, "extras.R"))
      source(file.path(dir, "analysis.R"))
      source(file.path(dir, "plan.R"))
      source(file.path(dir, "publish.R"))
      .builder_objects <- new.env(parent = emptyenv())
      assign(".builder_objects", .builder_objects, envir = globalenv())
      TRUE
    },
    args = list(dir = builder_dir)
  )
  if (!isTRUE(setup)) {
    return(list(error = "The background worker could not be initialized."))
  }
  list(session = rs)
}

#' Load one of the built-in examples, without a file.
builder_session_example <- function(rs, id, example_id) {
  rs$call(
    function(id, example_id) {
      ex <- Filter(function(e) identical(e$id, example_id), builder_examples())
      if (!length(ex)) {
        return(list(error = "This example does not exist."))
      }
      made <- ex[[1]]$make()
      if (!is.null(made$error)) {
        return(list(error = made$error))
      }
      assign(
        id,
        made$object,
        envir = get(".builder_objects", envir = globalenv())
      )
      list(
        profile = describe_seurat(made$object),
        format = made$format,
        levels = builder_group_levels_for(
          made$object,
          describe_seurat(made$object)$group_candidates
        )
      )
    },
    args = list(id = id, example_id = example_id)
  )
}

#' Ask the worker to load a file and describe it.
#'
#' The object stays there; what comes back is a profile of a few kilobytes.
builder_session_load <- function(rs, id, path) {
  rs$call(
    function(id, path) {
      read <- builder_read_object(path)
      if (!is.null(read$error)) {
        return(list(error = read$error))
      }
      assign(
        id,
        read$object,
        envir = get(".builder_objects", envir = globalenv())
      )
      list(
        profile = describe_seurat(read$object),
        format = read$format,
        levels = builder_group_levels_for(
          read$object,
          describe_seurat(read$object)$group_candidates
        )
      )
    },
    args = list(id = id, path = path)
  )
}

#' Coordinates for the projection preview: two columns and a label, sampled.
builder_session_preview <- function(rs, id, reduction, group, max_cells) {
  rs$call(
    function(id, reduction, group, max_cells) {
      obj <- get(id, envir = get(".builder_objects", envir = globalenv()))
      builder_preview_frame(obj, reduction, group, max_cells)
    },
    args = list(
      id = id,
      reduction = reduction,
      group = group,
      max_cells = max_cells
    )
  )
}

#' Spatial coordinates, for deciding where a background image sits.
builder_session_coords <- function(rs, id, image = NULL) {
  rs$call(
    function(id, image) {
      obj <- get(id, envir = get(".builder_objects", envir = globalenv()))
      co <- builder_spatial_coords(obj, image)
      if (is.null(co)) {
        return(NULL)
      }
      ## Sampled: the alignment preview needs a shape, not every cell.
      n <- length(co[[1]])
      keep <- if (n > 4000) {
        set.seed(7)
        sort(sample.int(n, 4000))
      } else {
        seq_len(n)
      }
      list(x = co[[1]], y = co[[2]], sx = co[[1]][keep], sy = co[[2]][keep])
    },
    args = list(id = id, image = image)
  )
}

#' Where one slide scan lands on EVERY section, section by section.
#'
#' "Apply to all" cannot copy the extent it computed for the section on screen.
#' In bounding-box mode the extent *means* "fit this section's cells", and
#' sections sit at different offsets -- copying section one's numbers puts the
#' other slides thousands of units away, selected but invisible. So the extent
#' is re-derived per section here, where the coordinates live, and only the
#' geometry travels: the encoded picture stays in the Shiny process rather than
#' making a round trip per section.
#'
#' @return A named list, one entry per section, each `list(bounds =, outside =,
#'   total =)`.
builder_session_section_bounds <- function(
  rs,
  id,
  sections,
  mode,
  width,
  height,
  um_per_px = 1,
  dx = 0,
  dy = 0,
  scale = 1
) {
  rs$call(
    function(id, sections, mode, width, height, um_per_px, dx, dy, scale) {
      obj <- get(id, envir = get(".builder_objects", envir = globalenv()))
      out <- list()
      for (nm in sections) {
        co <- builder_spatial_coords(obj, nm)
        if (is.null(co)) {
          next
        }
        b0 <- builder_image_bounds(
          mode,
          co,
          list(width = width, height = height),
          um_per_px = um_per_px
        )
        if (!is.null(b0$error)) {
          next
        }
        bounds <- builder_adjust_bounds(b0, dx = dx, dy = dy, scale = scale)
        out[[nm]] <- list(
          bounds = bounds,
          cover = builder_bounds_cover(bounds, co)
        )
      }
      out
    },
    args = list(
      id = id,
      sections = sections,
      mode = mode,
      width = width,
      height = height,
      um_per_px = um_per_px,
      dx = dx,
      dy = dy,
      scale = scale
    )
  )
}

#' Run the whole export in the worker.
#'
#' Everything expensive happens there: the optional analyses, the matrix
#' write, the bundle. What comes back is a report.
builder_session_build <- function(rs, plan) {
  rs$call(
    function(plan) {
      out <- list(
        built = character(),
        labels = character(),
        failures = character(),
        analysis_log = character(),
        carried = character(),
        colors = list(),
        app_dir = NULL
      )
      if (
        !dir.exists(plan$out_dir) &&
          !dir.create(plan$out_dir, showWarnings = FALSE, recursive = TRUE)
      ) {
        return(list(
          error = paste0(
            "Cannot create the output directory: ",
            plan$out_dir
          )
        ))
      }
      stage_dir <- tempfile(".cerebro-builder-", tmpdir = plan$out_dir)
      if (!dir.create(stage_dir)) {
        return(list(error = "Cannot create a staging directory."))
      }
      on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)
      store <- get(".builder_objects", envir = globalenv())

      for (item in plan$items) {
        obj <- get(item$id, envir = store)
        obj@reductions <- obj@reductions[item$reductions]
        prepared <- try(
          builder_prepare_export_layer(obj, item$assay, item$layer),
          silent = TRUE
        )
        if (inherits(prepared, "try-error")) {
          out$failures <- c(
            out$failures,
            paste0(
              item$name,
              ": ",
              conditionMessage(attr(prepared, "condition"))
            )
          )
          next
        }
        obj <- prepared

        ran <- builder_run_analyses(obj, item$analyses, item)
        obj <- ran$object
        if (length(ran$log)) {
          out$analysis_log <- c(
            out$analysis_log,
            paste0(item$name, ": ", ran$log)
          )
        }
        obj <- builder_attach_tables(obj, item$tables)

        crb <- file.path(stage_dir, item$filename)
        res <- try(
          CerebroNexus::exportFromSeurat(
            object = obj,
            assay = item$assay,
            slot = item$layer,
            file = crb,
            experiment_name = item$name,
            organism = item$organism,
            groups = item$groups,
            nUMI = item$nUMI,
            nGene = item$nGene,
            verbose = FALSE
          ),
          silent = TRUE
        )
        if (inherits(res, "try-error")) {
          out$failures <- c(
            out$failures,
            paste0(
              item$name,
              ": ",
              conditionMessage(attr(res, "condition"))
            )
          )
          next
        }
        out$built <- c(out$built, crb)
        out$labels <- c(out$labels, item$name)
        if (length(item$colors)) {
          ## Keyed by the SAME label passed as the dataset name below. The
          ## viewer maps configuration to data set by exact string comparison,
          ## so a label that differs by one character drops the whole palette.
          out$colors[[item$name]] <- item$colors
        }

        trek <- tryCatch(obj@misc$trekker, error = function(e) NULL)
        attached <- builder_attach_crb_extras(crb, item$images, trek)
        if (!is.null(attached$error)) {
          out$failures <- c(
            out$failures,
            paste0(item$name, ": ", attached$error)
          )
          next
        }
        if (isTRUE(attached$trekker)) {
          out$carried <- c(out$carried, paste0(item$name, ": Trekker"))
        }
      }

      if (length(out$failures) || length(out$built) != length(plan$items)) {
        out$error <- paste0(
          "Nothing was published because ",
          length(out$failures),
          " dataset build",
          if (length(out$failures) == 1L) "" else "s",
          " failed."
        )
        out$built <- character()
        out$labels <- character()
        return(out)
      }

      staged_targets <- out$built
      final_targets <- file.path(
        plan$out_dir,
        vapply(plan$items, `[[`, "", "filename")
      )

      if (isTRUE(plan$make_app)) {
        app_dir <- file.path(stage_dir, "cerebro_app")
        made <- try(
          CerebroNexus::createShinyApp(
            cerebro_data = stats::setNames(out$built, out$labels),
            result_dir = app_dir,
            colors = if (length(out$colors)) out$colors else NULL,
            launch_browser = FALSE,
            verbose = FALSE
          ),
          silent = TRUE
        )
        if (inherits(made, "try-error")) {
          out$failures <- c(
            out$failures,
            paste0(
              "App bundle: ",
              conditionMessage(attr(made, "condition"))
            )
          )
        } else {
          out$app_dir <- app_dir
          staged_targets <- c(staged_targets, app_dir)
          final_targets <- c(
            final_targets,
            file.path(plan$out_dir, "cerebro_app")
          )
        }
      }

      if (length(out$failures)) {
        out$error <- "Nothing was published because app bundling failed."
        out$built <- character()
        out$labels <- character()
        out$app_dir <- NULL
        return(out)
      }

      published <- builder_publish_batch(
        staged_targets,
        final_targets,
        overwrite = isTRUE(plan$overwrite)
      )
      if (!is.null(published$error)) {
        out$error <- published$error
        out$built <- character()
        out$labels <- character()
        out$app_dir <- NULL
        return(out)
      }

      out$built <- final_targets[seq_along(plan$items)]
      if (isTRUE(plan$make_app)) {
        out$app_dir <- final_targets[length(final_targets)]
      }
      out
    },
    args = list(plan = plan)
  )
}

#' Forget one object, so removing a data set actually frees its memory.
builder_session_drop <- function(rs, id) {
  rs$call(
    function(id) {
      store <- get(".builder_objects", envir = globalenv())
      if (exists(id, envir = store)) {
        rm(list = id, envir = store)
      }
      gc(FALSE)
      TRUE
    },
    args = list(id = id)
  )
}

#' Has the pending call finished? Returns NULL while it is still running.
#'
#' `read()` also delivers interim messages -- anything the worker printed --
#' before the one carrying the result. Treating those as the answer is how a
#' panel ends up permanently empty while the worker has in fact finished, so
#' only the completion code counts.
builder_session_poll <- function(rs, timeout = 0) {
  state <- rs$poll_process(timeout)
  if (!identical(as.character(state)[1], "ready")) {
    return(NULL)
  }
  msg <- rs$read()
  if (is.null(msg)) {
    return(NULL)
  }
  code <- msg$code
  if (is.null(code)) {
    code <- 200L
  }
  if (code == 200L) {
    if (!is.null(msg$error)) {
      return(list(error = conditionMessage(msg$error)))
    }
    return(list(value = msg$result, done = TRUE))
  }
  if (code >= 500L) {
    return(list(
      error = paste0(
        "Background worker error (code ",
        code,
        ")."
      )
    ))
  }
  ## 301 and friends: the worker wrote something. Keep waiting.
  NULL
}
