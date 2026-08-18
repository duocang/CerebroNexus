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
builder_session_start <- function(
  builder_dir,
  snapshot_root = NULL,
  snapshot_registry = list(),
  .async = FALSE,
  .bootstrap = NULL
) {
  worker <- builder_worker_start(
    builder_dir = builder_dir,
    snapshot_root = snapshot_root,
    snapshot_registry = snapshot_registry,
    .async = .async,
    .bootstrap = .bootstrap
  )
  if (!is.null(worker$error)) {
    return(list(error = worker$error))
  }
  list(worker = worker, session = worker$process)
}

builder_session_poll_startup <- function(worker, timeout = 0) {
  builder_worker_poll_startup(worker, timeout = timeout)
}

.builder_session_process <- function(worker) {
  if (inherits(worker, "builder_worker")) {
    return(worker$process)
  }
  worker
}

#' Load one of the built-in examples, without a file.
builder_session_example <- function(
  worker,
  id,
  example_id,
  request = NULL,
  progress_path = NULL,
  import_generation = 1L,
  .importer = NULL
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(
      id,
      example_id,
      request,
      progress_path,
      import_generation,
      importer
    ) {
      progress <- .builder_import_progress_callback(
        progress_path,
        import_generation
      )
      tryCatch(
        {
          if (is.function(importer)) {
            value <- importer(id, example_id, progress)
            return(builder_worker_response(request, value))
          }
          ex <- Filter(
            function(e) identical(e$id, example_id),
            builder_examples()
          )
          if (!length(ex)) {
            stop("This example does not exist.", call. = FALSE)
          }
          made <- ex[[1]]$make()
          if (!is.null(made$error)) {
            stop(made$error, call. = FALSE)
          }
          value <- .builder_register_adapter(
            builder_example_adapter(example_id, made$object),
            id,
            progress = progress
          )
          builder_worker_response(request, value)
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      example_id = example_id,
      request = request,
      progress_path = progress_path,
      import_generation = import_generation,
      importer = .importer
    )
  )
}

#' Ask the worker to load a file and describe it.
#'
#' The object stays there; what comes back is a profile of a few kilobytes.
builder_session_load <- function(
  worker,
  id,
  path,
  request = NULL,
  progress_path = NULL,
  import_generation = 1L,
  .importer = NULL
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(
      id,
      path,
      request,
      progress_path,
      import_generation,
      importer
    ) {
      progress <- .builder_import_progress_callback(
        progress_path,
        import_generation
      )
      tryCatch(
        {
          if (is.function(importer)) {
            value <- importer(id, path, progress)
            return(builder_worker_response(request, value))
          }
          value <- .builder_register_adapter(
            builder_seurat_file_adapter(path),
            id,
            progress = progress
          )
          builder_worker_response(request, value)
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      path = path,
      request = request,
      progress_path = progress_path,
      import_generation = import_generation,
      importer = .importer
    )
  )
}

#' Coordinates for the projection preview: two columns and a label, sampled.
builder_session_preview <- function(
  worker,
  id,
  reduction,
  group,
  max_cells,
  request = NULL
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(id, reduction, group, max_cells, request) {
      tryCatch(
        {
          builder_worker_require_capability("analysis")
          obj <- get(id, envir = get(".builder_objects", envir = globalenv()))
          builder_worker_response(
            request,
            builder_preview_frame(obj, reduction, group, max_cells)
          )
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      reduction = reduction,
      group = group,
      max_cells = max_cells,
      request = request
    )
  )
}

#' Ask the worker for all bounded projection thumbnails in one pass.
builder_session_projection_previews <- function(
  worker,
  id,
  projections,
  group,
  max_cells = 600L,
  request = NULL
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(id, projections, group, max_cells, request) {
      tryCatch(
        {
          builder_worker_require_capability("analysis")
          object <- get(
            id,
            envir = get(".builder_objects", envir = globalenv())
          )
          builder_worker_response(
            request,
            builder_projection_preview_catalog(
              object,
              projections = projections,
              group = group,
              max_cells = max_cells
            )
          )
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      projections = projections,
      group = group,
      max_cells = max_cells,
      request = request
    )
  )
}

#' Ask the worker for bounded trajectory coordinates and edges.
builder_session_trajectory_previews <- function(
  worker,
  id,
  trajectories,
  max_cells = 600L,
  request = NULL
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(id, trajectories, max_cells, request) {
      tryCatch(
        {
          builder_worker_require_capability("analysis")
          object <- get(
            id,
            envir = get(".builder_objects", envir = globalenv())
          )
          builder_worker_response(
            request,
            builder_trajectory_preview_catalog(
              object,
              trajectories = trajectories,
              max_cells = max_cells
            )
          )
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      trajectories = trajectories,
      max_cells = max_cells,
      request = request
    )
  )
}

#' Spatial coordinates, for deciding where a background image sits.
builder_session_coords <- function(worker, id, image = NULL, request = NULL) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(id, image, request) {
      tryCatch(
        {
          builder_worker_require_capability("spatial")
          obj <- get(id, envir = get(".builder_objects", envir = globalenv()))
          co <- builder_spatial_coords(obj, image)
          if (is.null(co)) {
            return(builder_worker_response(request, NULL))
          }
          ## Sampled: the alignment preview needs a shape, not every cell.
          n <- length(co[[1]])
          keep <- if (n > 4000) {
            set.seed(7)
            sort(sample.int(n, 4000))
          } else {
            seq_len(n)
          }
          builder_worker_response(
            request,
            list(
              x = co[[1]],
              y = co[[2]],
              sx = co[[1]][keep],
              sy = co[[2]][keep]
            )
          )
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(id = id, image = image, request = request)
  )
}

#' Paired transcriptome and physical coordinates for the alignment workbench.
builder_session_spatial_preview <- function(
  worker,
  id,
  default_projection = NULL,
  group = NULL,
  section_id = NULL,
  assay = NULL,
  layer = "data",
  coordinate_transforms = NULL,
  max_cells = 4000L,
  request = NULL
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(
      id,
      default_projection,
      group,
      section_id,
      assay,
      layer,
      coordinate_transforms,
      max_cells,
      request
    ) {
      tryCatch(
        {
          builder_worker_require_capability("spatial")
          object <- get(
            id,
            envir = get(".builder_objects", envir = globalenv())
          )
          builder_worker_response(
            request,
            builder_alignment_preview_model(
              object,
              default_projection = default_projection,
              group = group,
              section_id = section_id,
              assay = assay,
              layer = layer,
              coordinate_transforms = coordinate_transforms,
              max_cells = max_cells
            )
          )
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      default_projection = default_projection,
      group = group,
      section_id = section_id,
      assay = assay,
      layer = layer,
      coordinate_transforms = coordinate_transforms,
      max_cells = max_cells,
      request = request
    )
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
  worker,
  id,
  sections,
  mode,
  extent_width,
  extent_height,
  um_per_px = 1,
  dx = 0,
  dy = 0,
  scale = 1,
  request = NULL
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(
      id,
      sections,
      mode,
      extent_width,
      extent_height,
      um_per_px,
      dx,
      dy,
      scale,
      request
    ) {
      tryCatch(
        {
          builder_worker_require_capability("spatial")
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
              list(
                extent_width = extent_width,
                extent_height = extent_height
              ),
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
          builder_worker_response(request, out)
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      sections = sections,
      mode = mode,
      extent_width = extent_width,
      extent_height = extent_height,
      um_per_px = um_per_px,
      dx = dx,
      dy = dy,
      scale = scale,
      request = request
    )
  )
}

.builder_session_plan_snapshot_error <- function(plan, snapshot_registry) {
  if (!is.list(plan) || !is.list(plan$items) || !length(plan$items)) {
    return("BuildPlan does not contain any frozen dataset snapshots.")
  }
  if (!is.list(snapshot_registry)) {
    return("The worker snapshot registry is unavailable.")
  }
  for (item in plan$items) {
    id <- item$id
    label <- item$name
    if (
      !is.character(id) ||
        length(id) != 1L ||
        is.na(id) ||
        !nzchar(id)
    ) {
      return("BuildPlan contains a dataset without a stable id.")
    }
    if (!is.character(label) || length(label) != 1L || is.na(label)) {
      label <- id
    }
    if (is.list(item$reused_artifact)) {
      next
    }
    snapshot <- snapshot_registry[[id]]
    if (is.null(snapshot)) {
      return(paste0("The frozen snapshot is missing for ", label, "."))
    }
    identity <- item$source_snapshot_identity
    expected <- if (is.list(identity)) identity$snapshot else NULL
    if (
      !is.list(identity) ||
        !isTRUE(identity$available) ||
        !is.list(expected)
    ) {
      return(paste0(
        "BuildPlan does not contain an owned frozen snapshot for ",
        label,
        "."
      ))
    }
    mirrored <- all(vapply(
      names(expected),
      function(field) identical(identity[[field]], expected[[field]]),
      logical(1)
    ))
    if (!identical(snapshot, expected) || !mirrored) {
      return(paste0(
        "The worker snapshot for ",
        label,
        " does not match the frozen BuildPlan. Rebuild the plan and retry."
      ))
    }
  }
  NULL
}

#' Execute a frozen plan inside a coordinator-assigned private stage.
builder_session_build <- function(
  worker,
  plan,
  request = NULL,
  coordinator = NULL,
  auth_material = NULL
) {
  on.exit(auth_material <- NULL, add = TRUE)
  rs <- .builder_session_process(worker)
  validate_snapshots <- inherits(worker, "builder_worker")
  snapshots <- if (validate_snapshots) worker$snapshot_registry else list()
  if (isTRUE(plan$make_app)) {
    current_contract <- builder_installed_app_contract_version()
    if (
      !identical(plan$app_contract_version, 1L) ||
        !identical(current_contract, 1L)
    ) {
      return(rs$call(
        function(request) {
          error <- paste0(
            "Private app publication contract v1 is not available. ",
            "Build CRB-only output for now."
          )
          if (is.null(request)) {
            return(list(error = error))
          }
          builder_worker_response(request, error = error)
        },
        args = list(request = request)
      ))
    }
  }
  if (validate_snapshots) {
    snapshot_error <- .builder_session_plan_snapshot_error(plan, snapshots)
    if (!is.null(snapshot_error)) {
      rs$call(
        function(request, error) {
          builder_worker_response(request, error = error)
        },
        args = list(request = request, error = snapshot_error)
      )
      return(invisible(NULL))
    }
  }

  assigned_stage <- !is.null(coordinator)
  if (assigned_stage) {
    coordinator <- tryCatch(
      .builder_coordinator_handle(coordinator),
      error = function(error) error
    )
    if (
      inherits(coordinator, "condition") ||
        !dir.exists(coordinator$stage) ||
        !.pathWithin(coordinator$stage, coordinator$control)
    ) {
      stop(
        "The parent release coordinator assigned an invalid stage.",
        call. = FALSE
      )
    }
    stage <- coordinator$stage
  } else {
    stage_root <- if (validate_snapshots) worker$snapshot_root else tempdir()
    stage <- tempfile("builder-build-stage-", tmpdir = stage_root)
  }
  if (
    !assigned_stage &&
      !dir.create(stage, mode = "0700", showWarnings = FALSE)
  ) {
    rs$call(
      function(request) {
        builder_worker_response(
          request,
          error = "The coordinator could not create the assigned build stage."
        )
      },
      args = list(request = request)
    )
    return(invisible(NULL))
  }
  if (validate_snapshots && !assigned_stage) {
    owner <- tryCatch(.builder_stage_owner(stage), error = function(error) {
      error
    })
    if (inherits(owner, "condition")) {
      unlink(stage, recursive = TRUE, force = TRUE)
      rs$call(
        function(request, error) {
          builder_worker_response(request, error = error)
        },
        args = list(request = request, error = conditionMessage(owner))
      )
      return(invisible(NULL))
    }
  }
  if (isTRUE(plan$app_auth$enabled)) {
    auth_material <- builder_auth_validate_material(auth_material, stage)
  } else if (!is.null(auth_material)) {
    stop("A public build cannot use authentication material.", call. = FALSE)
  }

  rs$call(
    function(
      plan,
      stage,
      request,
      validate_snapshots,
      snapshot_validator,
      auth_material
    ) {
      on.exit(auth_material <- NULL, add = TRUE)
      tryCatch(
        {
          builder_worker_require_capability("build")
          registry <- if (isTRUE(validate_snapshots)) {
            snapshot_environment <- get(
              ".builder_snapshots",
              envir = globalenv()
            )
            ids <- ls(snapshot_environment, all.names = TRUE)
            if (length(ids)) {
              mget(
                ids,
                envir = snapshot_environment,
                inherits = FALSE
              )
            } else {
              list()
            }
          } else {
            list()
          }
          if (isTRUE(validate_snapshots)) {
            snapshot_error <- snapshot_validator(plan, registry)
            if (!is.null(snapshot_error)) {
              return(builder_worker_response(
                request,
                error = snapshot_error
              ))
            }
          }
          value <- builder_execute_plan(
            plan,
            stage,
            registry,
            auth_material = auth_material
          )
          if (
            isTRUE(plan$make_app) &&
              is.list(value) &&
              is.list(request)
          ) {
            value$build_id <- request$build_id
          }
          builder_worker_response(request, value = value)
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      plan = plan,
      stage = stage,
      request = request,
      validate_snapshots = validate_snapshots,
      snapshot_validator = .builder_session_plan_snapshot_error,
      auth_material = auth_material
    )
  )
}

#' Forget one object, so removing a data set actually frees its memory.
builder_session_drop <- function(worker, id, request = NULL) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(id, request) {
      tryCatch(
        {
          store <- get(".builder_objects", envir = globalenv())
          snapshots <- get(".builder_snapshots", envir = globalenv())
          if (exists(id, envir = store)) {
            rm(list = id, envir = store)
            gc(FALSE)
          }
          if (exists(id, envir = snapshots, inherits = FALSE)) {
            rm(list = id, envir = snapshots)
          }
          gc(FALSE)
          builder_worker_response(request, TRUE)
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(id = id, request = request)
  )
}

#' Has the pending call finished? Returns NULL while it is still running.
#'
#' `read()` also delivers interim messages -- anything the worker printed --
#' before the one carrying the result. Treating those as the answer is how a
#' panel ends up permanently empty while the worker has in fact finished, so
#' only the completion code counts.
builder_session_poll <- function(
  worker,
  timeout = 0,
  now = Sys.time()
) {
  builder_worker_poll(worker, timeout = timeout, now = now)
}
