#' Remove Common Leading Whitespace from a String
#'
#' Eliminates the minimal common indentation shared by all non-empty lines of
#' the input, preserving relative indentation within blocks.
#'
#' @param string A character string containing text with indentation.
#' @return A dedented character string.
#' @keywords internal
#' @noRd
dedent <- function(string) {
  if (!is.character(string) || length(string) != 1) {
    stop("Input must be a single character string")
  }
  lines <- strsplit(string, "\n", fixed = TRUE)[[1]]
  while (length(lines) > 0 && grepl("^\\s*$", lines[1])) {
    lines <- lines[-1]
  }
  while (length(lines) > 0 && grepl("^\\s*$", lines[length(lines)])) {
    lines <- lines[-length(lines)]
  }
  if (length(lines) == 0) {
    return("")
  }
  non_empty_lines <- lines[!grepl("^\\s*$", lines)]
  if (length(non_empty_lines) == 0) {
    return("")
  }
  lead_spaces <- vapply(
    non_empty_lines,
    function(line) {
      m <- regmatches(line, regexpr("^\\s*", line))
      nchar(m)
    },
    integer(1)
  )
  min_indent <- min(lead_spaces)
  if (min_indent > 0) {
    pat <- paste0("^\\s{", min_indent, "}")
    lines <- vapply(
      lines,
      function(line) {
        if (grepl("^\\s*$", line)) line else sub(pat, "", line)
      },
      character(1)
    )
  }
  paste(lines, collapse = "\n")
}

.portableBundlePath <- function(path, subject) {
  valid <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
    !grepl("\\\\", path) &&
    !grepl("^(/|~|[A-Za-z]:)", path)
  parts <- if (valid) {
    strsplit(path, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  windows_invalid <- length(parts) > 0L &&
    any(
      grepl("[[:cntrl:]<>:\"|?*]", parts) |
        grepl("[. ]$", parts) |
        grepl(
          "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])($|\\.)",
          parts,
          ignore.case = TRUE
        )
    )
  if (
    !valid ||
      length(parts) == 0L ||
      any(!nzchar(parts)) ||
      any(parts %in% c(".", "..")) ||
      windows_invalid
  ) {
    stop(
      subject,
      " must be one portable relative path using forward slashes, without ",
      "empty, '.', '..', or Windows-incompatible segments.",
      call. = FALSE
    )
  }
  paste(parts, collapse = "/")
}

.readBundleBackend <- function(crb_path) {
  object <- readRDS(crb_path)
  recognized <- is.environment(object) &&
    "Cerebro_v1.3" %in% class(object)
  if (!recognized) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' does not contain a recognized Cerebro object.",
      call. = FALSE
    )
  }
  getter <- if (is.environment(object) || is.list(object)) {
    object[["getExpressionBackend"]]
  } else {
    NULL
  }
  if (is.null(getter)) {
    return(list(type = "embedded", location = NULL, legacy = TRUE))
  }
  if (!is.function(getter)) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  backend <- getter()
  if (is.null(backend)) {
    return(list(type = "embedded", location = NULL, legacy = FALSE))
  }
  valid_type <- is.list(backend) &&
    is.character(backend$type) &&
    length(backend$type) == 1L &&
    !is.na(backend$type) &&
    backend$type %in% c("embedded", "h5", "bpcells")
  valid_location <- valid_type &&
    if (identical(backend$type, "embedded")) {
      is.null(backend$location)
    } else {
      is.character(backend$location) &&
        length(backend$location) == 1L &&
        !is.na(backend$location) &&
        nzchar(backend$location)
    }
  if (!valid_type || !valid_location) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  backend$legacy <- FALSE
  backend
}

.pathIsSymbolicLink <- function(path) {
  link <- Sys.readlink(path)
  !is.na(link) && nzchar(link)
}

.backendPathContainsSymbolicLink <- function(root, parts, source) {
  cursor <- root
  for (part in parts) {
    cursor <- file.path(cursor, part)
    if (.pathIsSymbolicLink(cursor)) {
      return(TRUE)
    }
  }
  if (!dir.exists(source)) {
    return(FALSE)
  }

  pending <- source
  while (length(pending) > 0L) {
    current <- pending[[1L]]
    pending <- pending[-1L]
    children <- list.files(
      current,
      all.files = TRUE,
      full.names = TRUE,
      no.. = TRUE
    )
    if (length(children) == 0L) {
      next
    }
    linked <- vapply(children, .pathIsSymbolicLink, logical(1))
    if (any(linked)) {
      return(TRUE)
    }
    pending <- c(pending, children[!linked & dir.exists(children)])
  }
  FALSE
}

.publishBundleStage <- function(stage, result_dir, overwrite, publish_mode) {
  backup <- NULL
  published <- FALSE
  on.exit(
    {
      if (!published && !is.null(backup) && dir.exists(backup)) {
        if (dir.exists(result_dir)) {
          unlink(result_dir, recursive = TRUE, force = TRUE)
        }
        if (!file.rename(backup, result_dir)) {
          warning(
            "App publication rollback could not restore the previous bundle ",
            "from: ",
            backup,
            call. = FALSE
          )
        }
      }
    },
    add = TRUE
  )

  if (file.exists(result_dir) && !dir.exists(result_dir)) {
    stop("'result_dir' exists and is not a directory.", call. = FALSE)
  }
  if (.pathIsSymbolicLink(result_dir)) {
    stop("'result_dir' must not be a symbolic link.", call. = FALSE)
  }
  if (
    !overwrite &&
      dir.exists(result_dir) &&
      length(list.files(result_dir, all.files = TRUE, no.. = TRUE)) > 0L
  ) {
    stop(
      "overwrite = FALSE rejects a non-empty result_dir; use an absent or ",
      "empty directory.",
      call. = FALSE
    )
  }
  if (dir.exists(result_dir)) {
    backup <- tempfile(
      pattern = paste0(".", basename(result_dir), "-backup-"),
      tmpdir = dirname(result_dir)
    )
    if (!file.rename(result_dir, backup)) {
      stop("Failed to stage the existing app for replacement.", call. = FALSE)
    }
  }
  if (!isTRUE(Sys.chmod(stage, mode = publish_mode))) {
    stop(
      "Failed to apply deployment permissions to the staged app.",
      call. = FALSE
    )
  }
  if (!file.rename(stage, result_dir)) {
    stop("Failed to publish the staged app bundle.", call. = FALSE)
  }
  published <- TRUE

  if (!is.null(backup) && dir.exists(backup)) {
    status <- unlink(backup, recursive = TRUE, force = TRUE)
    if (!identical(status, 0L)) {
      warning(
        "The new app was published, but an old backup remains at: ",
        backup,
        call. = FALSE
      )
    }
  }
  invisible(result_dir)
}

#' Create a self-contained Shiny app folder for Cerebro v1.4
#'
#' Bundles a Cerebro v1.4 Shiny app into \code{result_dir}, copying the
#' \code{inst/shiny/v1.4/} sources, the requested \code{.crb} data file(s),
#' and \code{extdata/}, and writes an \code{app.R} that sources the bundled
#' UI/server. The output directory can be served directly by shiny-server or
#' run with \code{shiny::runApp(result_dir)}.
#'
#' Supports external expression backends (\code{bpcells}, \code{h5}) in
#' addition to the embedded mode. The backend descriptor stored in each
#' \code{.crb} names a portable relative file or directory, which is copied to
#' the same relative location in the bundle. Missing or invalid descriptor-backed
#' sidecars and conflicting planned bundle targets stop the build rather than
#' producing an incomplete app. A configured runtime matrix override keeps its
#' existing precedence, is not copied or checked for existence at build time,
#' and skips the descriptor-backed sidecar copy. Required Cerebro files,
#' descriptor-backed sidecars, and planned bundle targets are validated before
#' the app is assembled in a private sibling directory. The completed stage
#' replaces \code{result_dir} only after every copy and configuration write
#' succeeds. On POSIX systems, the stage is mode \code{0700} while data is
#' copied, and replacement retains the existing deployment root's permission
#' bits. Platform-specific ACLs, ownership changes, and security labels remain
#' the deployment system's responsibility. Inputs must not be modified while
#' the build is running.
#'
#' @param cerebro_data Non-empty named character vector or list of \code{.crb}
#'   (or \code{.rds}) file paths. Names must be non-missing and unique and are
#'   used as dataset labels.
#' @param result_dir Output directory.
#' @param max_request_size Max upload size in MB; defaults to 8000.
#' @param port Port the generated app listens on; defaults to 1337.
#' @param host Host the generated app binds to; defaults to "127.0.0.1".
#' @param launch_browser Whether to launch a browser; defaults to TRUE.
#' @param quiet Passed to \code{shiny::runApp}; defaults to FALSE.
#' @param display_mode \code{shiny::runApp} display mode; defaults to "normal".
#' @param colors Optional named list of colour palettes per dataset.
#' @param cerebro_options Extra entries merged into \code{Cerebro.options} in
#'   the generated app.
#' @param overwrite If \code{TRUE} (default), replace \code{result_dir} only
#'   after a complete staged build succeeds. If \code{FALSE},
#'   \code{result_dir} must be absent or empty; a non-empty directory is
#'   rejected before any files are written.
#' @param verbose Print progress messages; defaults to TRUE.
#' @param crb_pick_smallest_file Forwarded to \code{Cerebro.options}.
#' @param show_upload_ui Forwarded to \code{Cerebro.options}.
#' @param welcome_message Welcome message shown in the Load Data tab.
#' @param point_size Named list with \code{overview_projection_point_size}
#'   (and optionally other keys) forwarded to \code{Cerebro.options}.
#' @param variable_to_compare Forwarded to \code{Cerebro.options}.
#' @param spatial_images Named list/vector of paths to spatial background images
#'   (e.g. tissue histology) shown behind the Spatial tab projection. Names must
#'   match \code{cerebro_data}. Existing images are copied into the app bundle;
#'   missing images are omitted with a warning.
#' @param spatial_images_flip_x Named list/vector; whether to flip the spatial
#'   background image horizontally. Names must match \code{cerebro_data}.
#' @param spatial_images_flip_y Named list/vector; whether to flip the spatial
#'   background image vertically. Names must match \code{cerebro_data}.
#' @param spatial_images_scale_x Named list/vector; scaling factor for the X
#'   axis of the spatial background image. Names must match \code{cerebro_data}.
#' @param spatial_images_scale_y Named list/vector; scaling factor for the Y
#'   axis of the spatial background image. Names must match \code{cerebro_data}.
#' @param spatial_images_offset_x Named list/vector; horizontal offset (in data
#'   units) applied to move the spatial background image. Names must match
#'   \code{cerebro_data}.
#' @param spatial_images_offset_y Named list/vector; vertical offset (in data
#'   units) applied to move the spatial background image. Names must match
#'   \code{cerebro_data}.
#' @param spatial_plot_rotation Named list/vector; initial rotation (degrees)
#'   applied to spatial cell coordinates. Names must match \code{cerebro_data}.
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return Invisibly returns \code{result_dir}.
#' @importFrom later later
#' @importFrom stats setNames
#' @export
createShinyApp <- function(
  cerebro_data,
  result_dir = NULL,
  max_request_size = 8000,
  port = 8080,
  host = "127.0.0.1",
  launch_browser = TRUE,
  quiet = FALSE,
  display_mode = "normal",
  colors = NULL,
  cerebro_options = list(exclude_trivial_metadata = TRUE),
  overwrite = TRUE,
  verbose = TRUE,
  crb_pick_smallest_file = TRUE,
  show_upload_ui = TRUE,
  welcome_message = "Welcome to CerebroNexus!",
  point_size = list(
    overview_projection_point_size = NULL
  ),
  variable_to_compare = NULL,
  spatial_images = NULL,
  spatial_images_flip_x = NULL,
  spatial_images_flip_y = NULL,
  spatial_images_scale_x = NULL,
  spatial_images_scale_y = NULL,
  spatial_images_offset_x = NULL,
  spatial_images_offset_y = NULL,
  spatial_plot_rotation = NULL,
  ...
) {
  # Validate inputs ----------------------------------------------------------##
  if (is.list(cerebro_data)) {
    valid_entries <- vapply(
      cerebro_data,
      function(path) {
        is.character(path) &&
          length(path) == 1L &&
          !is.na(path) &&
          nzchar(path)
      },
      logical(1)
    )
    if (!all(valid_entries)) {
      stop(
        "Every cerebro_data list entry must be one non-empty file path.",
        call. = FALSE
      )
    }
    data_names <- names(cerebro_data)
    cerebro_data <- vapply(cerebro_data, `[[`, character(1), 1L)
    names(cerebro_data) <- data_names
  }
  if (!is.character(cerebro_data)) {
    stop(
      "cerebro_data must be a named character vector or list of file paths.",
      call. = FALSE
    )
  }
  if (length(cerebro_data) == 0L) {
    stop("cerebro_data must contain at least one data set.", call. = FALSE)
  }
  if (!all(file.exists(cerebro_data))) {
    missing <- cerebro_data[!file.exists(cerebro_data)]
    stop(
      "Cerebro data file(s) not found: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!all(grepl("\\.(crb|rds)$", cerebro_data, ignore.case = TRUE))) {
    warning(
      "Some input files do not have .crb or .rds extension. Make sure they are valid Cerebro files."
    )
  }

  data_labels <- names(cerebro_data)
  if (
    is.null(data_labels) ||
      anyNA(data_labels) ||
      any(data_labels == "")
  ) {
    stop(
      "cerebro_data labels must be non-empty and non-missing.",
      call. = FALSE
    )
  }
  if (anyDuplicated(data_labels)) {
    stop("cerebro_data labels must be unique.", call. = FALSE)
  }
  if (
    !is.logical(overwrite) ||
      length(overwrite) != 1L ||
      is.na(overwrite)
  ) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    is.null(result_dir) ||
      !is.character(result_dir) ||
      length(result_dir) != 1L ||
      is.na(result_dir) ||
      !nzchar(result_dir)
  ) {
    stop("'result_dir' must be provided.", call. = FALSE)
  }
  if (file.exists(result_dir) && !dir.exists(result_dir)) {
    stop("'result_dir' exists and is not a directory.", call. = FALSE)
  }
  if (.pathIsSymbolicLink(result_dir)) {
    stop("'result_dir' must not be a symbolic link.", call. = FALSE)
  }
  if (
    !overwrite &&
      dir.exists(result_dir) &&
      length(list.files(result_dir, all.files = TRUE, no.. = TRUE)) > 0L
  ) {
    stop(
      "overwrite = FALSE rejects a non-empty result_dir; use an absent or ",
      "empty directory.",
      call. = FALSE
    )
  }

  if (!is.null(colors)) {
    if (is.null(names(colors)) || any(names(colors) == "")) {
      stop("colors must be a named list or vector.", call. = FALSE)
    }
    if (length(intersect(names(colors), names(cerebro_data))) == 0) {
      warning(
        "Colors and cerebro_data do not match, random colors will be used.",
        call. = FALSE
      )
      colors <- NULL
    }
  }

  if (!is.null(variable_to_compare) && !is.logical(variable_to_compare)) {
    if (
      (is.list(variable_to_compare) || is.vector(variable_to_compare)) &&
        !is.null(names(variable_to_compare))
    ) {
      if (
        length(intersect(names(variable_to_compare), names(cerebro_data))) == 0
      ) {
        warning(
          "No matching names found between variable_to_compare and cerebro_data. Ignoring.",
          call. = FALSE
        )
        variable_to_compare <- NULL
      }
    } else {
      warning(
        "variable_to_compare must be NULL, a single boolean, or a named list/vector. Ignoring.",
        call. = FALSE
      )
      variable_to_compare <- NULL
    }
  }

  ## Spatial background images (and their per-dataset transforms) must be named
  ## to match cerebro_data; drop with a warning if malformed rather than error,
  ## so a bad image spec never blocks app generation.
  validate_named_against_data <- function(x, arg_name) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.null(names(x)) || anyNA(names(x)) || any(names(x) == "")) {
      warning(
        arg_name,
        " must be a named list or vector. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    matching <- names(x) %in% names(cerebro_data)
    if (!any(matching)) {
      warning(
        "No matching names found between ",
        arg_name,
        " and cerebro_data. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    if (!all(matching)) {
      warning(
        "Some ",
        arg_name,
        " entries do not match cerebro_data and will be ignored: ",
        paste(unique(names(x)[!matching]), collapse = ", "),
        call. = FALSE
      )
    }
    x[matching]
  }
  spatial_images <- validate_named_against_data(
    spatial_images,
    "spatial_images"
  )
  spatial_images_flip_x <- validate_named_against_data(
    spatial_images_flip_x,
    "spatial_images_flip_x"
  )
  spatial_images_flip_y <- validate_named_against_data(
    spatial_images_flip_y,
    "spatial_images_flip_y"
  )
  spatial_images_scale_x <- validate_named_against_data(
    spatial_images_scale_x,
    "spatial_images_scale_x"
  )
  spatial_images_scale_y <- validate_named_against_data(
    spatial_images_scale_y,
    "spatial_images_scale_y"
  )
  spatial_images_offset_x <- validate_named_against_data(
    spatial_images_offset_x,
    "spatial_images_offset_x"
  )
  spatial_images_offset_y <- validate_named_against_data(
    spatial_images_offset_y,
    "spatial_images_offset_y"
  )
  spatial_plot_rotation <- validate_named_against_data(
    spatial_plot_rotation,
    "spatial_plot_rotation"
  )

  if (!requireNamespace("CerebroNexus", quietly = TRUE)) {
    stop(
      "Package 'CerebroNexus' is required but not installed.",
      call. = FALSE
    )
  }
  shiny_source <- system.file("shiny", package = "CerebroNexus")
  if (!dir.exists(shiny_source)) {
    stop(
      "Shiny source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }
  extdata_source <- system.file("extdata", package = "CerebroNexus")
  if (!dir.exists(extdata_source)) {
    stop(
      "extdata source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }

  # Preflight data inputs ----------------------------------------------------##
  backends <- lapply(cerebro_data, .readBundleBackend)
  copy_plan <- list()
  claimed_targets <- character()
  claimed_keys <- character()
  claimed_sources <- character()
  claimed_artifacts <- character()
  claimed_directories <- logical()
  claim_target <- function(target, source, artifact, directory = FALSE) {
    target <- .portableBundlePath(
      target,
      paste0("The ", artifact, " bundle target '", target, "'")
    )
    key <- tolower(target)
    for (claim_index in seq_along(claimed_keys)) {
      existing <- claimed_targets[[claim_index]]
      existing_key <- claimed_keys[[claim_index]]
      if (identical(key, existing_key)) {
        duplicate <- identical(target, existing) &&
          identical(source, claimed_sources[[claim_index]]) &&
          identical(artifact, claimed_artifacts[[claim_index]]) &&
          identical(isTRUE(directory), claimed_directories[[claim_index]])
        if (duplicate) {
          return(invisible(FALSE))
        }
        stop(
          "Different inputs resolve to the same bundle target '",
          target,
          "'. Rename one input before building the app.",
          call. = FALSE
        )
      }
      if (
        startsWith(key, paste0(existing_key, "/")) ||
          startsWith(existing_key, paste0(key, "/"))
      ) {
        stop(
          "Bundle target '",
          target,
          "' conflicts with parent or child target '",
          existing,
          "'. Rename one backend before building the app.",
          call. = FALSE
        )
      }
    }
    claimed_targets <<- c(claimed_targets, target)
    claimed_keys <<- c(claimed_keys, key)
    claimed_sources <<- c(claimed_sources, source)
    claimed_artifacts <<- c(claimed_artifacts, artifact)
    claimed_directories <<- c(claimed_directories, isTRUE(directory))
    copy_plan[[length(copy_plan) + 1L]] <<- list(
      target = target,
      source = source,
      artifact = artifact,
      directory = directory
    )
  }

  resolved_crb_sources <- vapply(
    cerebro_data,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = TRUE
  )
  for (index in seq_along(cerebro_data)) {
    claim_target(
      basename(cerebro_data[[index]]),
      resolved_crb_sources[[index]],
      "Cerebro data file"
    )
  }

  override_keys <- c("expression_matrix_h5", "expression_matrix_BPCells")
  for (key in override_keys) {
    override <- cerebro_options[[key]]
    if (
      !is.null(override) &&
        (!is.character(override) ||
          length(override) != 1L ||
          is.na(override) ||
          !nzchar(override))
    ) {
      stop(
        "cerebro_options[['",
        key,
        "']] must be one non-empty path.",
        call. = FALSE
      )
    }
  }

  override_users <- list(
    expression_matrix_h5 = character(),
    expression_matrix_BPCells = character()
  )
  for (index in seq_along(backends)) {
    backend <- backends[[index]]
    override_key <- NULL
    if (isTRUE(backend$legacy)) {
      if (!is.null(cerebro_options[["expression_matrix_h5"]])) {
        override_key <- "expression_matrix_h5"
      } else if (!is.null(cerebro_options[["expression_matrix_BPCells"]])) {
        override_key <- "expression_matrix_BPCells"
      }
    } else if (!identical(backend$type, "embedded")) {
      candidate <- switch(
        backend$type,
        h5 = "expression_matrix_h5",
        bpcells = "expression_matrix_BPCells"
      )
      if (!is.null(cerebro_options[[candidate]])) {
        override_key <- candidate
      }
    }
    if (!is.null(override_key)) {
      override_users[[override_key]] <- c(
        override_users[[override_key]],
        resolved_crb_sources[[index]]
      )
    }
  }
  distinct_override_users <- vapply(
    override_users,
    function(paths) length(unique(paths)),
    integer(1)
  )
  unsafe_override <- names(override_users)[distinct_override_users > 1L]
  if (length(unsafe_override) > 0L) {
    stop(
      "The global override cerebro_options[['",
      unsafe_override[[1L]],
      "']] would bind multiple Cerebro data files to the same expression ",
      "matrix. Use each .crb's own backend or build separate apps.",
      call. = FALSE
    )
  }

  for (index in seq_along(cerebro_data)) {
    file <- cerebro_data[[index]]
    backend <- backends[[index]]
    if (identical(backend$type, "embedded")) {
      next
    }
    override_key <- switch(
      backend$type,
      h5 = "expression_matrix_h5",
      bpcells = "expression_matrix_BPCells"
    )
    if (!is.null(cerebro_options[[override_key]])) {
      next
    }

    location <- .portableBundlePath(
      backend$location,
      paste0(
        "The ",
        backend$type,
        " backend location in '",
        basename(file),
        "'"
      )
    )
    parts <- strsplit(location, "/", fixed = TRUE)[[1L]]
    source_root <- dirname(file)
    source <- file.path(source_root, location)
    is_directory <- identical(backend$type, "bpcells")
    source_exists <- if (is_directory) {
      dir.exists(source)
    } else {
      file.exists(source) && !dir.exists(source)
    }
    if (!source_exists) {
      stop(
        "Expected the ",
        backend$type,
        " backend at '",
        source,
        "' recorded by '",
        basename(file),
        "', but it was not found.",
        call. = FALSE
      )
    }

    resolved_root <- normalizePath(
      source_root,
      winslash = "/",
      mustWork = TRUE
    )
    resolved_source <- normalizePath(
      source,
      winslash = "/",
      mustWork = TRUE
    )
    root_prefix <- if (identical(resolved_root, "/")) {
      "/"
    } else {
      paste0(sub("/+$", "", resolved_root), "/")
    }
    if (
      !startsWith(resolved_source, root_prefix) ||
        .backendPathContainsSymbolicLink(source_root, parts, source)
    ) {
      stop(
        "The ",
        backend$type,
        " backend location '",
        location,
        "' in '",
        basename(file),
        "' resolves through a symbolic link. Copy the real backend beside ",
        "the .crb before building the app.",
        call. = FALSE
      )
    }
    claim_target(
      location,
      resolved_source,
      paste0(backend$type, " backend"),
      directory = is_directory
    )
  }

  ## Spatial background images share the same target namespace as CRBs and
  ## external backends, so all collisions are rejected before any copy starts.
  if (!is.null(spatial_images) && length(spatial_images) > 0L) {
    bundled_spatial_images <- list()
    for (index in seq_along(spatial_images)) {
      dataset <- names(spatial_images)[[index]]
      copied_paths <- character()
      for (image in spatial_images[[index]]) {
        if (!file.exists(image)) {
          warning("Spatial image not found: ", image, call. = FALSE)
          next
        }
        target <- basename(image)
        claim_target(
          target,
          normalizePath(image, winslash = "/", mustWork = TRUE),
          "spatial image"
        )
        copied_paths <- c(copied_paths, file.path("data", target))
      }
      if (length(copied_paths) > 0L) {
        bundled_spatial_images[[length(bundled_spatial_images) + 1L]] <-
          copied_paths
        names(bundled_spatial_images)[[length(bundled_spatial_images)]] <-
          dataset
      }
    }
    spatial_images <- if (length(bundled_spatial_images) > 0L) {
      bundled_spatial_images
    } else {
      NULL
    }
  }

  # Assemble a private sibling stage ----------------------------------------##
  publish_mode <- if (dir.exists(result_dir)) {
    file.info(result_dir)$mode[[1L]]
  } else {
    current_umask <- strtoi(as.character(Sys.umask(NA)), base = 8L)
    as.octmode(bitwAnd(strtoi("777", base = 8L), bitwNot(current_umask)))
  }
  result_parent <- dirname(result_dir)
  if (!dir.exists(result_parent)) {
    dir.create(result_parent, recursive = TRUE, showWarnings = FALSE)
  }
  stage_result_dir <- tempfile(
    pattern = paste0(".", basename(result_dir), "-stage-"),
    tmpdir = result_parent
  )
  if (!dir.create(stage_result_dir, mode = "0700", showWarnings = FALSE)) {
    stop("Failed to create a private app staging directory.", call. = FALSE)
  }
  on.exit(
    unlink(stage_result_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  data_dir <- file.path(stage_result_dir, "data")
  app_file <- file.path(stage_result_dir, "app.R")

  if (verbose) {
    cat("Creating staged directory structure...\n")
  }
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    cat("Copying Shiny source files...\n")
  }
  if (!file.copy(shiny_source, stage_result_dir, recursive = TRUE)) {
    stop("Failed to copy Shiny source files.", call. = FALSE)
  }

  if (verbose) {
    cat("Copying data artifacts...\n")
  }
  for (entry in copy_plan) {
    target <- file.path(data_dir, entry$target)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    copied <- if (isTRUE(entry$directory)) {
      file.copy(entry$source, dirname(target), recursive = TRUE)
    } else {
      file.copy(entry$source, target, overwrite = FALSE)
    }
    copied_target_exists <- if (isTRUE(entry$directory)) {
      dir.exists(target)
    } else {
      file.exists(target) && !dir.exists(target)
    }
    if (!isTRUE(copied) || !copied_target_exists) {
      stop(
        "Failed to copy ",
        entry$artifact,
        ": ",
        entry$target,
        call. = FALSE
      )
    }
    if (verbose) {
      cat("  -", entry$target, paste0("(", entry$artifact, ")\n"))
    }
  }

  # Copy extdata -------------------------------------------------------------##
  if (verbose) {
    cat("Copying extdata files...\n")
  }
  if (!file.copy(extdata_source, stage_result_dir, recursive = TRUE)) {
    stop("Failed to copy extdata files.", call. = FALSE)
  }

  # Build Cerebro.options ----------------------------------------------------##
  if (verbose) {
    cat("Generating app.R file...\n")
  }

  crb_files <- setNames(
    paste0("data/", basename(cerebro_data)),
    names(cerebro_data)
  )

  cerebro_options[["mode"]] <- "open"
  ## Resolve the version while the package is present, then serialize it into
  ## the generated app. The standalone bundle never needs CerebroNexus at
  ## runtime merely to render its About page.
  cerebro_options[["cerebro_version"]] <- as.character(
    utils::packageVersion("CerebroNexus")
  )
  cerebro_options[["crb_file_to_load"]] <- crb_files
  cerebro_options[["cerebro_root"]] <- "."
  if (!is.null(crb_pick_smallest_file)) {
    cerebro_options[["crb_pick_smallest_file"]] <- crb_pick_smallest_file
  }
  if (!is.null(show_upload_ui)) {
    cerebro_options[["show_upload_ui"]] <- show_upload_ui
  }
  if (!is.null(point_size)) {
    cerebro_options[["point_size"]] <- point_size
  }
  if (!is.null(colors)) {
    cerebro_options[["colors"]] <- colors
  }
  if (!is.null(welcome_message)) {
    cerebro_options[["welcome_message"]] <- welcome_message
  }
  if (!is.null(variable_to_compare)) {
    cerebro_options[["variable_to_compare"]] <- variable_to_compare
  }
  if (!is.null(spatial_images)) {
    cerebro_options[["spatial_images"]] <- spatial_images
  }
  if (!is.null(spatial_images_flip_x)) {
    cerebro_options[["spatial_images_flip_x"]] <- spatial_images_flip_x
  }
  if (!is.null(spatial_images_flip_y)) {
    cerebro_options[["spatial_images_flip_y"]] <- spatial_images_flip_y
  }
  if (!is.null(spatial_images_scale_x)) {
    cerebro_options[["spatial_images_scale_x"]] <- spatial_images_scale_x
  }
  if (!is.null(spatial_images_scale_y)) {
    cerebro_options[["spatial_images_scale_y"]] <- spatial_images_scale_y
  }
  if (!is.null(spatial_images_offset_x)) {
    cerebro_options[["spatial_images_offset_x"]] <- spatial_images_offset_x
  }
  if (!is.null(spatial_images_offset_y)) {
    cerebro_options[["spatial_images_offset_y"]] <- spatial_images_offset_y
  }
  if (!is.null(spatial_plot_rotation)) {
    cerebro_options[["spatial_plot_rotation"]] <- spatial_plot_rotation
  }

  saveRDS(cerebro_options, file.path(stage_result_dir, "cerebro_config.rds"))

  # Generate app.R -----------------------------------------------------------##
  app_content <- glue::glue(
    '
    library(dplyr)
    library(DT)
    library(plotly)
    library(shiny)
    library(shinydashboard)
    library(shinyWidgets)

    cerebro_root <- "."

    if (file.exists("cerebro_config.rds")) {{
      Cerebro.options <<- readRDS("cerebro_config.rds")
    }} else {{
      stop("cerebro_config.rds not found!")
    }}

    if (!is.null(Cerebro.options$colors)) {{
      colors <- Cerebro.options$colors
    }}

    shiny_options <- list(
      maxRequestSize = {max_request_size} * 1024^2,
      port = {port},
      host = "{host}",
      launch.browser = {toupper(as.character(launch_browser))},
      quiet = {toupper(as.character(quiet))},
      display.mode = "{display_mode}"
    )

    shiny::addResourcePath("data", file.path(cerebro_root, "data"))

    source(file.path(cerebro_root, "shiny/v1.4/shiny_UI.R"))
    source(file.path(cerebro_root, "shiny/v1.4/shiny_server.R"))

    shiny::shinyApp(
      ui = ui,
      server = server,
      options = shiny_options
    )
    ',
    .trim = FALSE
  )

  writeLines(dedent(app_content), app_file)
  .publishBundleStage(
    stage_result_dir,
    result_dir,
    overwrite,
    publish_mode
  )

  # Summary ------------------------------------------------------------------##
  if (verbose) {
    cat("\n")
    cat("========================================\n")
    cat("Shiny app successfully created!\n")
    cat("========================================\n")
    cat("App directory:", result_dir, "\n")
    cat("Data file(s):\n")
    for (i in seq_along(cerebro_data)) {
      label <- names(cerebro_data)[i]
      if (!is.null(label) && nzchar(label)) {
        cat("  -", label, ":", basename(cerebro_data[i]), "\n")
      } else {
        cat("  -", basename(cerebro_data[i]), "\n")
      }
    }
    cat("Port:", port, "\n")
    cat("Host:", host, "\n")
    cat("Launch browser:", launch_browser, "\n")
    cat("\nTo launch the app, run:\n")
    cat("  setwd('", result_dir, "')\n", sep = "")
    cat("  shiny::runApp('app.R')\n")
    cat("========================================\n")
  }

  invisible(result_dir)
}
