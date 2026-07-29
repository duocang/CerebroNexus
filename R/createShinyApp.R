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

#' Create a self-contained Shiny app folder for Cerebro v1.4
#'
#' Bundles a Cerebro v1.4 Shiny app into \code{result_dir}, copying the
#' \code{inst/shiny/v1.4/} sources, the requested \code{.crb} data file(s),
#' and \code{extdata/}, and writes an \code{app.R} that sources the bundled
#' UI/server. The output directory can be served directly by shiny-server or
#' run with \code{shiny::runApp(result_dir)}.
#'
#' Supports external expression backends (\code{bpcells}, \code{h5}) in
#' addition to the embedded mode. Such a \code{.crb} holds no expression
#' matrix, only a tag naming the sibling \code{.bpcells/} directory or
#' \code{.h5} file that holds it. The sibling is read from that tag and copied
#' into the bundle under the same name, so renaming the \code{.crb} does not
#' lose it. A sibling that cannot be found is an error rather than a silent
#' omission: the generated app would otherwise start and then fail to show any
#' expression data. Pass \code{cerebro_options$expression_matrix_h5} or
#' \code{expression_matrix_BPCells} to point at a matrix elsewhere instead.
#'
#' @param cerebro_data Named character vector or list of \code{.crb} (or
#'   \code{.rds}) file paths. Names are used as dataset labels.
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
#' @param overwrite If \code{TRUE} (default), wipe \code{result_dir} first.
#' @param verbose Print progress messages; defaults to TRUE.
#' @param crb_pick_smallest_file Forwarded to \code{Cerebro.options}.
#' @param show_upload_ui Forwarded to \code{Cerebro.options}.
#' @param welcome_message Welcome message shown in the Load Data tab.
#' @param point_size Named list with \code{overview_projection_point_size}
#'   (and optionally other keys) forwarded to \code{Cerebro.options}.
#' @param variable_to_compare Forwarded to \code{Cerebro.options}.
#' @param spatial_images Named list/vector of paths to spatial background images
#'   (e.g. tissue histology) shown behind the Spatial tab projection. Names must
#'   match \code{cerebro_data}. Images are copied into the app bundle.
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

  if (is.null(names(cerebro_data)) || any(names(cerebro_data) == "")) {
    stop(
      "cerebro_data must be a named list or vector, and every element must have a name.",
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
    if (is.null(names(x)) || any(names(x) == "")) {
      warning(
        arg_name,
        " must be a named list or vector. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    if (length(intersect(names(x), names(cerebro_data))) == 0) {
      warning(
        "No matching names found between ",
        arg_name,
        " and cerebro_data. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    x
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

  if (is.null(result_dir)) {
    stop("'result_dir' must be provided.", call. = FALSE)
  }

  # Setup directories --------------------------------------------------------##
  data_dir <- file.path(result_dir, "data")
  app_file <- file.path(result_dir, "app.R")

  if (overwrite && dir.exists(result_dir)) {
    if (verbose) {
      cat("Removing existing directory:", result_dir, "\n")
    }
    unlink(result_dir, recursive = TRUE, force = TRUE)
  }

  if (verbose) {
    cat("Creating directory structure...\n")
  }
  dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy Shiny source --------------------------------------------------------##
  shiny_source <- system.file("shiny", package = "CerebroNexus")
  if (!dir.exists(shiny_source)) {
    stop(
      "Shiny source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }

  if (verbose) {
    cat("Copying Shiny source files...\n")
  }
  if (!file.copy(shiny_source, result_dir, recursive = TRUE)) {
    stop("Failed to copy Shiny source files.", call. = FALSE)
  }

  # Copy Cerebro data file(s) -----------------------------------------------##
  if (verbose) {
    cat("Copying Cerebro data file(s)...\n")
  }
  ## Shared across the data files so a second data set cannot claim a sibling
  ## name the first one already took.
  claimed_siblings <- new.env(parent = emptyenv())
  on.exit(
    {
      claim_root <- claimed_siblings[[".filesystem_claim_root"]]
      if (is.character(claim_root) && length(claim_root) == 1) {
        unlink(claim_root, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  for (file in cerebro_data) {
    if (verbose) {
      cat("  -", basename(file), "\n")
    }
    if (!file.copy(file, data_dir, recursive = TRUE)) {
      stop("Failed to copy Cerebro data file: ", basename(file), call. = FALSE)
    }
    .copyExpressionBackendSibling(
      crb_path = file,
      data_dir = data_dir,
      cerebro_options = cerebro_options,
      claimed = claimed_siblings,
      verbose = verbose
    )
  }

  # Copy spatial images ------------------------------------------------------##
  ## Side-copy each background image into data_dir and rewrite the stored path
  ## to the bundle-relative "data/<file>" so the generated app is portable.
  if (!is.null(spatial_images) && length(spatial_images) > 0) {
    if (verbose) {
      cat("Copying spatial images...\n")
    }
    for (nm in names(spatial_images)) {
      img_paths <- spatial_images[[nm]]
      copied_paths <- character(0)
      for (img in img_paths) {
        if (file.exists(img)) {
          dest <- file.path(data_dir, basename(img))
          if (!file.copy(img, dest, overwrite = TRUE)) {
            warning("Failed to copy spatial image: ", img, call. = FALSE)
            copied_paths <- c(copied_paths, img)
          } else {
            if (verbose) {
              cat("  -", basename(img), "\n")
            }
            copied_paths <- c(copied_paths, file.path("data", basename(img)))
          }
        } else {
          warning("Spatial image not found: ", img, call. = FALSE)
          copied_paths <- c(copied_paths, img)
        }
      }
      spatial_images[[nm]] <- copied_paths
    }
  }

  # Copy extdata -------------------------------------------------------------##
  if (verbose) {
    cat("Copying extdata files...\n")
  }
  extdata_source <- system.file("extdata", package = "CerebroNexus")
  if (!dir.exists(extdata_source)) {
    stop(
      "extdata source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }
  if (!file.copy(extdata_source, result_dir, recursive = TRUE)) {
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

  saveRDS(cerebro_options, file.path(result_dir, "cerebro_config.rds"))

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

#' Copy a .crb's external expression-matrix sibling into the bundle
#'
#' A `.crb` written with `expression_matrix_mode = "h5"` or `"bpcells"` carries
#' no expression matrix. It carries a backend tag naming a sibling file or
#' directory, which the running app resolves relative to the `.crb`.
#'
#' The sibling used to be located by guessing `<crb stem>.h5` and
#' `<crb stem>.bpcells`. The guess is right until someone renames the `.crb` --
#' an ordinary thing to do when giving datasets readable names -- and when it
#' missed, nothing was copied and nothing was said. The bundle looked complete
#' and failed only when a user opened the app. The tag is the authority on the
#' sibling's name, so read it, and treat a sibling that cannot be found as a
#' failure to build the bundle rather than a detail to skip.
#'
#' @param crb_path Path to the `.crb` that was just copied into the bundle.
#' @param data_dir The bundle's `data/` directory.
#' @param cerebro_options The options list passed to `createShinyApp()`.
#' @param claimed An environment shared across the data files of one bundle,
#'   recording which sibling names have been claimed and by which source, so a
#'   second data set cannot quietly overwrite the first one's matrix.
#' @param verbose Print progress messages.
#'
#' @keywords internal
#' @noRd
.copyExpressionBackendSibling <- function(
  crb_path,
  data_dir,
  cerebro_options = NULL,
  claimed = new.env(parent = emptyenv()),
  verbose = TRUE
) {
  ## The backend tag lives inside the serialised object, so finding it costs
  ## one deserialisation per data file. For an external backend that is nearly
  ## free -- the object holds a tag, not a matrix. For an embedded one it is
  ## not: a measured 4.9 MB `.crb` deserialises to a 17.4 MB matrix, and that
  ## is paid here only to learn there is no sibling to copy.
  ##
  ## It is paid anyway. The tag cannot be read without deserialising (RDS has
  ## no random access), and every cheaper route -- guessing `<stem>.h5`,
  ## judging by file size -- can silently miss a sibling that a renamed `.crb`
  ## still points at, which is the failure this whole path exists to prevent.
  ## See tmp/backend-inspection-decision.md for the measurements.
  object <- readRDS(crb_path)
  backend <- NULL
  if (is.environment(object) || is.list(object)) {
    getter <- object[["getExpressionBackend"]]
    if (is.function(getter)) {
      backend <- getter()
    }
  }
  rm(object)

  if (is.null(backend) || identical(backend$type, "embedded")) {
    return(invisible(NULL))
  }

  ## An absolute path in `Cerebro.options` outranks sibling resolution at
  ## runtime, so a bundle configured that way is expected to have no sibling
  ## next to the .crb. Demanding one would reject a working configuration.
  override_key <- switch(
    backend$type,
    bpcells = "expression_matrix_BPCells",
    h5 = "expression_matrix_h5",
    NULL
  )
  override <- if (is.null(override_key)) {
    NULL
  } else {
    cerebro_options[[override_key]]
  }

  if (!is.null(override)) {
    ## Skipping the sibling check on the strength of an override means the
    ## override has to be a usable *value*. An empty string is not one, and
    ## `!is.null("")` was the whole test -- so a typo turned the fail-loud
    ## guarantee off and the failure moved back to run time.
    ##
    ## Deliberately not checked: whether the path exists. It is resolved on
    ## whatever machine runs the generated app, which is routinely not this
    ## one, so a build machine without the matrix is a normal situation rather
    ## than an error. That is the limit of what this guarantees -- the value is
    ## well-formed, not that it will resolve.
    if (
      !is.character(override) ||
        length(override) != 1 ||
        is.na(override) ||
        !nzchar(override)
    ) {
      stop(
        "cerebro_options[['",
        override_key,
        "']] has to be a single non-empty path to the expression matrix. ",
        "Received: ",
        paste(utils::capture.output(utils::str(override)), collapse = " "),
        call. = FALSE
      )
    }

    ## `Cerebro.options` is one list for the whole app, so this override is not
    ## per-dataset: every .crb with this backend resolves to the same matrix.
    ## With one such .crb that is the intent; with several it is almost
    ## certainly not, and it would show as the wrong expression values rather
    ## than as an error.
    previous <- claimed[[paste0("override:", override_key)]]
    if (!is.null(previous)) {
      warning(
        "cerebro_options[['",
        override_key,
        "']] is a single setting for the whole app, but '",
        basename(crb_path),
        "' and '",
        basename(previous),
        "' both use the ",
        backend$type,
        " backend. Both will read the same expression matrix.",
        call. = FALSE
      )
    }
    claimed[[paste0("override:", override_key)]] <- crb_path

    if (verbose) {
      cat(
        "  - expression matrix taken from cerebro_options$",
        override_key,
        "; no sibling copied\n",
        sep = ""
      )
    }
    return(invisible(NULL))
  }

  if (is.null(backend$location) || !nzchar(backend$location)) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' declares an external '",
      backend$type,
      "' expression backend but carries no location tag, so the matrix it ",
      "refers to cannot be found. This .crb may have been generated by a ",
      "buggy exporter; re-export it with exportFromSeurat().",
      call. = FALSE
    )
  }

  ## The tag is resolved against the .crb's directory at runtime, so it has to
  ## be a relative path that stays inside the bundle. An absolute one would be
  ## mis-joined here anyway -- file.path("data", "/abs/m.h5") is
  ## "data//abs/m.h5" -- and a `..` segment would write outside the bundle
  ## while still failing to resolve once the app is deployed elsewhere.
  ## Normalise the tag lexically before anything compares or joins it. `m.h5`
  ## and `./m.h5` name one file but are two different strings, and comparing
  ## the strings is what the collision check below does -- so without this,
  ## two data sets could claim the same destination under different spellings
  ## and one would silently overwrite the other. Lexical rather than
  ## `normalizePath()`, because the destination does not exist yet.
  location_parts <- strsplit(backend$location, "[/\\\\]+")[[1]]
  location_parts <- location_parts[
    nzchar(location_parts) & location_parts != "."
  ]
  normalised_location <- paste(location_parts, collapse = "/")

  if (
    grepl("^(/|~|[A-Za-z]:)", backend$location) ||
      any(location_parts == "..") ||
      !nzchar(normalised_location)
  ) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' declares its ",
      backend$type,
      " expression matrix at '",
      backend$location,
      "', which is not a path inside the .crb's own directory. The runtime ",
      "resolves this tag relative to the .crb, so it has to stay relative and ",
      "must not climb out with '..'. Re-export with exportFromSeurat(), or ",
      "point at the matrix with cerebro_options[['",
      override_key,
      "']].",
      call. = FALSE
    )
  }

  is_directory_backend <- identical(backend$type, "bpcells")
  source_path <- file.path(dirname(crb_path), normalised_location)
  exists_on_disk <- if (is_directory_backend) dir.exists else file.exists

  if (!exists_on_disk(source_path)) {
    stop(
      "Expected the ",
      backend$type,
      " expression matrix at '",
      source_path,
      "' (derived from '",
      basename(crb_path),
      "' + backend location '",
      backend$location,
      "'), but it is not there. ",
      "Did the sibling get left behind when the .crb was moved or renamed? ",
      "The .crb holds no expression data on its own, so the generated app ",
      "would start and then fail to show any. ",
      "You can also point at a different absolute location via ",
      "cerebro_options[['",
      override_key,
      "']].",
      call. = FALSE
    )
  }

  ## Keep the sibling's name: the app resolves it by the location tag, which
  ## does not change when the .crb is renamed. `location` is normally a bare
  ## file name, but honour a relative sub-path if one was set.
  ##
  ## Two data sets can carry the same tag -- exporting `ds.crb` twice in
  ## different directories is enough -- and every sibling lands in the one
  ## `data/`. The second copy would overwrite the first and both .crb files
  ## would then read the same matrix. Renaming is not available as a fix: the
  ## tag lives inside the .crb and is what the app looks for. So refuse, and
  ## say which two files collided.
  ##
  ## Resolve sources before comparing them, so one source reached by two paths
  ## remains a harmless repeated claim. Destination identity is different:
  ## normalizePath() is not guaranteed to return a directory entry's actual
  ## case on every case-insensitive file system. Record each claim in a fresh
  ## directory on the bundle's own file system instead. Its directory lookup
  ## then decides whether two spellings are aliases, without confusing a
  ## sibling left by an overwrite = FALSE rebuild with a claim made now.
  resolved_source <- normalizePath(
    source_path,
    winslash = "/",
    mustWork = FALSE
  )
  claim_key <- paste0("sibling:", normalised_location)
  previous_source <- claimed[[claim_key]]
  if (
    !is.null(previous_source) && !identical(previous_source, resolved_source)
  ) {
    stop(
      "Two data sets want different expression matrices under the same name ",
      "in the bundle: '",
      normalised_location,
      "' from '",
      previous_source,
      "' and from '",
      resolved_source,
      "'. One would overwrite the other and both .crb files would read the ",
      "same matrix. The name comes from a tag inside each .crb, so it cannot ",
      "be changed here -- re-export one of them under a different file name, ",
      "which gives its matrix a different name too.",
      call. = FALSE
    )
  }
  if (!is.null(previous_source)) {
    ## the same source bundled twice: nothing to do, and nothing wrong
    return(invisible(NULL))
  }

  destination_dir <- file.path(data_dir, dirname(normalised_location))
  dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  destination_path <- file.path(
    destination_dir,
    basename(normalised_location)
  )

  claim_root <- claimed[[".filesystem_claim_root"]]
  if (is.null(claim_root)) {
    claim_root <- tempfile(
      pattern = ".cerebro-sibling-claims-",
      tmpdir = dirname(data_dir)
    )
    if (!dir.create(claim_root, showWarnings = FALSE)) {
      stop(
        "Failed to create a temporary sibling-claim directory beside '",
        data_dir,
        "'.",
        call. = FALSE
      )
    }
    claimed[[".filesystem_claim_root"]] <- claim_root
  }

  claim_parts <- strsplit(normalised_location, "/", fixed = TRUE)[[1]]
  claim_cursor <- claim_root
  claim_conflict <- FALSE
  if (length(claim_parts) > 1) {
    for (part in claim_parts[-length(claim_parts)]) {
      claim_cursor <- file.path(claim_cursor, part)
      if (file.exists(file.path(claim_cursor, ".claimed"))) {
        claim_conflict <- TRUE
        break
      }
      if (
        !dir.exists(claim_cursor) &&
          !dir.create(claim_cursor, showWarnings = FALSE)
      ) {
        claim_conflict <- TRUE
        break
      }
    }
  }
  claim_path <- file.path(claim_root, normalised_location)
  if (
    claim_conflict ||
      dir.exists(claim_path) ||
      !dir.create(claim_path, showWarnings = FALSE)
  ) {
    stop(
      "The expression-matrix destination '",
      destination_path,
      "' is already claimed in this bundle build while copying '",
      resolved_source,
      "'. The destination file system may treat this tag as an alias of an ",
      "earlier data set's tag (for example, names that differ only by case). ",
      "Copying it would overwrite the matrix already there.",
      call. = FALSE
    )
  }
  if (!file.create(file.path(claim_path, ".claimed"))) {
    stop(
      "Failed to record the expression-matrix destination claim for '",
      destination_path,
      "'.",
      call. = FALSE
    )
  }
  claimed[[claim_key]] <- resolved_source

  if (verbose) {
    cat("  -", normalised_location, paste0("(", backend$type, " matrix)"), "\n")
  }

  ## Recursive file.copy() can populate part of an existing BPCells directory
  ## before returning FALSE. Copy into a private directory first, then swap the
  ## complete file/directory into place. This also makes overwrite = FALSE
  ## rebuilds replace the old sibling as a unit instead of mixing generations.
  destination_existed <- file.exists(destination_path)
  stage_dir <- tempfile(
    pattern = paste0(".", basename(normalised_location), "-stage-"),
    tmpdir = destination_dir
  )
  if (!dir.create(stage_dir, showWarnings = FALSE)) {
    stop(
      "Failed to create a temporary directory while copying the ",
      backend$type,
      " expression matrix '",
      normalised_location,
      "'.",
      call. = FALSE
    )
  }
  on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)

  copied <- file.copy(
    source_path,
    stage_dir,
    overwrite = FALSE,
    recursive = is_directory_backend
  )
  staged_path <- file.path(stage_dir, basename(source_path))
  if (!isTRUE(copied) || !exists_on_disk(staged_path)) {
    stop(
      "Failed to copy the ",
      backend$type,
      " expression matrix '",
      normalised_location,
      "' into the app bundle.",
      call. = FALSE
    )
  }

  if (!destination_existed && file.exists(destination_path)) {
    stop(
      "Failed to copy the ",
      backend$type,
      " expression matrix '",
      normalised_location,
      "' into the app bundle because its destination appeared during the ",
      "copy. The existing destination was left unchanged.",
      call. = FALSE
    )
  }

  backup_path <- NULL
  if (destination_existed) {
    backup_path <- tempfile(
      pattern = paste0(".", basename(normalised_location), "-backup-"),
      tmpdir = destination_dir
    )
    if (!file.rename(destination_path, backup_path)) {
      stop(
        "Failed to move the existing ",
        backend$type,
        " expression matrix aside while rebuilding '",
        normalised_location,
        "'.",
        call. = FALSE
      )
    }
  }

  if (!file.rename(staged_path, destination_path)) {
    restored <- is.null(backup_path) ||
      file.rename(backup_path, destination_path)
    stop(
      "Failed to install the copied ",
      backend$type,
      " expression matrix '",
      normalised_location,
      "' into the app bundle.",
      if (!restored) {
        paste0(
          " Restoring the previous matrix also failed; its backup remains at '",
          backup_path,
          "'."
        )
      } else {
        ""
      },
      call. = FALSE
    )
  }
  if (!is.null(backup_path)) {
    unlink(backup_path, recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}
