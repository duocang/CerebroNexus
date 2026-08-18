##----------------------------------------------------------------------------##
## Reading a serialised Seurat object, whichever way it was written.
##
## `.rds` is base R. `qs` and `qs2` are what people reach for once objects get
## big enough that `saveRDS()` becomes the slow part of the day, so a tool that
## only reads `.rds` asks them to convert first.
##
## Pure: no Shiny. Each reader is optional -- the package is only needed by
## whoever actually has files in that format, and its absence is reported as a
## sentence rather than a missing-function error.
##----------------------------------------------------------------------------##

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Choose a local output directory with the operating system's picker.
#'
#' The optional selector keeps platform UI outside the Build flow and makes
#' cancellation and path normalization directly testable.
builder_choose_output_directory <- function(.select = NULL) {
  select <- .select %||%
    function() {
      system <- Sys.info()[["sysname"]] %||% ""
      if (identical(system, "Windows")) {
        return(utils::choose.dir(
          caption = "Choose where to save the build output."
        ))
      }
      if (identical(system, "Darwin")) {
        script <- paste(
          "POSIX path of (choose folder with prompt",
          '"Choose where to save the build output.")'
        )
        return(system2("osascript", c("-e", shQuote(script)), stdout = TRUE))
      }
      zenity <- Sys.which("zenity")
      if (nzchar(zenity)) {
        return(system2(
          zenity,
          c(
            "--file-selection",
            "--directory",
            shQuote(
              "--title=Choose where to save the build output."
            )
          ),
          stdout = TRUE,
          stderr = FALSE
        ))
      }
      kdialog <- Sys.which("kdialog")
      if (nzchar(kdialog)) {
        return(system2(
          kdialog,
          c("--getexistingdirectory", shQuote(path.expand("~"))),
          stdout = TRUE,
          stderr = FALSE
        ))
      }
      stop("No system folder picker is available.", call. = FALSE)
    }
  chosen <- tryCatch(select(), error = identity)
  if (inherits(chosen, "error")) {
    return(list(
      status = "error",
      path = NULL,
      error = conditionMessage(chosen)
    ))
  }
  if (!length(chosen) || is.na(chosen[[1L]]) || !nzchar(trimws(chosen[[1L]]))) {
    return(list(status = "cancelled", path = NULL))
  }
  path <- tryCatch(
    normalizePath(
      path.expand(trimws(chosen[[1L]])),
      winslash = "/",
      mustWork = TRUE
    ),
    error = identity
  )
  if (inherits(path, "error") || !dir.exists(path)) {
    return(list(
      status = "error",
      path = NULL,
      error = "The selected folder is not available."
    ))
  }
  list(status = "selected", path = path)
}

builder_project_osascript <- function(kind = c("directory", "manifest")) {
  kind <- match.arg(kind)
  if (identical(kind, "directory")) {
    return(paste0(
      "POSIX path of (choose folder with prompt ",
      "\"Choose a folder for the Builder project.\")"
    ))
  }
  paste0(
    "POSIX path of (choose file with prompt ",
    "\"Open a Builder project.\" of type {\"public.json\"})"
  )
}

#' Choose a durable Builder project folder.
builder_choose_project_directory <- function(.select = NULL) {
  select <- .select %||%
    function() {
      system <- Sys.info()[["sysname"]] %||% ""
      if (identical(system, "Windows")) {
        return(utils::choose.dir(
          caption = "Choose a folder for the Builder project."
        ))
      }
      if (identical(system, "Darwin")) {
        script <- builder_project_osascript("directory")
        return(system2("osascript", c("-e", shQuote(script)), stdout = TRUE))
      }
      zenity <- Sys.which("zenity")
      if (nzchar(zenity)) {
        return(system2(
          zenity,
          c(
            "--file-selection",
            "--directory",
            shQuote("--title=Choose a folder for the Builder project.")
          ),
          stdout = TRUE,
          stderr = FALSE
        ))
      }
      kdialog <- Sys.which("kdialog")
      if (nzchar(kdialog)) {
        return(system2(
          kdialog,
          c("--getexistingdirectory", shQuote(path.expand("~"))),
          stdout = TRUE,
          stderr = FALSE
        ))
      }
      stop("No system folder picker is available.", call. = FALSE)
    }
  chosen <- tryCatch(select(), error = identity)
  if (inherits(chosen, "error")) {
    return(list(
      status = "error",
      path = NULL,
      error = conditionMessage(chosen)
    ))
  }
  if (!length(chosen) || is.na(chosen[[1L]]) || !nzchar(trimws(chosen[[1L]]))) {
    return(list(status = "cancelled", path = NULL))
  }
  path <- tryCatch(
    normalizePath(
      path.expand(trimws(chosen[[1L]])),
      winslash = "/",
      mustWork = TRUE
    ),
    error = identity
  )
  if (inherits(path, "error") || !dir.exists(path)) {
    return(list(
      status = "error",
      path = NULL,
      error = "The selected folder is not available."
    ))
  }
  list(status = "selected", path = path)
}

#' Choose an existing Builder project manifest with the operating system picker.
builder_choose_project_manifest <- function(.select = NULL) {
  select <- .select %||%
    function() {
      system <- Sys.info()[["sysname"]] %||% ""
      if (identical(system, "Windows")) {
        return(file.choose())
      }
      if (identical(system, "Darwin")) {
        script <- builder_project_osascript("manifest")
        return(system2("osascript", c("-e", shQuote(script)), stdout = TRUE))
      }
      zenity <- Sys.which("zenity")
      if (nzchar(zenity)) {
        return(system2(
          zenity,
          c(
            "--file-selection",
            shQuote("--title=Open a Builder project."),
            shQuote("--file-filter=Builder project | *.json")
          ),
          stdout = TRUE,
          stderr = FALSE
        ))
      }
      kdialog <- Sys.which("kdialog")
      if (nzchar(kdialog)) {
        return(system2(
          kdialog,
          c("--getopenfilename", shQuote(path.expand("~")), shQuote("*.json")),
          stdout = TRUE,
          stderr = FALSE
        ))
      }
      stop("No system file picker is available.", call. = FALSE)
    }
  chosen <- tryCatch(select(), error = identity)
  if (inherits(chosen, "error")) {
    return(list(
      status = "error",
      path = NULL,
      error = conditionMessage(chosen)
    ))
  }
  if (!length(chosen) || is.na(chosen[[1L]]) || !nzchar(trimws(chosen[[1L]]))) {
    return(list(status = "cancelled", path = NULL))
  }
  path <- tryCatch(
    normalizePath(
      path.expand(trimws(chosen[[1L]])),
      winslash = "/",
      mustWork = TRUE
    ),
    error = identity
  )
  if (inherits(path, "error") || !file.exists(path) || dir.exists(path)) {
    return(list(
      status = "error",
      path = NULL,
      error = "The selected project file is not available."
    ))
  }
  list(status = "selected", path = path)
}

## Resource lookup must stay in the same immutable inst/ tree as this file.
## `system.file()` is deliberately only a fallback for runtimes where io.R was
## not sourced from a verifiable <inst>/builder/io.R path (for example, a
## future namespace-loaded copy). A source checkout may coexist with an older
## installed CerebroNexus, and mixing those two trees makes fixtures depend on
## whichever package library happens to win lookup.
.builder_io_source_path <- local({
  frames <- sys.frames()
  paths <- vapply(
    rev(frames),
    function(frame) frame$ofile %||% "",
    character(1)
  )
  paths <- paths[nzchar(paths)]
  paths <- paths[
    basename(paths) == "io.R" & basename(dirname(paths)) == "builder"
  ]
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    paths <- vapply(
      rev(frames),
      function(frame) {
        path <- get0("file", envir = frame, inherits = FALSE)
        if (is.character(path) && length(path) == 1L) path else ""
      },
      character(1)
    )
    paths <- paths[
      nzchar(paths) &
        basename(paths) == "io.R" &
        basename(dirname(paths)) == "builder" &
        file.exists(paths)
    ]
  }
  if (!length(paths)) "" else normalizePath(paths[[1L]], mustWork = TRUE)
})

.builder_example_inst_root <- local({
  if (!nzchar(.builder_io_source_path)) {
    return("")
  }
  root <- normalizePath(
    dirname(dirname(.builder_io_source_path)),
    mustWork = TRUE
  )
  expected <- normalizePath(
    file.path(root, "builder", "io.R"),
    mustWork = TRUE
  )
  if (!identical(expected, .builder_io_source_path)) "" else root
})

## One row per format: the extensions it claims, the package it needs, and how
## to read with it. Adding a format means adding a row.
##
## There is deliberately no `.qd` row. qs2's qdata format does not serialise S4
## at all: `qd_save()` on a Seurat object warns "Objects of type S4 are not
## supported in qdata format", writes a 38-byte container, and reading it back
## yields NULL. A `.qd` file cannot hold a Seurat object, so listing it among
## the readable formats told users something untrue. `.qs2`, from the same
## package, does work.
builder_formats <- list(
  list(
    id = "rds",
    label = "RDS",
    extensions = "rds",
    package = NULL,
    read = function(path) readRDS(path)
  ),
  list(
    id = "qs2",
    label = "qs2",
    extensions = "qs2",
    package = "qs2",
    read = function(path) qs2::qs_read(path)
  ),
  list(
    id = "qs",
    label = "qs",
    extensions = "qs",
    package = "qs",
    read = function(path) qs::qread(path)
  )
)

#' Which formats can be read right now.
#'
#' @return A data.frame with one row per format: id, label, extensions,
#'   package, available.
builder_available_formats <- function() {
  do.call(
    rbind,
    lapply(builder_formats, function(f) {
      data.frame(
        id = f$id,
        label = f$label,
        extensions = paste(f$extensions, collapse = ", "),
        package = if (is.null(f$package)) NA_character_ else f$package,
        available = is.null(f$package) ||
          requireNamespace(f$package, quietly = TRUE),
        stringsAsFactors = FALSE
      )
    })
  )
}

#' A short line naming what can and cannot be read, for the interface.
builder_format_summary <- function() {
  fmt <- builder_available_formats()
  ok <- fmt$label[fmt$available]
  missing <- fmt[!fmt$available, ]
  out <- paste0("Reads: ", paste(unique(ok), collapse = ", "))
  if (nrow(missing) > 0) {
    out <- paste0(
      out,
      ". Install ",
      paste(unique(missing$package), collapse = ", "),
      " to also read ",
      paste(unique(missing$label), collapse = ", ")
    )
  }
  out
}

#' Pick the format for a path, by extension.
#'
#' @return The format entry, or `NULL` when the extension is not one we claim.
builder_match_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  for (f in builder_formats) {
    if (ext %in% f$extensions) {
      return(f)
    }
  }
  NULL
}

#' Read a serialised object, choosing the reader by file extension.
#'
#' @param path Path to the file.
#'
#' @return A list with `object` on success, or `error` with a sentence saying
#'   what went wrong. Never throws: the caller is a page, not a script.
builder_read_object <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(list(error = "File not found."))
  }
  if (dir.exists(path)) {
    return(list(error = "This path is a directory, not a file."))
  }

  fmt <- builder_match_format(path)
  if (is.null(fmt)) {
    known <- paste(
      unique(unlist(lapply(builder_formats, `[[`, "extensions"))),
      collapse = ", "
    )
    return(list(
      error = paste0(
        "Unsupported .",
        tools::file_ext(path),
        " extension. Supported formats: ",
        known,
        "."
      )
    ))
  }

  if (!is.null(fmt$package) && !requireNamespace(fmt$package, quietly = TRUE)) {
    return(list(
      error = paste0(
        "Reading .",
        tools::file_ext(path),
        " requires the ",
        fmt$package,
        " package. Run install.packages(\"",
        fmt$package,
        "\") or save the object as .rds first."
      )
    ))
  }

  obj <- try(fmt$read(path), silent = TRUE)
  if (inherits(obj, "try-error")) {
    return(list(
      error = paste0(
        "Could not read with ",
        fmt$label,
        ": ",
        conditionMessage(attr(obj, "condition"))
      )
    ))
  }
  if (!inherits(obj, "Seurat")) {
    return(list(
      error = paste0(
        "The file contains a ",
        class(obj)[1],
        " object, not a Seurat object."
      )
    ))
  }
  list(object = obj, format = fmt$label)
}

#' Everything readable in a directory, for the file picker.
builder_list_candidates <- function(dir) {
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(character())
  }
  exts <- unique(unlist(lapply(builder_formats, `[[`, "extensions")))
  pattern <- paste0("[.](", paste(exts, collapse = "|"), ")$")
  sort(list.files(
    dir,
    pattern = pattern,
    ignore.case = TRUE,
    full.names = TRUE
  ))
}

## ---------------------------------------------------------------------------
## Example data, so the tool can be tried before anyone has a file to hand
## ---------------------------------------------------------------------------

.builder_example_path <- function(relative) {
  root <- .builder_example_inst_root
  if (!nzchar(root)) {
    root <- system.file(package = "CerebroNexus")
  }
  if (!nzchar(root)) {
    return("")
  }
  root <- normalizePath(root, mustWork = TRUE)
  path <- file.path(root, relative)
  if (!file.exists(path) && !dir.exists(path)) {
    return("")
  }
  path <- normalizePath(path, mustWork = TRUE)
  inside <- identical(path, root) ||
    startsWith(path, paste0(root, .Platform$file.sep))
  if (!inside) "" else path
}


#' Construct one immutable Builder example catalog record.
builder_example_record <- function(
  id,
  label,
  detail,
  provenance,
  serialized_path,
  make = NULL,
  expected_manifest = character(),
  expected_dispositions = character(),
  expected_pages = character(),
  expected_supporting_content = character(),
  histology_images = list(),
  gallery_visible = TRUE
) {
  require_string <- function(value, name, nonempty = TRUE) {
    valid <- is.character(value) && length(value) == 1L && !is.na(value)
    if (isTRUE(nonempty)) {
      valid <- valid && nzchar(value)
    }
    if (!valid) {
      qualifier <- if (isTRUE(nonempty)) "a non-empty" else "a single"
      stop("`", name, "` must be ", qualifier, " string.", call. = FALSE)
    }
  }
  for (name in c("id", "label", "detail", "provenance")) {
    require_string(get(name), name)
  }
  allowed_provenance <- c("real", "synthetic")
  if (!(provenance %in% allowed_provenance)) {
    stop(
      "`provenance` must be one of: ",
      paste(allowed_provenance, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  require_string(serialized_path, "serialized_path", nonempty = FALSE)
  if (
    !is.character(expected_manifest) ||
      anyNA(expected_manifest) ||
      any(!nzchar(expected_manifest)) ||
      anyDuplicated(expected_manifest) > 0L ||
      !is.null(names(expected_manifest))
  ) {
    stop(
      "`expected_manifest` must contain unique non-empty strings without names.",
      call. = FALSE
    )
  }
  disposition_names <- names(expected_dispositions)
  if (
    length(expected_dispositions) > 0L &&
      (is.null(disposition_names) ||
        anyNA(disposition_names) ||
        any(!nzchar(disposition_names)) ||
        anyDuplicated(disposition_names) > 0L)
  ) {
    stop(
      "Use named `expected_dispositions` with unique non-empty names.",
      call. = FALSE
    )
  }
  allowed_dispositions <- c("preserved", "converted")
  if (
    !is.character(expected_dispositions) ||
      anyNA(expected_dispositions) ||
      any(!nzchar(expected_dispositions)) ||
      any(!(expected_dispositions %in% allowed_dispositions))
  ) {
    stop(
      "`expected_dispositions` must use only: ",
      paste(allowed_dispositions, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!identical(expected_manifest, disposition_names %||% character())) {
    stop(
      "`expected_manifest` must exactly match disposition names.",
      call. = FALSE
    )
  }
  for (name in c("expected_pages", "expected_supporting_content")) {
    value <- get(name)
    if (
      !is.character(value) ||
        anyNA(value) ||
        any(!nzchar(value)) ||
        anyDuplicated(value) > 0L ||
        !is.null(names(value))
    ) {
      stop(
        "`",
        name,
        "` must contain unique non-empty strings without names.",
        call. = FALSE
      )
    }
  }
  if (
    !is.list(histology_images) ||
      (length(histology_images) &&
        (is.null(names(histology_images)) ||
          anyNA(names(histology_images)) ||
          any(!nzchar(names(histology_images))) ||
          anyDuplicated(names(histology_images)) > 0L))
  ) {
    stop(
      "`histology_images` must be a named list with unique non-empty IDs.",
      call. = FALSE
    )
  }
  for (image_id in names(histology_images)) {
    image <- histology_images[[image_id]]
    required <- c("id", "label", "stain", "path", "section_id", "fov_ids")
    if (!is.list(image) || !all(required %in% names(image))) {
      stop(
        "`histology_images` entries must declare id, label, stain, path, ",
        "section_id, and fov_ids.",
        call. = FALSE
      )
    }
    if (!identical(image$id, image_id)) {
      stop(
        "`histology_images` entry IDs must match their list names.",
        call. = FALSE
      )
    }
    for (name in c("id", "label", "stain", "path", "section_id")) {
      require_string(image[[name]], paste0("histology_images$", name))
    }
    if (
      !is.character(image$fov_ids) ||
        !length(image$fov_ids) ||
        anyNA(image$fov_ids) ||
        any(!nzchar(image$fov_ids)) ||
        anyDuplicated(image$fov_ids) > 0L
    ) {
      stop(
        "`histology_images` entries must declare unique non-empty `fov_ids`.",
        call. = FALSE
      )
    }
    if (!image$path %in% expected_supporting_content) {
      stop(
        "Each `histology_images` path must appear in ",
        "`expected_supporting_content`.",
        call. = FALSE
      )
    }
  }
  if (
    !is.logical(gallery_visible) ||
      length(gallery_visible) != 1L ||
      is.na(gallery_visible)
  ) {
    stop("`gallery_visible` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(make)) {
    make <- local({
      path <- serialized_path
      function() {
        if (!nzchar(path) || !file.exists(path)) {
          return(list(error = "The package example object was not found."))
        }
        list(object = readRDS(path), format = "Built-in example")
      }
    })
  }
  if (!is.function(make)) {
    stop("`make` must be a function.", call. = FALSE)
  }
  list(
    id = id,
    label = label,
    detail = detail,
    provenance = provenance,
    make = make,
    serialized_path = serialized_path,
    expected_manifest = expected_manifest,
    expected_dispositions = expected_dispositions,
    expected_pages = expected_pages,
    expected_supporting_content = expected_supporting_content,
    histology_images = histology_images,
    gallery_visible = gallery_visible
  )
}

#' Stable, offline examples covering every Builder content family.
builder_example_catalog <- function() {
  fixture <- function(name) {
    .builder_example_path(
      file.path("builder", "fixtures", name)
    )
  }
  core <- function(reductions = "umap", groups = "preserved") {
    values <- stats::setNames(
      rep("preserved", 5L + length(reductions)),
      c(
        "dataset_identity",
        "expression",
        "metadata",
        "groups",
        paste0("reduction:", reductions),
        "projection"
      )
    )
    values[["groups"]] <- groups
    values
  }
  with_content <- function(core_dispositions, ...) {
    c(core_dispositions, unlist(list(...), use.names = TRUE))
  }
  record <- function(..., expected_dispositions = character()) {
    expected_dispositions <- c(
      expected_dispositions,
      metadata_policy = "preserved"
    )
    builder_example_record(
      ...,
      expected_manifest = names(expected_dispositions),
      expected_dispositions = expected_dispositions
    )
  }
  histology_images <- list(
    section_a_1_he = list(
      id = "section_a_1_he",
      label = "H&E",
      stain = "H&E",
      path = "section_a_1_he.png",
      section_id = "section_a_1",
      fov_ids = "section_a_1_fov_1"
    ),
    section_a_1_dapi = list(
      id = "section_a_1_dapi",
      label = "DAPI",
      stain = "DAPI",
      path = "section_a_1_dapi.png",
      section_id = "section_a_1",
      fov_ids = "section_a_1_fov_1"
    ),
    section_a_2_he = list(
      id = "section_a_2_he",
      label = "H&E",
      stain = "H&E",
      path = "section_a_2_he.png",
      section_id = "section_a_2",
      fov_ids = "section_a_2_fov_1"
    ),
    section_a_2_dapi = list(
      id = "section_a_2_dapi",
      label = "DAPI",
      stain = "DAPI",
      path = "section_a_2_dapi.png",
      section_id = "section_a_2",
      fov_ids = "section_a_2_fov_1"
    ),
    section_b_1_he = list(
      id = "section_b_1_he",
      label = "H&E",
      stain = "H&E",
      path = "section_b_1_he.png",
      section_id = "section_b_1",
      fov_ids = "section_b_1_fov_1"
    ),
    section_b_1_if = list(
      id = "section_b_1_if",
      label = "IF",
      stain = "IF",
      path = "section_b_1_if.png",
      section_id = "section_b_1",
      fov_ids = "section_b_1_fov_1"
    ),
    section_b_1_pas = list(
      id = "section_b_1_pas",
      label = "PAS",
      stain = "PAS",
      path = "section_b_1_pas.png",
      section_id = "section_b_1",
      fov_ids = "section_b_1_fov_1"
    )
  )
  all_content <- record(
    "all_content",
    "All content",
    paste(
      "Synthetic Seurat with three patients, six measured sections/FOVs,",
      "seven histology images, and Trekker"
    ),
    "synthetic",
    fixture("all_content.rds"),
    expected_dispositions = with_content(
      core(c("pca", "umap", "tsne")),
      spatial = "preserved",
      trekker = "preserved"
    ),
    expected_pages = c("spatial", "trekker"),
    expected_supporting_content = unname(vapply(
      histology_images,
      `[[`,
      character(1),
      "path"
    )),
    histology_images = histology_images
  )
  list(all_content = all_content)
}

#' Static first-paint directory for the example picker.
builder_example_directory <- local({
  directory <- list(
    list(
      id = "all_content",
      label = "All content",
      detail = paste(
        "Synthetic Seurat with three patients, six measured sections/FOVs,",
        "seven histology images, and Trekker"
      ),
      source = "builder/fixtures/all_content.rds"
    )
  )
  names(directory) <- vapply(directory, `[[`, character(1), "id")
  function() directory
})

#' Gallery projection of the stable example catalog.
builder_examples <- function() {
  lapply(
    Filter(
      function(record) isTRUE(record$gallery_visible),
      builder_example_catalog()
    ),
    function(record) record[c("id", "label", "detail", "make")]
  )
}

#' A small spatial object, generated rather than shipped.
#'
#' Built here rather than stored as a fixture so it costs no repository space
#' and always matches the Seurat version in use.
builder_make_spatial_example <- function() {
  if (!requireNamespace("SeuratObject", quietly = TRUE)) {
    return(list(error = "The SeuratObject package is required."))
  }
  set.seed(1)
  n_cells <- 60
  n_genes <- 40
  counts <- matrix(
    stats::rpois(n_genes * n_cells, lambda = 3),
    nrow = n_genes,
    dimnames = list(
      paste0("Gene", seq_len(n_genes)),
      paste0("Cell", seq_len(n_cells))
    )
  )
  obj <- SeuratObject::CreateSeuratObject(counts = counts)
  obj$sample <- rep(c("S1", "S2"), length.out = n_cells)
  obj$seurat_clusters <- factor(rep(c("C1", "C2"), length.out = n_cells))
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      stats::rnorm(n_cells * 2),
      ncol = 2,
      dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )
  coords <- data.frame(
    x = stats::runif(n_cells, 2, 99),
    y = stats::runif(n_cells, 2, 99),
    cell = colnames(obj),
    stringsAsFactors = FALSE
  )
  ## `CreateCentroids` is what carries the cell names through; handing
  ## `CreateFOV` a bare coordinate frame loses them and the assignment then
  ## fails with "Cannot add new cells".
  fov <- try(
    SeuratObject::CreateFOV(
      coords = list(centroids = SeuratObject::CreateCentroids(coords)),
      type = "centroids",
      assay = "RNA"
    ),
    silent = TRUE
  )
  if (inherits(fov, "try-error")) {
    return(list(error = "This Seurat version cannot create an FOV."))
  }
  obj[["fov"]] <- fov
  list(object = obj, format = "Built-in example")
}
