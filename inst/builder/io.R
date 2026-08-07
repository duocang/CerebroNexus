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

#' Objects that can be produced without the user supplying anything.
#'
#' `pbmc_small` ships with SeuratObject, so it is there wherever Seurat is.
#' The other two come from this package's own test fixtures.
builder_examples <- function() {
  list(
    list(
      id = "pbmc_small",
      label = "pbmc_small",
      detail = "The 80-cell PBMC subset included with Seurat",
      make = function() {
        if (!requireNamespace("SeuratObject", quietly = TRUE)) {
          return(list(error = "The SeuratObject package is required."))
        }
        obj <- SeuratObject::pbmc_small
        list(object = obj, format = "Built-in example")
      }
    ),
    list(
      id = "pbmc_repo",
      label = "PBMC (package example)",
      detail = "An 80-cell object with marker genes from package extdata",
      make = function() {
        path <- system.file(
          "extdata/v1.4/pbmc_seurat.rds",
          package = "CerebroNexus"
        )
        if (!nzchar(path)) {
          return(list(error = "The package example object was not found."))
        }
        list(object = readRDS(path), format = "Built-in example")
      }
    ),
    list(
      id = "spatial_synthetic",
      label = "Spatial example (synthetic)",
      detail = "60 cells with coordinates for testing image alignment",
      make = function() builder_make_spatial_example()
    )
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

## ---------------------------------------------------------------------------
## Browsing the filesystem
## ---------------------------------------------------------------------------

#' One directory's worth of entries, for the file browser.
#'
#' Directories first, then files this tool can actually read. Anything else is
#' left out rather than shown greyed: a listing full of things you cannot pick
#' is harder to scan than a short one.
builder_browse <- function(dir) {
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(list(error = "This directory does not exist."))
  }
  exts <- unique(unlist(lapply(builder_formats, `[[`, "extensions")))
  entries <- list.files(dir, all.files = FALSE, full.names = TRUE)
  info <- file.info(entries)

  dirs <- basename(entries[which(info$isdir)])
  files <- entries[which(!info$isdir)]
  keep <- tolower(tools::file_ext(files)) %in% exts
  files <- files[keep]
  sizes <- file.info(files)$size

  list(
    dir = normalizePath(dir, mustWork = FALSE),
    parent = normalizePath(dirname(dir), mustWork = FALSE),
    dirs = sort(dirs),
    files = data.frame(
      name = basename(files),
      path = files,
      size = sizes,
      stringsAsFactors = FALSE
    )[order(basename(files)), , drop = FALSE]
  )
}

#' Places worth starting from.
builder_browse_roots <- function() {
  home <- path.expand("~")
  candidates <- c(
    "Home" = home,
    "Desktop" = file.path(home, "Desktop"),
    "Documents" = file.path(home, "Documents"),
    "Working directory" = getwd(),
    "Filesystem root" = "/"
  )
  candidates[dir.exists(candidates)]
}
