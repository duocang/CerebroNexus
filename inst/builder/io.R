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

.builder_fixture_with_seed <- function(seed, code) {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) {
    caller_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (seed_exists) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed)
  force(code)
}

.builder_fixture_stabilize <- function(object) {
  fixed_time <- as.POSIXct("2026-08-06 00:00:00", tz = "UTC")
  if (length(object@commands)) {
    for (name in names(object@commands)) {
      object@commands[[name]]@time.stamp <- fixed_time
    }
  }
  object
}

.builder_fixture_object <- function(
  n_cells,
  n_genes = 40L,
  samples = "donorA",
  organism = c("hg", "mm")
) {
  organism <- match.arg(organism)
  cells <- paste0("cell", seq_len(n_cells))
  prefix <- if (organism == "hg") {
    c("MT-", "RPS", "RPL")
  } else {
    c("mt-", "Rps", "Rpl")
  }
  genes <- c(
    paste0(prefix[1L], c("CO1", "ND1", "CYB")),
    paste0(prefix[2L], 3:6),
    paste0(prefix[3L], 3:6),
    paste0("Gene", seq_len(n_genes - 11L))
  )
  counts <- Matrix::Matrix(
    stats::rpois(length(genes) * n_cells, lambda = 3),
    nrow = length(genes),
    dimnames = list(genes, cells),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(counts = counts)
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object$sample <- factor(sample(samples, n_cells, replace = TRUE))
  object$cell_type <- factor(
    sample(c("Neuron", "Astrocyte", "Microglia"), n_cells, replace = TRUE)
  )
  object$condition <- factor(sample(c("control", "treated"), n_cells, TRUE))
  embeddings <- matrix(
    stats::rnorm(n_cells * 2L),
    ncol = 2L,
    dimnames = list(cells, c("UMAP_1", "UMAP_2"))
  )
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings,
    key = "UMAP_",
    assay = "RNA"
  )
  object
}

.builder_fixture_add_section <- function(
  object,
  name,
  cells,
  origin,
  span
) {
  coordinates <- data.frame(
    x = stats::runif(length(cells), 0, span[1L]) + origin[1L],
    y = stats::runif(length(cells), 0, span[2L]) + origin[2L],
    cell = cells
  )
  object[[name]] <- SeuratObject::CreateFOV(
    coords = list(
      centroids = SeuratObject::CreateCentroids(coordinates)
    ),
    type = "centroids",
    assay = "RNA",
    key = paste0(tolower(name), "_")
  )
  object
}

.builder_fixture_immune_repertoire <- function(object, chain) {
  cells <- colnames(object)
  by_sample <- split(cells, as.character(object$sample))
  prefix <- switch(
    chain,
    TRA = "TRAV1.TRAJ1",
    TRB = "TRBV1.TRBJ1",
    IGH = "IGHV1.IGHJ1"
  )
  lapply(by_sample, function(barcodes) {
    data.frame(
      barcode = barcodes,
      CTgene = rep(prefix, length(barcodes)),
      CTnt = paste0("ACGT", seq_along(barcodes)),
      CTaa = paste0("CASSLGQ", seq_along(barcodes), "F"),
      CTstrict = paste0(chain, "_clone_", seq_along(barcodes)),
      stringsAsFactors = FALSE
    )
  })
}

.builder_fixture_hla <- function(samples) {
  typing <- expand.grid(
    sample = samples,
    locus = c("A", "B", "C"),
    copy = 1:2,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  typing$donor_id <- typing$sample
  typing$allele <- sprintf(
    "HLA-%s*%02d:%02d",
    typing$locus,
    seq_len(nrow(typing)),
    typing$copy
  )
  typing$locus <- paste0("HLA-", typing$locus)
  typing$resolution <- "2-field"
  typing[, c("sample", "donor_id", "locus", "copy", "allele", "resolution")]
}

.builder_fixture_immune <- function(mode) {
  object <- .builder_fixture_object(
    24L,
    samples = c("donor1", "donor2")
  )
  if (mode %in% c("tcr_hla", "tcr_only")) {
    object@misc$immune_repertoire <-
      .builder_fixture_immune_repertoire(object, "TRB")
  }
  if (identical(mode, "bcr_only")) {
    object@misc$immune_repertoire <-
      .builder_fixture_immune_repertoire(object, "IGH")
  }
  if (mode %in% c("tcr_hla", "hla_only")) {
    object@misc$hla_typing <- .builder_fixture_hla(levels(object$sample))
    object@misc$hla_typing_source_type <- "synthetic"
  }
  if (identical(mode, "metadata_tcr")) {
    table <- do.call(
      rbind,
      .builder_fixture_immune_repertoire(object, "TRB")
    )
    table <- table[match(colnames(object), table$barcode), , drop = FALSE]
    for (column in c("CTgene", "CTnt", "CTaa", "CTstrict")) {
      object@meta.data[[column]] <- table[[column]]
    }
  }
  if (identical(mode, "legacy_tcr")) {
    object@misc$tcr_data <- .builder_fixture_immune_repertoire(object, "TRB")
  }
  object
}

.builder_fixture_spatial <- function() {
  object <- .builder_fixture_object(30L, organism = "mm")
  cells <- colnames(object)
  object <- .builder_fixture_add_section(
    object,
    "section_a",
    cells[seq_len(15L)],
    c(10, 20),
    c(96, 72)
  )
  object <- .builder_fixture_add_section(
    object,
    "section_b",
    cells[16:30],
    c(250, 40),
    c(80, 64)
  )
  object[["tsne"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      SeuratObject::Embeddings(object, "umap") + 0.5,
      ncol = 2L,
      dimnames = list(cells, c("TSNE_1", "TSNE_2"))
    ),
    key = "TSNE_",
    assay = "RNA"
  )
  object
}

.builder_fixture_all_content <- function() {
  object <- .builder_fixture_immune("tcr_hla")
  cells <- colnames(object)
  object <- .builder_fixture_add_section(
    object,
    "section_a",
    cells[seq_len(12L)],
    c(10, 20),
    c(96, 72)
  )
  object <- .builder_fixture_add_section(
    object,
    "section_b",
    cells[13:24],
    c(250, 40),
    c(80, 64)
  )
  object@misc$marker_genes <- list(
    cerebro_seurat = list(
      cell_type = data.frame(
        cell_type = c("Neuron", "Astrocyte"),
        gene = c("Gene1", "Gene2"),
        avg_log2FC = c(1, 0.8),
        stringsAsFactors = FALSE
      )
    )
  )
  object@misc$most_expressed_genes <- list(
    cell_type = data.frame(
      cell_type = c("Neuron", "Astrocyte"),
      gene = c("Gene1", "Gene2"),
      pct = c(90, 85),
      stringsAsFactors = FALSE
    )
  )
  object@misc$mean_expression <- list(
    cell_type = data.frame(
      cell_type = c("Neuron", "Astrocyte"),
      gene = c("Gene1", "Gene2"),
      mean_expr = c(2.5, 1.8),
      stringsAsFactors = FALSE
    )
  )
  object@misc$enriched_pathways <- list(
    synthetic = list(
      cell_type = data.frame(
        cell_type = c("Neuron", "Astrocyte"),
        Term = c("Synapse", "Glial differentiation"),
        Combined.Score = c(8.2, 7.5),
        stringsAsFactors = FALSE
      )
    )
  )
  object@misc$extra_material <- list(
    tables = list(
      summary = data.frame(metric = "cells", value = ncol(object))
    )
  )
  trajectory_cells <- cells[c(1L, 3L)]
  object@misc$trajectories <- list(
    monocle2 = list(
      lineage = list(
        meta = data.frame(
          DR_1 = c(0.1, 0.8),
          DR_2 = c(0.2, 0.7),
          pseudotime = c(0, 1),
          state = factor(c("1", "2"), levels = c("1", "2")),
          row.names = trajectory_cells,
          check.names = FALSE
        ),
        edges = data.frame(
          source = "n1",
          target = "n2",
          weight = 1,
          source_dim_1 = 0.1,
          source_dim_2 = 0.2,
          target_dim_1 = 0.8,
          target_dim_2 = 0.7,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    )
  )
  trekker <- readRDS(
    .builder_example_path("extdata/examples/demo_trekker.crb")
  )$getTrekker()
  keep <- seq_along(cells)
  for (name in c("x", "y", "ux", "uy", "clusters")) {
    trekker[[name]] <- trekker[[name]][keep]
  }
  trekker$barcodes <- cells
  trekker$fields <- lapply(trekker$fields, function(field) {
    field$v <- field$v[keep]
    field
  })
  trekker$conf <- lapply(trekker$conf, function(values) values[keep])
  trekker$moran <- utils::head(trekker$moran, 8L)
  for (index in seq_along(trekker$moran)) {
    trekker$moran[[index]]$gene <- paste0("Gene", index)
  }
  trekker$evidence <- list()
  trekker$qc_examples <- list()
  trekker$meta$n_cells_full <- length(cells)
  trekker$meta$n_cells <- length(cells)
  trekker$meta$n_genes_obj <- nrow(object)
  trekker$meta$generated <- "2026-08-06T00:00:00+0000"
  trekker$qc$total_nuclei <- length(cells)
  trekker$qc$in_lib <- length(cells)
  trekker$qc$positioned <- length(cells)
  trekker$qc$conf <- length(cells)
  trekker$qc$o_1 <- length(cells)
  trekker$qc$n_0 <- 0
  trekker$qc$n_1 <- length(cells)
  trekker$qc$n_2 <- 0
  trekker$qc$n_3 <- 0
  trekker$qc$n_4p <- 0
  trekker$qc$salv_2 <- 0
  trekker$qc$salv_3 <- 0
  object@misc$trekker <- trekker
  object
}

builder_make_permanent_fixture <- function(id) {
  .builder_fixture_with_seed(2026L, {
    modes <- c(
      immune_tcr_hla = "tcr_hla",
      immune_tcr_only = "tcr_only",
      immune_hla_only = "hla_only",
      immune_bcr_only = "bcr_only",
      immune_metadata_tcr = "metadata_tcr",
      immune_legacy_tcr = "legacy_tcr"
    )
    object <- if (identical(id, "spatial_multi_section")) {
      .builder_fixture_spatial()
    } else if (identical(id, "all_content")) {
      .builder_fixture_all_content()
    } else if (id %in% names(modes)) {
      .builder_fixture_immune(unname(modes[[id]]))
    } else {
      stop("Unknown permanent Builder fixture: ", id, call. = FALSE)
    }
    .builder_fixture_stabilize(object)
  })
}

.builder_fixture_write_tissue_png <- function(path, width, height, seed) {
  .builder_fixture_with_seed(seed, {
    gx <- matrix(rep(seq_len(width), each = height), nrow = height)
    gy <- matrix(rep(seq_len(height), times = width), nrow = height)
    band <- 0.5 + 0.35 * sin(gy / height * 6 * pi) * cos(gx / width * 2 * pi)
    vignette <- 1 -
      0.5 *
        (((gx - width / 2) / width)^2 +
          ((gy - height / 2) / height)^2) *
        4
    vignette[vignette < 0] <- 0
    red <- pmin(1, band * vignette * 1.05)
    green <- pmin(1, band * vignette * 0.75)
    blue <- pmin(1, band * vignette * 0.95)
    png::writePNG(
      array(c(red, green, blue), dim = c(height, width, 3)),
      path
    )
  })
  invisible(path)
}

builder_write_permanent_fixtures <- function(output_dir) {
  .builder_fixture_with_seed(2026L, {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    .builder_fixture_write_tissue_png(
      file.path(output_dir, "spatial_section_a.png"),
      96L,
      72L,
      1401L
    )
    .builder_fixture_write_tissue_png(
      file.path(output_dir, "spatial_section_b.png"),
      80L,
      64L,
      1402L
    )
    ids <- c(
      "spatial_multi_section",
      "immune_tcr_hla",
      "immune_tcr_only",
      "immune_hla_only",
      "immune_bcr_only",
      "immune_metadata_tcr",
      "immune_legacy_tcr",
      "all_content"
    )
    for (id in ids) {
      saveRDS(
        builder_make_permanent_fixture(id),
        file.path(output_dir, paste0(id, ".rds")),
        version = 3L
      )
    }
  })
  invisible(output_dir)
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
  synthetic_make <- function(id) {
    force(id)
    function() {
      list(
        object = builder_make_permanent_fixture(id),
        format = "Built-in example"
      )
    }
  }
  records <- list(
    record(
      "basic_pbmc",
      "Basic PBMC",
      "A compact real PBMC object for the core expression workflow",
      "real",
      .builder_example_path("extdata/examples/pbmc_seurat.rds"),
      expected_dispositions = with_content(
        core(c("pca", "umap")),
        marker_genes = "preserved"
      ),
      expected_pages = "marker_genes"
    ),
    record(
      "spatial_multi_section",
      "Spatial multi-section",
      "Two synthetic tissue sections with independent image alignment",
      "synthetic",
      fixture("spatial_multi_section.rds"),
      make = synthetic_make("spatial_multi_section"),
      expected_dispositions = with_content(
        core(c("umap", "tsne")),
        spatial = "preserved"
      ),
      expected_pages = "spatial",
      expected_supporting_content = c(
        "spatial_section_a.png",
        "spatial_section_b.png"
      )
    ),
    record(
      "immune_tcr_hla",
      "Immune: TCR + HLA",
      "T-cell receptor data with donor HLA typing",
      "synthetic",
      fixture("immune_tcr_hla.rds"),
      make = synthetic_make("immune_tcr_hla"),
      expected_dispositions = with_content(
        core(),
        immune_repertoire = "preserved",
        hla_tcr_motifs = "preserved",
        hla = "preserved"
      ),
      expected_pages = c("immune_repertoire", "hla_tcr_motifs")
    ),
    record(
      "immune_tcr_only",
      "Immune: TCR only",
      "T-cell receptor data without HLA typing",
      "synthetic",
      fixture("immune_tcr_only.rds"),
      make = synthetic_make("immune_tcr_only"),
      expected_dispositions = with_content(
        core(),
        immune_repertoire = "preserved",
        hla_tcr_motifs = "preserved"
      ),
      expected_pages = c("immune_repertoire", "hla_tcr_motifs")
    ),
    record(
      "immune_hla_only",
      "Immune: HLA only",
      "HLA typing without an immune repertoire",
      "synthetic",
      fixture("immune_hla_only.rds"),
      make = synthetic_make("immune_hla_only"),
      expected_dispositions = with_content(core(), hla = "preserved")
    ),
    record(
      "immune_bcr_only",
      "Immune: BCR only",
      "B-cell receptor data without TCR chains",
      "synthetic",
      fixture("immune_bcr_only.rds"),
      make = synthetic_make("immune_bcr_only"),
      expected_dispositions = with_content(
        core(),
        immune_repertoire = "preserved"
      ),
      expected_pages = "immune_repertoire"
    ),
    record(
      "immune_metadata_tcr",
      "Immune: metadata TCR",
      "TCR fields in metadata for Builder conversion",
      "synthetic",
      fixture("immune_metadata_tcr.rds"),
      make = synthetic_make("immune_metadata_tcr"),
      expected_dispositions = with_content(
        core(groups = "converted"),
        immune_repertoire = "converted",
        hla_tcr_motifs = "converted"
      ),
      expected_pages = c("immune_repertoire", "hla_tcr_motifs")
    ),
    record(
      "immune_legacy_tcr",
      "Immune: legacy TCR",
      "Legacy TCR payload for Builder conversion",
      "synthetic",
      fixture("immune_legacy_tcr.rds"),
      make = synthetic_make("immune_legacy_tcr"),
      expected_dispositions = with_content(
        core(),
        immune_repertoire = "converted",
        hla_tcr_motifs = "converted"
      ),
      expected_pages = c("immune_repertoire", "hla_tcr_motifs")
    ),
    record(
      "all_content",
      "All content",
      "Expression, immune, HLA, spatial, Trekker, and supporting content",
      "synthetic",
      fixture("all_content.rds"),
      make = synthetic_make("all_content"),
      expected_dispositions = with_content(
        core(),
        marker_genes = "preserved",
        most_expressed_genes = "preserved",
        mean_expression = "preserved",
        enriched_pathways = "preserved",
        trajectory = "preserved",
        extra_material = "preserved",
        immune_repertoire = "preserved",
        hla_tcr_motifs = "preserved",
        hla = "preserved",
        spatial = "preserved",
        trekker = "preserved"
      ),
      expected_pages = c(
        "marker_genes",
        "most_expressed_genes",
        "enriched_pathways",
        "extra_material",
        "immune_repertoire",
        "trajectory",
        "spatial",
        "trekker",
        "hla_tcr_motifs"
      ),
      expected_supporting_content = c(
        "spatial_section_a.png",
        "spatial_section_b.png",
        "extra_material"
      )
    )
  )
  names(records) <- vapply(records, `[[`, character(1), "id")
  records
}

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
