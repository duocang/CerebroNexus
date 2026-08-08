builder_content_spatial_inst_path <- function(...) {
  relative <- file.path(...)
  path <- testthat::test_path("..", "..", "inst", relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  path
}

builder_content_spatial_source_runtime <- function(local = parent.frame()) {
  files <- c(
    builder_content_spatial_inst_path(
      "viewer",
      "core",
      "spatial_coordinate_contract.R"
    ),
    builder_content_spatial_inst_path(
      "builder",
      "spatial.R"
    ),
    builder_content_spatial_inst_path(
      "builder",
      "content_spatial.R"
    )
  )
  for (file in files[nzchar(files) & file.exists(files)]) {
    sys.source(file, envir = local)
  }
  invisible(files)
}

builder_content_spatial_example_object <- local({
  cached <- NULL

  function(section_names = "fov") {
    if (is.null(cached)) {
      environment <- new.env(parent = baseenv())
      sys.source(
        builder_content_spatial_inst_path("builder", "io.R"),
        envir = environment
      )
      built <- suppressWarnings(environment$builder_make_spatial_example())
      if (!is.null(built$error)) {
        stop(built$error, call. = FALSE)
      }
      cached <<- built$object
    }

    object <- unserialize(serialize(cached, NULL))
    image <- methods::slot(object, "images")[[1L]]
    images <- rep(list(image), length(section_names))
    names(images) <- section_names
    methods::slot(object, "images", check = FALSE) <- images
    object
  }
})

builder_content_spatial_context <- function(object) {
  cells <- SeuratObject::Cells(object)
  features <- SeuratObject::Features(object)
  caller <- parent.frame()
  metadata <- get(
    "builder_profile_metadata",
    envir = caller,
    inherits = TRUE
  )(object@meta.data, cells)
  assays <- get(
    "builder_profile_assays",
    envir = caller,
    inherits = TRUE
  )(object, cells)
  reductions <- get(
    "builder_profile_reductions",
    envir = caller,
    inherits = TRUE
  )(object, cells)
  list(
    cells = cells,
    features = features,
    metadata = metadata,
    assays = assays,
    default_assay = SeuratObject::DefaultAssay(object),
    groups = metadata$groups,
    reductions = reductions,
    source = builder_profile_source_fixture()
  )
}

.builder_content_spatial_demo_payload <- local({
  cached <- NULL

  function() {
    if (!is.null(cached)) {
      return(cached)
    }
    path <- builder_content_spatial_inst_path(
      "extdata",
      "examples",
      "demo_trekker.crb"
    )
    object <- readRDS(path)
    payload <- object$getTrekker()
    evidence_index <- payload$evidence[[1L]]$cell + 1L
    keep <- unique(c(seq_len(4L), evidence_index))

    for (name in c("barcodes", "x", "y", "ux", "uy", "clusters")) {
      payload[[name]] <- payload[[name]][keep]
    }
    payload$fields <- lapply(payload$fields, function(field) {
      field$v <- field$v[keep]
      field
    })
    payload$conf <- lapply(payload$conf, function(values) values[keep])
    payload$moran <- utils::head(payload$moran, 3L)
    payload$evidence <- lapply(
      Filter(
        function(entry) (entry$cell + 1L) %in% keep,
        payload$evidence
      ),
      function(entry) {
        entry$cell <- match(entry$cell + 1L, keep) - 1L
        entry
      }
    )
    payload$meta$n_cells <- length(keep)
    payload$meta$n_cells_full <- length(keep)
    payload$qc$total_nuclei <- length(keep)
    payload$qc$in_lib <- length(keep)
    payload$qc$positioned <- length(keep)
    payload$qc$conf <- length(keep)
    payload$qc$o_1 <- length(keep)
    payload$qc$n_0 <- 0
    payload$qc$n_1 <- length(keep)
    payload$qc$n_2 <- 0
    payload$qc$n_3 <- 0
    payload$qc$n_4p <- 0
    payload$qc$salv_2 <- 0
    payload$qc$salv_3 <- 0
    cached <<- payload
    payload
  }
})

builder_content_spatial_trekker_context <- function(payload) {
  genes <- vapply(payload$moran, function(entry) entry$gene, character(1))
  list(
    cells = c(payload$barcodes, "dataset-only-cell"),
    features = unique(c(genes, "dataset-only-gene")),
    metadata = list(),
    assays = list(),
    default_assay = "RNA",
    groups = list(),
    reductions = list(),
    source = builder_profile_source_fixture()
  )
}

builder_content_spatial_mutate <- function(payload, expression) {
  copy <- unserialize(serialize(payload, NULL))
  environment <- list2env(copy, parent = baseenv())
  eval(substitute(expression), envir = environment)
  mget(names(copy), envir = environment, inherits = FALSE)
}

builder_content_spatial_expect_record <- function(record) {
  expect_named(
    record,
    c(
      "detected",
      "valid",
      "normalized",
      "diagnostics",
      "requirements",
      "page_candidates"
    ),
    ignore.order = TRUE
  )
  expect_type(record$detected, "logical")
  expect_length(record$detected, 1L)
  expect_false(is.na(record$detected))
  expect_type(record$valid, "logical")
  expect_length(record$valid, 1L)
  expect_false(is.na(record$valid))
  expect_type(record$diagnostics, "character")
  expect_false(anyNA(record$diagnostics))
  expect_type(record$requirements, "character")
  expect_false(anyNA(record$requirements))
  expect_type(record$page_candidates, "character")
  expect_false(anyNA(record$page_candidates))
  expect_false(anyDuplicated(record$page_candidates) > 0L)
}
