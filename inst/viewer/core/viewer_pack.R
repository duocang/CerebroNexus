VIEWER_PACK_SCHEMA_VERSION <- 1L

viewerPackCellOrderFingerprint <- function(cells) {
  cells <- enc2utf8(as.character(cells))
  if (anyNA(cells) || any(!nzchar(cells)) || anyDuplicated(cells)) {
    return(NA_character_)
  }
  path <- tempfile("viewer-pack-cell-order-")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")),
    path
  )
  paste0("md5-cell-order-v1:", unname(tools::md5sum(path)))
}

viewerPackOpen <- function(file, object) {
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !file.exists(file) ||
      dir.exists(file)
  ) {
    return(NULL)
  }
  file <- normalizePath(file, mustWork = TRUE)
  pack <- file.path(
    dirname(file),
    paste0(tools::file_path_sans_ext(basename(file)), ".viewer")
  )
  manifest_file <- file.path(pack, "manifest.json")
  if (!dir.exists(pack) || !file.exists(manifest_file)) {
    return(NULL)
  }
  manifest <- tryCatch(
    jsonlite::read_json(manifest_file, simplifyVector = TRUE),
    error = function(error) NULL
  )
  if (
    !is.list(manifest) ||
      !identical(manifest$schema_version, VIEWER_PACK_SCHEMA_VERSION) ||
      !identical(
        manifest$dataset_fingerprint,
        paste0("md5-crb-v1:", unname(tools::md5sum(file)))
      )
  ) {
    return(NULL)
  }
  cell_count <- suppressWarnings(as.integer(manifest$n_cells))
  if (
    length(cell_count) != 1L ||
      is.na(cell_count) ||
      cell_count < 0L ||
      !identical(as.numeric(cell_count), as.numeric(manifest$n_cells))
  ) {
    return(NULL)
  }
  assets <- manifest$assets
  if (
    !is.data.frame(assets) ||
      !all(c("path", "bytes", "checksum") %in% colnames(assets)) ||
      any(!file.exists(file.path(pack, assets$path)))
  ) {
    return(NULL)
  }
  list(
    path = pack,
    manifest = manifest,
    cells = NULL,
    cell_count = cell_count,
    canonical_order = TRUE,
    cache = new.env(parent = emptyenv())
  )
}

viewerPackValidateCellOrder <- function(pack) {
  if (isTRUE(pack$canonical_order)) {
    return(TRUE)
  }
  key <- ".cell_order_valid"
  if (exists(key, envir = pack$cache, inherits = FALSE)) {
    return(isTRUE(get(key, envir = pack$cache, inherits = FALSE)))
  }
  path <- "common/cell_order.qs2"
  assets <- pack$manifest$assets
  row <- which(as.character(assets$path) == path)
  file <- file.path(pack$path, path)
  valid <- length(row) == 1L &&
    file.exists(file) &&
    !dir.exists(file) &&
    identical(
      as.numeric(file.info(file)$size),
      as.numeric(assets$bytes[[row]])
    ) &&
    identical(
      unname(tools::md5sum(file)),
      as.character(assets$checksum[[row]])
    )
  stored <- if (valid) {
    tryCatch(qs2::qs_read(file), error = function(error) NULL)
  } else {
    NULL
  }
  valid <- is.character(stored) && identical(stored, pack$cells)
  if (valid) {
    assign(path, stored, envir = pack$cache)
  }
  assign(key, valid, envir = pack$cache)
  valid
}

viewerPackReadAsset <- function(pack, path, validate_cell_order = TRUE) {
  if (is.null(pack) || !is.list(pack) || !is.environment(pack$cache)) {
    return(NULL)
  }
  if (isTRUE(validate_cell_order) && !viewerPackValidateCellOrder(pack)) {
    return(NULL)
  }
  if (exists(path, envir = pack$cache, inherits = FALSE)) {
    return(get(path, envir = pack$cache, inherits = FALSE))
  }
  assets <- pack$manifest$assets
  row <- which(as.character(assets$path) == path)
  if (length(row) != 1L) {
    return(NULL)
  }
  file <- file.path(pack$path, path)
  valid <- file.exists(file) &&
    !dir.exists(file) &&
    identical(
      as.numeric(file.info(file)$size),
      as.numeric(assets$bytes[[row]])
    ) &&
    identical(unname(tools::md5sum(file)), as.character(assets$checksum[[row]]))
  if (!valid) {
    return(NULL)
  }
  value <- tryCatch(qs2::qs_read(file), error = function(error) NULL)
  if (!is.null(value)) {
    assign(path, value, envir = pack$cache)
  }
  value
}

viewerPackCellBarcodes <- function(pack, index = NULL) {
  cells <- pack$cells
  if (is.null(cells)) {
    cells <- viewerPackReadAsset(
      pack,
      "common/cell_order.qs2",
      validate_cell_order = FALSE
    )
  }
  if (
    !is.character(cells) ||
      length(cells) != pack$cell_count ||
      anyNA(cells) ||
      any(!nzchar(cells))
  ) {
    return(NULL)
  }
  if (is.null(index)) {
    return(cells)
  }
  index <- suppressWarnings(as.integer(index))
  if (anyNA(index) || any(index < 1L | index > length(cells))) {
    return(NULL)
  }
  cells[index]
}

viewerPackTrajectoryIndex <- function(pack, method, name) {
  indexes <- viewerPackReadAsset(
    pack,
    file.path("trajectory", "cell_index.qs2"),
    validate_cell_order = FALSE
  )
  index <- indexes[[method]][[name]]
  if (is.null(index)) NULL else as.integer(index)
}

viewerPackHlaSegments <- function(pack, chain) {
  if (
    !is.character(chain) || length(chain) != 1L || !chain %in% c("TRA", "TRB")
  ) {
    return(NULL)
  }
  viewerPackReadAsset(pack, file.path("hla_tcr", paste0(chain, ".qs2")))
}

viewerPackHlaFirstFrame <- function(pack, chain) {
  if (
    !is.character(chain) || length(chain) != 1L || !chain %in% c("TRA", "TRB")
  ) {
    return(NULL)
  }
  viewerPackReadAsset(
    pack,
    file.path("hla_tcr", paste0(chain, ".first.qs2")),
    validate_cell_order = FALSE
  )
}

viewerPackImmuneIndex <- function(pack, receptor) {
  if (
    !is.character(receptor) ||
      length(receptor) != 1L ||
      !receptor %in% c("TCR", "BCR")
  ) {
    return(NULL)
  }
  viewerPackReadAsset(pack, file.path("immune", paste0(receptor, ".qs2")))
}

viewerPackImmuneAbundance <- function(pack, clone_col) {
  values <- viewerPackReadAsset(pack, "immune/abundance.qs2")
  if (is.null(values) || !(clone_col %in% names(values))) {
    return(NULL)
  }
  values[[clone_col]]
}
