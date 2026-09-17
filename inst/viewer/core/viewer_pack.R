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
  metadata <- tryCatch(object$getMetaData(), error = function(error) NULL)
  if (!is.data.frame(metadata) || !("cell_barcode" %in% colnames(metadata))) {
    return(NULL)
  }
  cells <- as.character(metadata$cell_barcode)
  if (!identical(as.integer(manifest$n_cells), length(cells))) {
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
    cells = cells,
    cache = new.env(parent = emptyenv())
  )
}

viewerPackValidateCellOrder <- function(pack) {
  key <- ".cell_order_valid"
  if (exists(key, envir = pack$cache, inherits = FALSE)) {
    return(isTRUE(get(key, envir = pack$cache, inherits = FALSE)))
  }
  valid <- identical(
    pack$manifest$cell_order_fingerprint,
    viewerPackCellOrderFingerprint(pack$cells)
  )
  assign(key, valid, envir = pack$cache)
  valid
}

viewerPackReadAsset <- function(pack, path) {
  if (is.null(pack) || !is.list(pack) || !is.environment(pack$cache)) {
    return(NULL)
  }
  if (!viewerPackValidateCellOrder(pack)) {
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

viewerPackHlaSegments <- function(pack, chain) {
  if (
    !is.character(chain) || length(chain) != 1L || !chain %in% c("TRA", "TRB")
  ) {
    return(NULL)
  }
  viewerPackReadAsset(pack, file.path("hla_tcr", paste0(chain, ".qs2")))
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
