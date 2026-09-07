builder_eval_public_sources <- function() {
  tenx_license <- "CC BY 4.0"
  list(
    pbmc_expression = list(
      id = "pbmc_expression",
      label = "10x PBMC 3k gene expression",
      provider = "10x Genomics",
      accession = "pbmc3k; Cell Ranger 1.1.0",
      source_url = paste0(
        "https://cf.10xgenomics.com/samples/cell-exp/1.1.0/",
        "pbmc3k/pbmc3k_web_summary.html"
      ),
      license = tenx_license,
      organism = "Homo sapiens",
      tissue = "peripheral blood",
      technology = "Chromium Single Cell 3-prime gene expression",
      acquisition = "download_archive",
      preparation = paste(
        "Create a Seurat object from the filtered gene-barcode matrix,",
        "retain stable feature identifiers, take the lexicographically first",
        "requested cells, normalize RNA, and add declared technical Builder",
        "grouping/projection controls."
      ),
      files = list(
        matrix_archive = list(
          relative_path = "pbmc3k_filtered_gene_bc_matrices.tar.gz",
          url = paste0(
            "https://cf.10xgenomics.com/samples/cell-exp/1.1.0/",
            "pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz"
          ),
          sha256 = paste0(
            "847d6ebd9a1ec9a768f2be7e40ca42cb",
            "fe75ebeb6d76a4c24167041699dc28b5"
          )
        )
      )
    ),
    visium_brain = list(
      id = "visium_brain",
      label = "10x Visium mouse brain sagittal anterior section 1",
      provider = "10x Genomics; SeuratData",
      accession = "V1_Mouse_Brain_Sagittal_Anterior; stxBrain 0.1.2",
      source_url = paste0(
        "https://www.10xgenomics.com/datasets/",
        "mouse-brain-serial-section-1-sagittal-anterior-1-standard-1-1-0"
      ),
      license = tenx_license,
      organism = "Mus musculus",
      tissue = "brain, sagittal anterior",
      technology = "10x Visium Spatial Gene Expression",
      acquisition = "r_package",
      preparation = paste(
        "Load stxBrain anterior1 through SeuratData, update the object,",
        "take the lexicographically first requested spots, and add declared",
        "technical Builder grouping/projection controls."
      ),
      package = "stxBrain.SeuratData",
      package_version = "0.1.2",
      dataset = "stxBrain",
      dataset_type = "anterior1",
      files = list()
    ),
    pbmc_vdj = list(
      id = "pbmc_vdj",
      label = "10x PBMC 5-prime matched gene expression, TCR, and BCR",
      provider = "10x Genomics",
      accession = "vdj_v1_hs_pbmc3; Cell Ranger 3.1.0",
      source_url = paste0(
        "https://www.10xgenomics.com/datasets/",
        "pbm-cs-of-a-healthy-donor-next-gem-v-1-1-1-1-standard-3-1-0"
      ),
      license = tenx_license,
      organism = "Homo sapiens",
      tissue = "peripheral blood",
      technology = "Chromium Next GEM 5-prime gene expression and V(D)J",
      acquisition = "download_archive_and_contigs",
      preparation = paste(
        "Create a Seurat object from the matched filtered feature matrix,",
        "retain stable feature identifiers and requested cells with productive",
        "TCR/BCR contigs, collapse one deterministic receptor record per",
        "barcode, normalize RNA, and add declared technical Builder",
        "grouping/projection controls."
      ),
      files = list(
        matrix_archive = list(
          relative_path = "vdj_v1_hs_pbmc3_filtered_feature_bc_matrix.tar.gz",
          url = paste0(
            "https://cf.10xgenomics.com/samples/cell-vdj/3.1.0/",
            "vdj_v1_hs_pbmc3/",
            "vdj_v1_hs_pbmc3_filtered_feature_bc_matrix.tar.gz"
          ),
          sha256 = paste0(
            "73a84efb6d02c47c6005e9f91fcc8f36",
            "293b2aa08ed3c1529601e5516ee71884"
          )
        ),
        t_contigs = list(
          relative_path = "vdj_v1_hs_pbmc3_t_filtered_contigs.csv",
          url = paste0(
            "https://cf.10xgenomics.com/samples/cell-vdj/3.1.0/",
            "vdj_v1_hs_pbmc3/",
            "vdj_v1_hs_pbmc3_t_filtered_contig_annotations.csv"
          ),
          sha256 = paste0(
            "5cb5a061a7da7e9d5e31b7f5dbd9c7f",
            "56629fc946f52dd4d681bd798cbffd806"
          )
        ),
        b_contigs = list(
          relative_path = "vdj_v1_hs_pbmc3_b_filtered_contigs.csv",
          url = paste0(
            "https://cf.10xgenomics.com/samples/cell-vdj/3.1.0/",
            "vdj_v1_hs_pbmc3/",
            "vdj_v1_hs_pbmc3_b_filtered_contig_annotations.csv"
          ),
          sha256 = paste0(
            "f17d8477fd8df49992f020a3c133a748e",
            "0582c267bb3ec1d8954e302e9a4d1d9"
          )
        )
      )
    )
  )
}

.builder_eval_public_scalar <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

builder_eval_source_complete <- function(source) {
  common <- c(
    "id",
    "label",
    "provider",
    "accession",
    "source_url",
    "license",
    "organism",
    "tissue",
    "technology",
    "acquisition",
    "preparation"
  )
  if (
    !is.list(source) ||
      !all(common %in% names(source)) ||
      !all(vapply(source[common], .builder_eval_public_scalar, logical(1))) ||
      !grepl("^https://", source$source_url) ||
      !is.list(source$files)
  ) {
    return(FALSE)
  }
  if (identical(source$acquisition, "r_package")) {
    package_fields <- c("package", "package_version", "dataset", "dataset_type")
    return(
      all(package_fields %in% names(source)) &&
        all(vapply(
          source[package_fields],
          .builder_eval_public_scalar,
          logical(1)
        ))
    )
  }
  length(source$files) > 0L &&
    all(vapply(
      source$files,
      function(file) {
        is.list(file) &&
          .builder_eval_public_scalar(file$relative_path) &&
          identical(basename(file$relative_path), file$relative_path) &&
          .builder_eval_public_scalar(file$url) &&
          grepl("^https://", file$url) &&
          .builder_eval_public_scalar(file$sha256) &&
          grepl("^[0-9a-f]{64}$", file$sha256)
      },
      logical(1)
    ))
}

.builder_eval_public_within <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  startsWith(path, paste0(root, "/"))
}

builder_eval_public_cache_paths <- function(
  cache,
  sources = builder_eval_public_sources()
) {
  if (!.builder_eval_public_scalar(cache)) {
    stop("A caller-owned public-data cache directory is required.")
  }
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  cache <- normalizePath(cache, winslash = "/", mustWork = TRUE)
  paths <- lapply(sources, function(source) {
    directory <- file.path(cache, "downloads", source$id)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    targets <- vapply(
      source$files,
      function(file) file.path(directory, file$relative_path),
      character(1)
    )
    if (
      length(targets) &&
        !all(vapply(
          targets,
          .builder_eval_public_within,
          logical(1),
          root = cache
        ))
    ) {
      stop("A public-data cache target escaped the caller cache.")
    }
    targets
  })
  names(paths) <- names(sources)
  paths
}

builder_eval_public_downloads <- function(source_id, cache) {
  sources <- builder_eval_public_sources()
  source <- sources[[source_id]]
  if (is.null(source)) {
    stop("Unknown public Builder source: ", source_id, ".")
  }
  targets <- builder_eval_public_cache_paths(cache, sources)[[source_id]]
  for (name in names(targets)) {
    target <- targets[[name]]
    if (
      file.exists(target) && !dir.exists(target) && file.info(target)$size > 0
    ) {
      if (
        !identical(
          builder_eval_sha256_file(target),
          source$files[[name]]$sha256
        )
      ) {
        stop("A cached public source file has an unexpected SHA-256 digest.")
      }
      next
    }
    partial <- paste0(target, ".part")
    status <- tryCatch(
      utils::download.file(
        source$files[[name]]$url,
        partial,
        mode = "wb",
        quiet = TRUE
      ),
      error = function(error) error
    )
    if (
      inherits(status, "condition") ||
        !identical(status, 0L) ||
        !file.exists(partial) ||
        dir.exists(partial) ||
        file.info(partial)$size <= 0
    ) {
      stop("Public source download failed for `", source_id, "/", name, "`.")
    }
    if (
      !identical(
        builder_eval_sha256_file(partial),
        source$files[[name]]$sha256
      )
    ) {
      stop("A downloaded public source file has an unexpected SHA-256 digest.")
    }
    if (!file.rename(partial, target)) {
      stop("A completed public source download could not be cached.")
    }
  }
  targets
}

builder_eval_sha256_file <- function(path) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("The openssl package is required for public source provenance.")
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop("A public source file is unavailable for hashing.")
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  digest <- as.character(openssl::sha256(connection))
  attributes(digest) <- NULL
  digest[[1L]]
}

builder_eval_public_object_manifest <- function(source, object, files) {
  if (!builder_eval_source_complete(source)) {
    stop("A complete public source record is required.")
  }
  if (!inherits(object, "Seurat") || ncol(object) < 1L || nrow(object) < 1L) {
    stop("A non-empty prepared Seurat object is required.")
  }
  if (
    !is.character(files) || is.null(names(files)) || any(!file.exists(files))
  ) {
    stop("Named public source files are required for provenance.")
  }
  object_file <- tempfile("builder-public-object-", fileext = ".rds")
  on.exit(unlink(object_file, force = TRUE), add = TRUE)
  saveRDS(object, object_file, version = 3L)
  file_records <- lapply(names(files), function(name) {
    specification <- source$files[[name]] %||% list()
    list(
      role = name,
      filename = basename(files[[name]]),
      url = specification$url %||% source$source_url,
      bytes = as.double(file.info(files[[name]])$size),
      sha256 = builder_eval_sha256_file(files[[name]])
    )
  })
  list(
    schema_version = 1L,
    source_id = source$id,
    label = source$label,
    provider = source$provider,
    accession = source$accession,
    source_url = source$source_url,
    license = source$license,
    organism = source$organism,
    tissue = source$tissue,
    technology = source$technology,
    acquisition = source$acquisition,
    preparation = source$preparation,
    cells = as.integer(ncol(object)),
    features = as.integer(nrow(object)),
    object_bytes = as.double(file.info(object_file)$size),
    object_sha256 = builder_eval_sha256_file(object_file),
    files = file_records
  )
}

.builder_eval_public_extract_matrix <- function(archive, directory) {
  members <- utils::untar(archive, list = TRUE)
  unsafe <- startsWith(members, "/") |
    grepl("(^|/)[.][.]($|/)", members)
  if (!length(members) || any(unsafe)) {
    stop("A public matrix archive has unsafe members.")
  }
  matrix_member <- grep("(^|/)matrix[.]mtx([.]gz)?$", members, value = TRUE)
  if (length(matrix_member) != 1L) {
    stop("A public matrix archive has no unique matrix directory.")
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  marker <- file.path(directory, ".complete")
  matrix_path <- file.path(directory, matrix_member)
  archive_hash <- builder_eval_sha256_file(archive)
  complete <- file.exists(marker) &&
    file.exists(matrix_path) &&
    identical(readLines(marker, warn = FALSE), archive_hash)
  if (!complete) {
    utils::untar(archive, exdir = directory)
    if (!file.exists(matrix_path)) {
      stop("The public matrix archive could not be extracted.")
    }
    writeLines(archive_hash, marker, useBytes = TRUE)
  }
  dirname(matrix_path)
}

.builder_eval_public_counts <- function(directory) {
  counts <- Seurat::Read10X(
    directory,
    gene.column = 1L,
    unique.features = TRUE
  )
  if (is.list(counts)) {
    index <- match("Gene Expression", names(counts))
    if (is.na(index)) {
      index <- 1L
    }
    counts <- counts[[index]]
  }
  counts
}

.builder_eval_public_cells <- function(cells, max_cells) {
  cells <- sort(unique(as.character(cells)), method = "radix")
  if (!is.numeric(max_cells) || length(max_cells) != 1L || max_cells < 1L) {
    stop("The public source cell limit is invalid.")
  }
  utils::head(cells, as.integer(max_cells))
}

.builder_eval_public_stabilize <- function(object) {
  fixed_time <- as.POSIXct("2026-08-24 00:00:00", tz = "UTC")
  if (length(object@commands)) {
    for (name in names(object@commands)) {
      object@commands[[name]]@time.stamp <- fixed_time
    }
  }
  object
}

.builder_eval_public_builder_controls <- function(object) {
  if (!inherits(object, "Seurat") || ncol(object) < 2L) {
    stop("Public Builder evaluation requires at least two cells or spots.")
  }
  cells <- colnames(object)
  object$builder_evaluation_partition <- factor(
    rep(c("A", "B"), length.out = length(cells)),
    levels = c("A", "B")
  )
  index <- seq_along(cells)
  embeddings <- cbind(
    BEVAL_1 = seq(-1, 1, length.out = length(cells)),
    BEVAL_2 = ((index - 1L) %% 11L) / 5 - 1
  )
  rownames(embeddings) <- cells
  object[["builder_evaluation"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embeddings,
    key = "BEVAL_",
    assay = SeuratObject::DefaultAssay(object)
  )
  object@misc$builder_evaluation_public_controls <- list(
    schema_version = 1L,
    purpose = paste(
      "Technical grouping and coordinates for Builder transport evaluation;",
      "not a biological clustering or embedding."
    ),
    grouping = "builder_evaluation_partition",
    reduction = "builder_evaluation"
  )
  object
}

.builder_eval_prepare_pbmc_expression <- function(cache, source, max_cells) {
  files <- builder_eval_public_downloads(source$id, cache)
  extracted <- file.path(cache, "prepared", source$id)
  directory <- .builder_eval_public_extract_matrix(
    files[["matrix_archive"]],
    extracted
  )
  counts <- .builder_eval_public_counts(directory)
  cells <- .builder_eval_public_cells(colnames(counts), max_cells)
  object <- SeuratObject::CreateSeuratObject(
    counts = counts[, cells, drop = FALSE],
    project = "10x_pbmc3k"
  )
  object$sample <- factor("pbmc3k")
  object$public_source <- factor(source$id)
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object <- .builder_eval_public_builder_controls(object)
  list(object = .builder_eval_public_stabilize(object), files = files)
}

.builder_eval_prepare_visium <- function(cache, source, max_cells) {
  if (
    !requireNamespace("SeuratData", quietly = TRUE) ||
      !requireNamespace(source$package, quietly = TRUE)
  ) {
    stop(
      "The publication profile requires `",
      source$package,
      "` version ",
      source$package_version,
      "."
    )
  }
  installed_version <- as.character(utils::packageVersion(source$package))
  if (!identical(installed_version, source$package_version)) {
    stop(
      "The publication profile requires `",
      source$package,
      "` version ",
      source$package_version,
      "; installed version is ",
      installed_version,
      "."
    )
  }
  object <- SeuratData::LoadData(source$dataset, type = source$dataset_type)
  object <- SeuratObject::UpdateSeuratObject(object)
  cells <- .builder_eval_public_cells(colnames(object), max_cells)
  object <- subset(object, cells = cells)
  object$public_source <- factor(source$id)
  object <- .builder_eval_public_builder_controls(object)
  package_root <- system.file(package = source$package)
  data_files <- file.path(
    package_root,
    "data",
    c("Rdata.rds", "Rdata.rdx", "Rdata.rdb")
  )
  data_files <- data_files[file.exists(data_files)]
  names(data_files) <- paste0("package_data_", seq_along(data_files))
  list(object = .builder_eval_public_stabilize(object), files = data_files)
}

.builder_eval_public_repertoire <- function(files, cells, max_cells) {
  read_contigs <- function(path, receptor) {
    table <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    required <- c("barcode", "chain", "v_gene", "j_gene", "cdr3", "cdr3_nt")
    if (!all(required %in% names(table))) {
      stop("A public V(D)J contig table is missing required columns.")
    }
    if ("productive" %in% names(table)) {
      productive <- tolower(as.character(table$productive)) %in%
        c("true", "t", "1")
      table <- table[productive, , drop = FALSE]
    }
    table$receptor <- receptor
    table
  }
  contigs <- rbind(
    read_contigs(files[["t_contigs"]], "TCR"),
    read_contigs(files[["b_contigs"]], "BCR")
  )
  complete <- !is.na(contigs$barcode) &
    nzchar(contigs$barcode) &
    !is.na(contigs$v_gene) &
    nzchar(contigs$v_gene) &
    !is.na(contigs$j_gene) &
    nzchar(contigs$j_gene) &
    !is.na(contigs$cdr3) &
    nzchar(contigs$cdr3) &
    !is.na(contigs$cdr3_nt) &
    nzchar(contigs$cdr3_nt) &
    contigs$barcode %in% cells
  contigs <- contigs[complete, , drop = FALSE]
  contigs <- contigs[
    order(
      contigs$barcode,
      contigs$receptor,
      contigs$chain,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  contigs <- contigs[!duplicated(contigs$barcode), , drop = FALSE]
  selected <- .builder_eval_public_cells(contigs$barcode, max_cells)
  contigs <- contigs[match(selected, contigs$barcode), , drop = FALSE]
  strict_column <- intersect(
    c("raw_clonotype_id", "clonotype_id"),
    names(contigs)
  )
  strict <- if (length(strict_column)) {
    as.character(contigs[[strict_column[[1L]]]])
  } else {
    paste0(contigs$receptor, "_", seq_len(nrow(contigs)))
  }
  strict[is.na(strict) | !nzchar(strict)] <- paste0(
    contigs$receptor[is.na(strict) | !nzchar(strict)],
    "_unassigned"
  )
  list(
    cells = selected,
    table = data.frame(
      barcode = contigs$barcode,
      CTgene = paste(contigs$v_gene, contigs$j_gene, sep = "."),
      CTnt = contigs$cdr3_nt,
      CTaa = contigs$cdr3,
      CTstrict = strict,
      receptor = contigs$receptor,
      stringsAsFactors = FALSE
    )
  )
}

.builder_eval_prepare_pbmc_vdj <- function(cache, source, max_cells) {
  files <- builder_eval_public_downloads(source$id, cache)
  extracted <- file.path(cache, "prepared", source$id)
  directory <- .builder_eval_public_extract_matrix(
    files[["matrix_archive"]],
    extracted
  )
  counts <- .builder_eval_public_counts(directory)
  repertoire <- .builder_eval_public_repertoire(
    files,
    colnames(counts),
    max_cells
  )
  if (!length(repertoire$cells)) {
    stop("The public V(D)J and gene-expression sources have no shared cells.")
  }
  object <- SeuratObject::CreateSeuratObject(
    counts = counts[, repertoire$cells, drop = FALSE],
    project = "10x_vdj_v1_hs_pbmc3"
  )
  object$sample <- factor("vdj_v1_hs_pbmc3")
  receptor <- repertoire$table$receptor
  names(receptor) <- repertoire$table$barcode
  object$receptor_type <- factor(receptor[colnames(object)])
  object$public_source <- factor(source$id)
  object@misc$immune_repertoire <- list(
    vdj_v1_hs_pbmc3 = repertoire$table[, c(
      "barcode",
      "CTgene",
      "CTnt",
      "CTaa",
      "CTstrict"
    )]
  )
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object <- .builder_eval_public_builder_controls(object)
  list(object = .builder_eval_public_stabilize(object), files = files)
}

builder_eval_prepare_public_source <- function(
  source_id,
  cache,
  max_cells = 500L
) {
  sources <- builder_eval_public_sources()
  source <- sources[[source_id]]
  if (is.null(source)) {
    stop("Unknown public Builder source: ", source_id, ".")
  }
  prepared <- switch(
    source_id,
    pbmc_expression = .builder_eval_prepare_pbmc_expression(
      cache,
      source,
      max_cells
    ),
    visium_brain = .builder_eval_prepare_visium(cache, source, max_cells),
    pbmc_vdj = .builder_eval_prepare_pbmc_vdj(cache, source, max_cells),
    stop("The public Builder source has no preparation rule.")
  )
  list(
    object = prepared$object,
    manifest = builder_eval_public_object_manifest(
      source,
      prepared$object,
      prepared$files
    )
  )
}
