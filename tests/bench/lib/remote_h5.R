# Read cell blocks out of a remote HDF5 matrix without downloading the file.
#
# Both source layouts we care about store the matrix as a compressed-sparse
# structure whose major axis is cells:
#
#   10x .h5   /<group>/{data,indices,indptr,shape,gene_names,barcodes}
#             CSC over cells -> indices are gene indices  (indptr = ncells + 1)
#   .h5ad     /X/{data,indices,indptr} + /var/_index + /obs/_index
#             CSR over cells -> indices are gene indices  (indptr = ncells + 1)
#
# They are therefore the same thing on the wire, and a contiguous run of cells
# is a contiguous hyperslab of `data`/`indices`. We read `indptr` (a few MB),
# derive the non-zero range for the cells we want, and range-read only that.
# rhdf5's ROS3 driver turns each read into HTTP range requests, so the transfer
# is proportional to the tier size rather than to the 4-34 GB file.
#
# Two ROS3 landmines, both measured on these files rather than assumed:
#
#   * `indptr` is int64 and its values run past 2^31 here, so it must be read
#     with bit64conversion = "double". The default int32 coercion turns the
#     pointers into NA and the failure surfaces much later as a nonsense nnz.
#   * Reading a variable-length string dataset at an offset other than the
#     first ABORTS the R process (SIGTRAP in the driver) once the data sits
#     beyond ~4 GB into the file. Cell names are therefore synthesised from the
#     row index; gene names are still read, because /var is small and sits at a
#     low offset.

suppressPackageStartupMessages({
  library(rhdf5)
  library(Matrix)
})

# ---- structure ---------------------------------------------------------------

# Dataset paths differ between the two layouts; everything downstream is shared.
bench_paths <- function(spec) {
  if (identical(spec$kind, "tenx")) {
    g <- paste0("/", spec$group)
    list(
      data = paste0(g, "/data"),
      indices = paste0(g, "/indices"),
      indptr = paste0(g, "/indptr"),
      genes = paste0(g, "/gene_names"),
      cells = paste0(g, "/barcodes")
    )
  } else {
    list(
      data = "/X/data",
      indices = "/X/indices",
      indptr = "/X/indptr",
      genes = NA_character_, # resolved from /var attributes, see bench_labels()
      cells = NA_character_
    )
  }
}

# AnnData does not fix the names of its index datasets: the obs index on the
# CELLxGENE files is `barcodekey`, not `_index`, and which one it is only shows
# up in the group's `_index` attribute. Gene symbols additionally live in a
# categorical (`categories` + `codes` sub-datasets) rather than a string array.
bench_labels <- function(spec, fid) {
  if (identical(spec$kind, "tenx")) {
    p <- bench_paths(spec)
    return(list(genes = p$genes, cells = p$cells))
  }
  obs_idx <- rhdf5::h5readAttributes(fid, "/obs")[["_index"]]
  var_idx <- rhdf5::h5readAttributes(fid, "/var")[["_index"]]
  # Prefer human-readable symbols; fall back to whatever the index holds.
  genes <- if (bench_link_exists(fid, "/var/feature_name")) {
    "/var/feature_name"
  } else {
    paste0("/var/", var_idx)
  }
  list(genes = genes, cells = paste0("/obs/", obs_idx))
}

# H5Lexists() raises rather than returning FALSE when an intermediate component
# of the path is a dataset instead of a group, which is exactly the case when
# probing "<plain dataset>/codes".
bench_link_exists <- function(fid, path) {
  isTRUE(tryCatch(rhdf5::H5Lexists(fid, path), error = function(e) FALSE))
}

# Read a string vector that may be either a plain dataset or an AnnData
# categorical. `start`/`count` slice the observation axis when given.
bench_read_strings <- function(fid, path, start = NULL, count = NULL) {
  if (bench_link_exists(fid, paste0(path, "/codes"))) {
    codes <- if (is.null(start)) {
      rhdf5::h5read(fid, paste0(path, "/codes"))
    } else {
      rhdf5::h5read(fid, paste0(path, "/codes"), start = start, count = count)
    }
    cats <- as.character(rhdf5::h5read(fid, paste0(path, "/categories")))
    return(cats[as.integer(codes) + 1L])
  }
  out <- if (is.null(start)) {
    rhdf5::h5read(fid, path)
  } else {
    rhdf5::h5read(fid, path, start = start, count = count)
  }
  as.character(out)
}

# One persistent handle per source. H5Fopen() takes the ROS3 driver through a
# file-access property list rather than the `s3 = TRUE` shortcut that h5ls() and
# h5read() expose, and reusing the handle avoids re-negotiating the connection
# on every chunk read.
bench_open <- function(spec) {
  # A run-scoped copy in the scratch directory, when present, is preferred:
  # ROS3 issues small serial range requests and measures ~8x slower than a
  # plain download of the same bytes, so the sweep caches each source once and
  # deletes it on exit. ROS3 is still the path for probing, where the point is
  # to read metadata without transferring anything.
  if (!is.null(spec$local_path) && file.exists(spec$local_path)) {
    return(rhdf5::H5Fopen(spec$local_path, flags = "H5F_ACC_RDONLY"))
  }
  fapl <- rhdf5::H5Pcreate("H5P_FILE_ACCESS")
  rhdf5::H5Pset_fapl_ros3(fapl)
  on.exit(rhdf5::H5Pclose(fapl), add = TRUE)
  rhdf5::H5Fopen(spec$url, flags = "H5F_ACC_RDONLY", fapl = fapl)
}

#' Cache a source in `dir` for the duration of a run. Returns the local path.
#'
#' The caller is responsible for deleting `dir`; run_sweep.sh does that from a
#' trap so an interrupted run does not leave tens of GB behind.
bench_fetch_source <- function(spec, dir, verbose = TRUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dir, basename(sub("\\?.*$", "", spec$url)))
  if (file.exists(dest)) {
    return(dest)
  }
  part <- paste0(dest, ".part")
  if (verbose) {
    message(sprintf("  fetching %s -> %s", spec$url, dest))
  }
  # --continue-at resumes a partial transfer if a previous run was interrupted
  # while the same scratch directory was still in place.
  status <- system2(
    "curl",
    c(
      "-fL",
      "--retry",
      "3",
      "--retry-delay",
      "5",
      "--continue-at",
      "-",
      "-o",
      shQuote(part),
      shQuote(spec$url)
    )
  )
  if (!identical(status, 0L) || !file.exists(part)) {
    stop("download failed for ", spec$url, call. = FALSE)
  }
  file.rename(part, dest)
  dest
}

bench_dim1 <- function(fid, name) {
  did <- rhdf5::H5Dopen(fid, name)
  on.exit(rhdf5::H5Dclose(did), add = TRUE)
  sid <- rhdf5::H5Dget_space(did)
  on.exit(rhdf5::H5Sclose(sid), add = TRUE)
  as.numeric(rhdf5::H5Sget_simple_extent_dims(sid)$size)
}

# Gene count comes from the minor axis: the 10x layout records it in `shape`,
# AnnData in the `shape` attribute of /X (cells x genes for a csr_matrix).
bench_n_genes <- function(spec, fid) {
  if (identical(spec$kind, "tenx")) {
    return(as.numeric(rhdf5::h5read(fid, paste0("/", spec$group, "/shape"))[1]))
  }
  shape <- rhdf5::h5readAttributes(fid, "/X")[["shape"]]
  as.numeric(shape[2])
}

#' Probe a remote source: dimensions and non-zero count, no bulk transfer.
bench_probe <- function(spec) {
  p <- bench_paths(spec)
  fid <- bench_open(spec)
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)

  nnz <- bench_dim1(fid, p$data)
  n_cells <- bench_dim1(fid, p$indptr) - 1
  n_genes <- bench_n_genes(spec, fid)

  list(
    label = spec$label,
    kind = spec$kind,
    n_cells = n_cells,
    n_genes = n_genes,
    nnz = nnz,
    nnz_per_cell = nnz / n_cells,
    # A dgCMatrix stores i (int, 4 B) and x (double, 8 B) per non-zero.
    dgc_gb_full = nnz * 12 / 2^30,
    # 32-bit index limit of the CsparseMatrix representation.
    dgc_representable = nnz <= .Machine$integer.max
  )
}

# ---- cell-block planning -----------------------------------------------------

# Cells in both files are grouped by sample/donor, so one contiguous run of
# 50k cells is a handful of donors rather than a cross-section of the study.
# We therefore take `n_chunks` evenly spaced contiguous runs. Chunks stay
# contiguous because that is what keeps the read a hyperslab instead of
# millions of single-element requests.
bench_plan_chunks <- function(n_cells_total, n_take, n_chunks = 4L) {
  n_take <- min(n_take, n_cells_total)
  n_chunks <- max(1L, min(as.integer(n_chunks), floor(n_take / 1000)))
  sizes <- rep(n_take %/% n_chunks, n_chunks)
  sizes[seq_len(n_take %% n_chunks)] <- sizes[seq_len(n_take %% n_chunks)] + 1
  # Spread the chunk starts across the whole file, leaving room for each run.
  span <- n_cells_total - n_take
  offsets <- if (n_chunks == 1L) {
    0
  } else {
    round(seq(0, span, length.out = n_chunks))
  }
  starts <- offsets + c(0, cumsum(sizes)[-n_chunks])
  data.frame(start = starts + 1, size = sizes) # 1-based inclusive starts
}

# ---- the reader --------------------------------------------------------------

#' Read a cell subset of a remote sparse matrix as a genes x cells dgCMatrix.
#'
#' Peak memory is (final matrix + one chunk): `i` and `x` are preallocated once
#' and each chunk is read straight into its slice, so no cbind() doubling.
bench_read_subset <- function(spec, n_take, n_chunks = 4L, verbose = TRUE) {
  p <- bench_paths(spec)
  fid <- bench_open(spec)
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)

  lab <- bench_labels(spec, fid)
  n_cells_total <- bench_dim1(fid, p$indptr) - 1
  n_genes <- bench_n_genes(spec, fid)
  plan <- bench_plan_chunks(n_cells_total, n_take, n_chunks)

  # Pass 1: indptr per chunk -> nnz budget. Reading size+1 pointers gives the
  # non-zero range of the chunk: (ptr[1], ptr[size + 1]].
  # indptr is int64 and its values exceed 2^31 on these files (nnz > 2.1e9), so
  # it must be pulled as double; the default int32 coercion silently NAs out.
  # `indices` stays native because gene indices are small and reading a
  # billion-element chunk as double would double the peak footprint.
  ptrs <- lapply(seq_len(nrow(plan)), function(k) {
    as.numeric(rhdf5::h5read(
      fid,
      p$indptr,
      start = plan$start[k],
      count = plan$size[k] + 1,
      bit64conversion = "double"
    ))
  })
  chunk_nnz <- vapply(ptrs, function(v) v[length(v)] - v[1], numeric(1))
  total_nnz <- sum(chunk_nnz)

  if (total_nnz > .Machine$integer.max) {
    stop(
      sprintf(
        paste0(
          "tier needs nnz = %.3e, which exceeds the 32-bit dgCMatrix index ",
          "limit (%.3e). A dgCMatrix cannot represent this subset at all, ",
          "independent of available RAM."
        ),
        total_nnz,
        .Machine$integer.max
      ),
      call. = FALSE
    )
  }
  if (verbose) {
    message(sprintf(
      "  reading %s cells in %d chunk(s), nnz %.3e (~%.1f GB as dgCMatrix)",
      format(sum(plan$size), big.mark = ","),
      nrow(plan),
      total_nnz,
      total_nnz * 12 / 2^30
    ))
  }

  n_take <- sum(plan$size)
  i_all <- integer(total_nnz)
  x_all <- numeric(total_nnz)
  p_all <- numeric(n_take + 1)
  cells <- character(n_take)

  nz_at <- 0 # non-zeros written so far
  col_at <- 0 # columns written so far
  for (k in seq_len(nrow(plan))) {
    pk <- ptrs[[k]]
    nz <- chunk_nnz[k]
    if (nz > 0) {
      # HDF5 indptr is 0-based; hyperslab start is 1-based. dgCMatrix@i is
      # itself 0-based, so the gene indices are stored verbatim.
      idx <- rhdf5::h5read(fid, p$indices, start = pk[1] + 1, count = nz)
      i_all[nz_at + seq_len(nz)] <- as.integer(idx)
      rm(idx)
      val <- rhdf5::h5read(fid, p$data, start = pk[1] + 1, count = nz)
      storage.mode(val) <- "double"
      x_all[nz_at + seq_len(nz)] <- val
      rm(val)
    }
    # Rebase this chunk's pointers onto the running total.
    p_all[col_at + 1 + seq_len(plan$size[k])] <- (pk[-1] - pk[1]) + nz_at
    # Cell names are SYNTHESISED from the global row index rather than read.
    # Reading the variable-length string barcode dataset at any offset other
    # than the first aborts the R process outright (SIGTRAP inside the ROS3
    # driver) on files larger than 4 GB; a full read of 1.5 M variable-length
    # strings takes minutes. Cell identity is irrelevant to a backend
    # benchmark - dimensions and values are what is under test - and the index
    # still points back at the exact row of the source file.
    cells[col_at + seq_len(plan$size[k])] <- sprintf(
      "cell_%d",
      plan$start[k] + seq_len(plan$size[k]) - 1
    )
    nz_at <- nz_at + nz
    col_at <- col_at + plan$size[k]
    if (verbose) {
      message(sprintf(
        "    chunk %d/%d done (%.1f%% of tier nnz)",
        k,
        nrow(plan),
        100 * nz_at / max(1, total_nnz)
      ))
    }
  }
  rm(ptrs)

  genes <- make.unique(bench_read_strings(fid, lab$genes))

  # dgCMatrix requires row indices ascending within each column. The 10x
  # 1.3M-neuron file stores them DESCENDING (measured, not assumed), so the
  # assembled vectors have to be reordered. Reversing i/x wholesale makes every
  # column ascending in one pass; the side effect is that the columns come out
  # back-to-front, which we absorb by reversing the column pointers and the cell
  # names rather than by re-subsetting a multi-GB matrix.
  ord <- bench_column_order(i_all, p_all)
  if (identical(ord, "descending")) {
    i_all <- rev(i_all)
    x_all <- rev(x_all)
    p_all <- cumsum(c(0, rev(diff(p_all))))
    cells <- rev(cells)
  } else if (identical(ord, "mixed")) {
    warning(
      "column-major order is neither ascending nor descending; ",
      "falling back to a per-column sort (slow)",
      call. = FALSE
    )
    for (j in seq_len(n_take)) {
      lo <- p_all[j] + 1
      hi <- p_all[j + 1]
      if (hi > lo) {
        sl <- lo:hi
        o <- order(i_all[sl])
        i_all[sl] <- i_all[sl][o]
        x_all[sl] <- x_all[sl][o]
      }
    }
  }

  m <- new(
    "dgCMatrix",
    i = i_all,
    p = as.integer(p_all),
    x = x_all,
    Dim = c(as.integer(n_genes), as.integer(n_take)),
    Dimnames = list(genes, make.unique(cells))
  )
  m
}

# Classify within-column ordering from a sample of non-empty columns, so the
# expensive correction only runs when the file actually needs it.
bench_column_order <- function(i_all, p_all, n_sample = 64L) {
  counts <- diff(p_all)
  cand <- which(counts > 1)
  if (!length(cand)) {
    return("ascending")
  }
  cand <- cand[unique(round(seq(
    1,
    length(cand),
    length.out = min(n_sample, length(cand))
  )))]
  asc <- desc <- TRUE
  for (j in cand) {
    seg <- i_all[(p_all[j] + 1):p_all[j + 1]]
    if (is.unsorted(seg)) {
      asc <- FALSE
    }
    if (is.unsorted(rev(seg))) {
      desc <- FALSE
    }
    if (!asc && !desc) break
  }
  if (asc) {
    "ascending"
  } else if (desc) {
    "descending"
  } else {
    "mixed"
  }
}
