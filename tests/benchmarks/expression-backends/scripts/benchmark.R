# Shared configuration and helpers for the backend benchmark.
# Source this file from tests or benchmark_cli.R.

# ---- sources.R ----
# Remote data source registry for the expression-backend benchmark.
#
# Every source is a public HDF5 file. Metadata inspection uses the rhdf5 ROS3
# virtual file driver and transfers only the ranges it needs. A benchmark run
# downloads the complete file into its marked scratch directory so all timed
# source reads are local and network throughput is excluded from measurements.
#
# Numbers in the comments were measured with `benchmark_cli.R inspect`.

BENCH_SOURCES <- list(
  # 10x Genomics 1.3 M mouse brain cells (E18), the canonical large-scale
  # scRNA-seq reference. Raw integer counts, CSC over cells.
  # 1,306,127 cells x 27,998 genes, nnz 2,624,828,308, 3.93 GB remote.
  mouse_brain_e18 = list(
    label = "10x mouse brain E18",
    accession = "GSE93421; SRP096558",
    landing_page = paste0(
      "https://www.10xgenomics.com/datasets/",
      "1-3-million-brain-cells-from-e-18-mice-2-standard-1-3-0"
    ),
    kind = "tenx",
    url = paste0(
      "https://cf.10xgenomics.com/samples/cell-exp/1.3.0/1M_neurons/",
      "1M_neurons_filtered_gene_bc_matrices_h5.h5"
    ),
    group = "mm10",
    organism = "mm10",
    slot = "counts",
    expected_bytes = 4216018749,
    expected_sha256 = paste0(
      "255a36ee92de25cb3568faa2c27d31fe",
      "6d0db30f285c5c977be8d6245de14044"
    ),
    full_cells = 1306127,
    panel_c1_cells = 400e3,
    # Shared tiers; bench_scale_schedule also appends this source's full_cells.
    tiers = c(1e3, 10e3, 50e3, 100e3, 300e3, 500e3, 1e6),
    comparison_tiers = c(1e3, 10e3, 50e3, 100e3, 300e3, 500e3, 1e6)
  ),

  # CELLxGENE Discover: population-scale cross-disorder atlas of the human
  # prefrontal cortex (HBCC cohort). Normalised float values, CSR over cells.
  # 1,486,324 cells x 34,176 genes, nnz 6,111,732,728, 14.15 GB remote.
  human_pfc_hbcc = list(
    label = "human PFC cross-disorder (HBCC)",
    dataset_id = "d27fb144-f105-46c2-b36f-f51421f74e4e",
    collection_id = "84ce6837-548d-4a1f-919f-0bc0d9a3952f",
    doi = "10.1038/s41597-025-04687-5",
    landing_page = paste0(
      "https://cellxgene.cziscience.com/collections/",
      "84ce6837-548d-4a1f-919f-0bc0d9a3952f"
    ),
    kind = "h5ad",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad"
    ),
    organism = "hg38",
    slot = "data",
    expected_bytes = 14150526668,
    expected_sha256 = paste0(
      "aeca0480ab8941a7e4cf6b0ff6dc8c",
      "5f9d0de376466d65ca8198dc873f1cb16f"
    ),
    full_cells = 1486324,
    panel_c1_cells = 300e3,
    # Shared tiers; the exact full endpoint is measured within Scale itself.
    tiers = c(1e3, 10e3, 50e3, 100e3, 300e3, 500e3, 1e6),
    comparison_tiers = c(1e3, 10e3, 50e3, 100e3, 300e3, 500e3, 1e6)
  ),

  # Same collection, MSSM cohort: 4,140,453 cells, 33.6 GB remote. Opt-in via
  # BENCH_SOURCES_EXTRA=human_pfc_mssm because probing it alone streams more
  # than the other two sources combined.
  human_pfc_mssm = list(
    label = "human PFC cross-disorder (MSSM)",
    kind = "h5ad",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad"
    ),
    organism = "hg38",
    slot = "data",
    expected_bytes = 36077725286,
    tiers = c(50e3, 200e3),
    comparison_tiers = 50e3,
    opt_in = TRUE
  )
)

# Which sources run by default.
bench_active_sources <- function() {
  extra <- strsplit(Sys.getenv("BENCH_SOURCES_EXTRA", ""), "[,[:space:]]+")[[1]]
  extra <- extra[nzchar(extra)]
  keep <- vapply(BENCH_SOURCES, function(s) !isTRUE(s$opt_in), logical(1))
  active <- names(BENCH_SOURCES)[keep | names(BENCH_SOURCES) %in% extra]
  only <- strsplit(Sys.getenv("BENCH_SOURCES_ONLY", ""), "[,[:space:]]+")[[1]]
  only <- only[nzchar(only)]
  if (!length(only)) {
    return(active)
  }
  unknown <- setdiff(only, names(BENCH_SOURCES))
  if (length(unknown)) {
    stop("unknown BENCH_SOURCES_ONLY value: ", paste(unknown, collapse = ", "))
  }
  intersect(active, only)
}

# ---- protocol.R ----
# Pure protocol helpers for the real-data expression-backend benchmark.

bench_profile <- function(name = Sys.getenv("BENCH_PROFILE", "quick")) {
  profiles <- list(
    quick = list(
      name = "quick",
      export_repeats = 1L,
      access_repeats = 1L,
      query_genes = 12L,
      hot_iterations = 1L,
      include_scale_tiers = FALSE,
      comparison_tier_mode = "smallest",
      article_eligible = FALSE
    ),
    standard = list(
      name = "standard",
      export_repeats = 3L,
      access_repeats = 1L,
      query_genes = 12L,
      hot_iterations = 2L,
      include_scale_tiers = TRUE,
      comparison_tier_mode = "all",
      article_eligible = FALSE
    ),
    scale = list(
      name = "scale",
      export_repeats = 5L,
      access_repeats = 2L,
      query_genes = 12L,
      hot_iterations = 3L,
      include_scale_tiers = TRUE,
      comparison_tier_mode = "all",
      article_eligible = TRUE
    ),
    panel_c1 = list(
      name = "panel_c1",
      export_repeats = 3L,
      access_repeats = 2L,
      query_genes = 12L,
      hot_iterations = 3L,
      include_scale_tiers = FALSE,
      comparison_tier_mode = "all",
      article_eligible = TRUE
    ),
    panel_c2 = list(
      name = "panel_c2",
      export_repeats = 5L,
      access_repeats = 2L,
      query_genes = 12L,
      hot_iterations = 3L,
      include_scale_tiers = FALSE,
      comparison_tier_mode = "all",
      article_eligible = TRUE
    ),
    stress = list(
      name = "stress",
      export_repeats = 1L,
      access_repeats = 1L,
      query_genes = 12L,
      hot_iterations = 1L,
      include_scale_tiers = TRUE,
      comparison_tier_mode = "all",
      article_eligible = FALSE
    )
  )
  profile <- profiles[[name]]
  if (is.null(profile)) {
    stop(
      "unknown benchmark profile: ",
      name,
      paste0(
        "; expected quick, standard, scale, ",
        "panel_c1, panel_c2, or stress"
      ),
      call. = FALSE
    )
  }
  profile
}

# Out-of-core backends built by build-full. "h5" is the package exporter's
# compressed layout; "h5_uncompressed" is the same 10x H5 at gzip level 0, a
# storage/read trade-off reference. The package opens both as its "h5" backend.
BENCH_FULL_BACKENDS <- c("bpcells", "h5", "h5_uncompressed")

bench_storage_backend <- function(backend) {
  if (identical(backend, "h5_uncompressed")) "h5" else backend
}

# One plotting identity per backend, shared by figure_full.R and figure_scale.R.
# Okabe-Ito colours; uncompressed H5 is the lighter blue with an open square.
bench_backend_style <- function() {
  list(
    order = c("h5", "h5_uncompressed", "bpcells", "embedded"),
    palette = c(h5 = "#0072B2", h5_uncompressed = "#56B4E9",
                bpcells = "#E69F00", embedded = "#D55E00"),
    shapes = c(h5 = 15, h5_uncompressed = 0, bpcells = 16, embedded = 17),
    linetypes = c(h5 = "solid", h5_uncompressed = "22",
                  bpcells = "solid", embedded = "solid"),
    labels = c(h5 = "H5 (compressed)", h5_uncompressed = "H5 (uncompressed)",
               bpcells = "BPCells", embedded = "Embedded")
  )
}

bench_h5_gzip_level <- function(backend) {
  if (identical(backend, "h5_uncompressed")) {
    0L
  } else {
    HDF5Array::getHDF5DumpCompressionLevel()
  }
}

# Keep only scheduled cells matching BENCH_SCHEDULE_FILTER, e.g.
# "source=human_pfc_hbcc;n_cells=500000;backend=embedded". Each key takes
# comma-separated values. Repeats, order positions and access repeats are kept,
# so a supplement measures the selected cells exactly as the full study would.
bench_filter_schedule <- function(
  schedule,
  filter = Sys.getenv("BENCH_SCHEDULE_FILTER")
) {
  if (!nzchar(filter)) {
    return(schedule)
  }
  keep <- rep(TRUE, nrow(schedule))
  for (term in strsplit(filter, ";", fixed = TRUE)[[1L]]) {
    parts <- strsplit(trimws(term), "=", fixed = TRUE)[[1L]]
    if (length(parts) != 2L || !nzchar(parts[2L])) {
      stop("malformed BENCH_SCHEDULE_FILTER term: ", term, call. = FALSE)
    }
    key <- parts[1L]
    if (!key %in% c("source", "n_cells", "backend")) {
      stop("unknown BENCH_SCHEDULE_FILTER key: ", key, call. = FALSE)
    }
    values <- trimws(strsplit(parts[2L], ",", fixed = TRUE)[[1L]])
    keep <- keep & if (identical(key, "n_cells")) {
      schedule$n_cells %in% as.numeric(values)
    } else {
      schedule[[key]] %in% values
    }
  }
  if (!any(keep)) {
    stop("BENCH_SCHEDULE_FILTER matches no scheduled cell: ", filter, call. = FALSE)
  }
  out <- schedule[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

bench_scale_schedule <- function(specs) {
  sources <- intersect(c("mouse_brain_e18", "human_pfc_hbcc"), names(specs))
  if (length(sources) != 2L) {
    stop(
      "scale profile requires the mouse and human sources",
      call. = FALSE
    )
  }
  embedded_specs <- external_specs <- specs
  for (source in sources) {
    full <- specs[[source]]$full_cells
    if (length(full) != 1L || !is.finite(full) || full < 1) {
      stop("scale requires a valid full_cells for ", source, call. = FALSE)
    }
    # Shared tiers up to 500k plus each source's exact full size. A 1M tier sits
    # next to the 1.31M/1.49M endpoints on the log axis, adds little, and was the
    # costliest tier, so Scale does not measure it. The endpoints also serve the
    # Full figure, so a separate Full study is not needed.
    shared <- specs[[source]]$comparison_tiers
    tiers <- sort(unique(c(shared[shared <= 500e3], full)))
    embedded_specs[[source]]$tiers <-
      embedded_specs[[source]]$comparison_tiers <-
        tiers[tiers <= 500e3]
    external_specs[[source]]$tiers <-
      external_specs[[source]]$comparison_tiers <-
        tiers[tiers > 500e3]
  }
  rbind(
    bench_schedule(
      embedded_specs,
      "scale",
      sources = sources,
      backends = c("embedded", BENCH_FULL_BACKENDS)
    ),
    bench_schedule(
      external_specs,
      "scale",
      sources = sources,
      backends = BENCH_FULL_BACKENDS
    )
  )
}

bench_panel_c_schedule <- function(specs, part = c("c1", "c2")) {
  if (length(part) != 1L || !part %in% c("c1", "c2")) {
    stop("Panel C part must be c1 or c2", call. = FALSE)
  }
  sources <- intersect(c("mouse_brain_e18", "human_pfc_hbcc"), names(specs))
  if (length(sources) != 2L) {
    stop(
      "Panel C requires the mouse and human benchmark sources",
      call. = FALSE
    )
  }
  field <- if (part == "c1") "panel_c1_cells" else "full_cells"
  missing <- sources[vapply(
    specs[sources],
    function(x) is.null(x[[field]]),
    logical(1)
  )]
  if (length(missing)) {
    stop("Panel C source metadata is missing ", field, call. = FALSE)
  }
  panel_specs <- specs[sources]
  for (source in sources) {
    tier <- panel_specs[[source]][[field]]
    panel_specs[[source]]$tiers <- tier
    panel_specs[[source]]$comparison_tiers <- tier
  }
  bench_schedule(
    panel_specs,
    paste0("panel_c", substring(part, 2L)),
    sources = sources,
    backends = if (part == "c1") {
      c("embedded", "bpcells", "h5")
    } else {
      BENCH_FULL_BACKENDS
    }
  )
}

bench_schedule <- function(
  specs,
  profile = "quick",
  sources = names(specs),
  backends = c("embedded", "bpcells", "h5")
) {
  if (is.character(profile)) {
    profile <- bench_profile(profile)
  }
  unknown <- setdiff(sources, names(specs))
  if (length(unknown)) {
    stop("unknown benchmark source: ", paste(unknown, collapse = ", "))
  }

  rows <- list()
  at <- 0L
  for (source in sources) {
    spec <- specs[[source]]
    comparison_tiers <- spec$comparison_tiers
    if (is.null(comparison_tiers)) {
      comparison_tiers <- min(spec$tiers)
    }
    if (identical(profile$comparison_tier_mode, "smallest")) {
      comparison_tiers <- min(comparison_tiers)
    }
    tiers <- if (isTRUE(profile$include_scale_tiers)) {
      spec$tiers
    } else {
      comparison_tiers
    }
    for (n_cells in tiers) {
      comparison <- n_cells %in% comparison_tiers
      n_repeats <- if (comparison) profile$export_repeats else 1L
      for (export_repeat in seq_len(n_repeats)) {
        shift <- (export_repeat - 1L) %% length(backends)
        order <- backends[
          ((seq_along(backends) + shift - 1L) %% length(backends)) + 1L
        ]
        for (position in seq_along(order)) {
          at <- at + 1L
          rows[[at]] <- data.frame(
            profile = profile$name,
            source = source,
            n_cells = as.numeric(n_cells),
            comparison = comparison,
            export_repeat = as.integer(export_repeat),
            order_position = as.integer(position),
            backend = order[position],
            access_repeats = if (comparison) profile$access_repeats else 1L,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, rows)
}

bench_stratified_gene_panel <- function(
  genes,
  nnz,
  n_genes = 50L
) {
  if (length(genes) != length(nnz)) {
    stop("genes and nnz must have the same length", call. = FALSE)
  }
  active <- data.frame(
    gene = as.character(genes),
    nnz = as.numeric(nnz),
    stringsAsFactors = FALSE
  )
  active <- active[
    is.finite(active$nnz) & active$nnz > 0 & nzchar(active$gene),
  ]
  if (!nrow(active)) {
    stop("no expressed genes are available for the query panel", call. = FALSE)
  }
  active <- active[order(active$nnz, active$gene), , drop = FALSE]
  n_take <- min(as.integer(n_genes), nrow(active))
  selected <- unique(round(seq(1, nrow(active), length.out = n_take)))
  panel <- active[selected, , drop = FALSE]

  first <- which.min(abs(panel$nnz - stats::median(active$nnz)))
  panel <- panel[c(first, setdiff(seq_len(nrow(panel)), first)), , drop = FALSE]
  panel$role <- c("first", rep("hot", nrow(panel) - 1L))
  rownames(panel) <- NULL
  panel
}

bench_serialized_fingerprint <- function(payload) {
  raw <- serialize(
    payload,
    connection = NULL,
    ascii = FALSE,
    xdr = TRUE,
    version = 3
  )
  path <- tempfile("cerebro-bench-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  writeBin(raw, con)
  close(con)
  as.character(unname(tools::md5sum(path)))
}

bench_numeric_fingerprint <- function(x) {
  dimensions <- dim(x)
  values <- if (is.null(dimensions)) {
    as.numeric(x)
  } else {
    as.numeric(as.matrix(x))
  }
  bench_serialized_fingerprint(list(
    dim = dimensions,
    values = values
  ))
}

bench_subset_cells <- function(n_cells, limit = 100000L) {
  if (
    length(n_cells) != 1L ||
      !is.finite(n_cells) ||
      n_cells < 1L ||
      n_cells != as.integer(n_cells)
  ) {
    stop("n_cells must be one positive integer", call. = FALSE)
  }
  count <- min(as.integer(n_cells), as.integer(limit))
  rev(unique(as.integer(round(seq.int(1, n_cells, length.out = count)))))
}

.bench_result_key <- function(x) {
  paste(
    x$source,
    sprintf("%.0f", as.numeric(x$n_cells)),
    x$backend,
    as.integer(x$export_repeat),
    sep = "|"
  )
}

bench_validate_timing_evidence <- function(exports, access, timings) {
  check_seconds <- function(values) {
    if (any(!is.finite(values) | values < 0)) {
      stop("invalid elapsed time in benchmark evidence", call. = FALSE)
    }
  }
  for (rows in list(exports, access)) {
    for (column in grep("_secs$", names(rows), value = TRUE)) {
      values <- rows[[column]]
      # Shell serialization is not a separate operation in the small-tier path.
      optional <- column %in% c("shell_secs", "serialize_secs")
      check_seconds(values[
        (!is.na(rows$status) & rows$status == "OK" & !optional) |
          !is.na(values)
      ])
    }
  }
  if (
    !"timing_contract" %in% names(access) ||
      anyNA(access$timing_contract) ||
      any(access$timing_contract != "monotonic-v1")
  ) {
    stop("new publication requires monotonic-v1 timing evidence", call. = FALSE)
  }
  ids <- c(
    "run_id",
    "source",
    "n_cells",
    "backend",
    "export_repeat",
    "access_repeat"
  )
  required <- c(
    ids,
    "sequence",
    "phase",
    "clock",
    "resolution_secs",
    "monotonic_start",
    "monotonic_end",
    "wall_start",
    "wall_end",
    "seconds"
  )
  if (
    !all(required %in% names(timings)) ||
      !nrow(timings) ||
      !all(ids %in% names(access)) ||
      anyNA(timings[required])
  ) {
    stop("incomplete raw timing evidence", call. = FALSE)
  }
  check_seconds(timings$seconds)
  if (
    any(timings$clock != "CLOCK_MONOTONIC") ||
      any(
        !is.finite(as.matrix(timings[c(
          "monotonic_start",
          "monotonic_end",
          "wall_start",
          "wall_end",
          "resolution_secs"
        )]))
      ) ||
      any(timings$resolution_secs <= 0) ||
      any(timings$monotonic_end < timings$monotonic_start) ||
      any(
        abs(
          timings$seconds - (timings$monotonic_end - timings$monotonic_start)
        ) >
          pmax(1e-6, 2 * timings$resolution_secs)
      )
  ) {
    stop("invalid clock endpoints in raw timing evidence", call. = FALSE)
  }
  key <- function(rows) do.call(paste, c(rows[ids], sep = "\r"))
  access_keys <- key(access)
  timing_keys <- key(timings)
  if (anyDuplicated(access_keys) || !setequal(access_keys, timing_keys)) {
    stop("incomplete raw timing coverage", call. = FALSE)
  }
  single <- c(
    startup = "startup_secs",
    first = "first_query_secs",
    block = "block_secs",
    subset_row = "subset_row_secs",
    subset_block = "subset_block_secs"
  )
  for (i in seq_len(nrow(access))) {
    rows <- timings[timing_keys == access_keys[i], , drop = FALSE]
    if (
      !is.finite(access$n_hot[i]) ||
        access$n_hot[i] < 1 ||
        nrow(rows) != access$n_hot[i] + length(single) ||
        !identical(as.integer(rows$sequence), seq_len(nrow(rows))) ||
        any(!rows$phase %in% c(names(single), "hot")) ||
        any(
          vapply(names(single), function(x) sum(rows$phase == x), integer(1)) !=
            1L
        ) ||
        sum(rows$phase == "hot") != access$n_hot[i]
    ) {
      stop("incomplete raw timing sequence", call. = FALSE)
    }
    observed <- c(
      stats::setNames(
        vapply(
          names(single),
          function(x) rows$seconds[rows$phase == x],
          numeric(1)
        ),
        single
      ),
      hot_p50_secs = unname(stats::quantile(
        rows$seconds[rows$phase == "hot"],
        0.5
      )),
      hot_p95_secs = unname(stats::quantile(
        rows$seconds[rows$phase == "hot"],
        0.95
      ))
    )
    if (
      !all(names(observed) %in% names(access)) ||
        !isTRUE(all.equal(
          unname(observed),
          as.numeric(access[i, names(observed)]),
          tolerance = 1e-8
        ))
    ) {
      stop("timing summary differs from raw observations", call. = FALSE)
    }
  }
  invisible(TRUE)
}

bench_validate_results <- function(
  schedule,
  exports,
  access,
  crashes = data.frame(),
  profile = bench_profile("quick")
) {
  expected <- .bench_result_key(schedule)
  export_keys <- if (nrow(exports)) .bench_result_key(exports) else character()
  crash_keys <- if (
    nrow(crashes) &&
      all(
        c("source", "n_cells", "backend", "export_repeat") %in% names(crashes)
      )
  ) {
    export_crashes <- if ("stage" %in% names(crashes)) {
      crashes[crashes$stage == "export", , drop = FALSE]
    } else {
      crashes
    }
    .bench_result_key(export_crashes)
  } else {
    character()
  }
  outcome <- c(export_keys, crash_keys)
  counts <- table(factor(outcome, levels = expected))
  if (any(counts == 0L)) {
    stop(
      "missing export outcome for scheduled cell: ",
      names(counts)[counts == 0L][1],
      call. = FALSE
    )
  }
  if (any(counts > 1L) || anyDuplicated(expected)) {
    stop("duplicate export outcome for scheduled cell", call. = FALSE)
  }

  if (any(!outcome %in% expected)) {
    stop("unscheduled export outcome", call. = FALSE)
  }
  if (nrow(exports) && anyNA(exports$status)) {
    stop("export outcome has no status", call. = FALSE)
  }
  if (identical(profile$name, "scale")) {
    failed_embedded <- exports$backend == "embedded" & exports$status != "OK"
    if (
      any(
        failed_embedded & !grepl("^SKIPPED\\(resource\\):", exports$status)
      ) ||
        length(crash_keys)
    ) {
      stop("build failure is not a documented resource limit", call. = FALSE)
    }
  }
  successful <- exports[
    exports$status == "OK",
    ,
    drop = FALSE
  ]
  required <- schedule$comparison &
    !(identical(profile$name, "scale") &
      schedule$backend == "embedded")
  comparison_keys <- expected[required]
  if (!all(comparison_keys %in% .bench_result_key(successful))) {
    stop(
      "comparison tier did not complete every required backend",
      call. = FALSE
    )
  }

  if (nrow(access)) {
    required_access <- c(
      "status",
      "access_contract",
      "correctness",
      "row_fingerprint",
      "reference_row_fingerprint",
      "block_fingerprint",
      "reference_block_fingerprint"
    )
    subset_access <- c(
      "subset_row_fingerprint",
      "reference_subset_row_fingerprint",
      "subset_block_fingerprint",
      "reference_subset_block_fingerprint"
    )
    if (!all(required_access %in% names(access))) {
      stop("access results are missing required columns", call. = FALSE)
    }
    has_subset <- subset_access %in% names(access)
    if (any(has_subset) && !all(has_subset)) {
      stop("access subset results are incomplete", call. = FALSE)
    }
    if (identical(profile$name, "panel_c2") && !all(has_subset)) {
      stop("full-source access results require subset metrics", call. = FALSE)
    }
    checked_access <- access
    if (
      any(is.na(checked_access$status)) ||
        any(checked_access$status != "OK")
    ) {
      stop("access process failed", call. = FALSE)
    }
    if (
      anyNA(access$access_contract) ||
        any(access$access_contract != "materialized-v2")
    ) {
      stop(
        "access results require the materialized-v2 timing contract",
        call. = FALSE
      )
    }
    mismatch <-
      checked_access$correctness != "OK" |
      checked_access$row_fingerprint !=
        checked_access$reference_row_fingerprint |
      checked_access$block_fingerprint !=
        checked_access$reference_block_fingerprint
    if (all(has_subset)) {
      mismatch <- mismatch |
        checked_access$subset_row_fingerprint !=
          checked_access$reference_subset_row_fingerprint |
        checked_access$subset_block_fingerprint !=
          checked_access$reference_subset_block_fingerprint
    }
    if (any(is.na(mismatch) | mismatch)) {
      stop("backend correctness fingerprint mismatch", call. = FALSE)
    }
  }

  successful_keys <- .bench_result_key(successful)
  access_keys <- if (nrow(access)) .bench_result_key(access) else character()
  if (any(!access_keys %in% successful_keys)) {
    stop("access result has no successful scheduled build", call. = FALSE)
  }
  for (i in seq_len(nrow(successful))) {
    key <- successful_keys[i]
    planned <- schedule[expected == key, , drop = FALSE]
    wanted <- planned$access_repeats[1]
    observed <- sum(access_keys == key)
    if (observed != wanted) {
      stop(
        sprintf(
          "missing access measurement for %s: expected %d, observed %d",
          key,
          wanted,
          observed
        ),
        call. = FALSE
      )
    }
    repeats <- access$access_repeat[access_keys == key]
    if (
      anyNA(repeats) ||
        anyDuplicated(repeats) ||
        !setequal(repeats, seq_len(wanted))
    ) {
      stop("invalid or duplicate access repeat", call. = FALSE)
    }
  }
  TRUE
}

bench_require_article_profile <- function(profile) {
  if (is.character(profile)) {
    profile <- bench_profile(profile)
  }
  if (!isTRUE(profile$article_eligible)) {
    stop(
      "the user-facing article requires an evidence-grade benchmark profile",
      call. = FALSE
    )
  }
  TRUE
}

# ---- access_metrics.R ----
# Runtime access measurement helpers.

bench_build_query_plan <- function(m, n_genes = 50L) {
  genes <- rownames(m)
  if (is.null(genes) || anyNA(genes) || any(!nzchar(genes))) {
    stop("the benchmark matrix must have non-empty gene names", call. = FALSE)
  }
  counts <- tabulate(m@i + 1L, nbins = nrow(m))
  panel <- bench_stratified_gene_panel(genes, counts, n_genes = n_genes)

  first_values <- as.numeric(m[panel$gene[1], ])
  block_values <- as.matrix(m[panel$gene, , drop = FALSE])
  subset_cells <- bench_subset_cells(ncol(m))
  plan <- list(
    schema_version = 2L,
    n_cells = ncol(m),
    n_genes = nrow(m),
    nnz = sum(counts),
    panel = panel,
    subset_cells = subset_cells,
    subset_cells_fingerprint = bench_numeric_fingerprint(subset_cells),
    reference_row_fingerprint = bench_numeric_fingerprint(first_values),
    reference_block_fingerprint = bench_numeric_fingerprint(block_values),
    reference_subset_row_fingerprint = bench_numeric_fingerprint(
      first_values[subset_cells]
    ),
    reference_subset_block_fingerprint = bench_numeric_fingerprint(
      block_values[, subset_cells, drop = FALSE]
    )
  )
  plan$query_plan_fingerprint <- bench_serialized_fingerprint(plan)
  plan
}

# Compile once per process, before measuring; no extra R dependency is needed.
bench_clock <- local({
  native <- NULL
  function() {
    if (is.null(native)) {
      src <- tempfile("bench-clock-", fileext = ".c")
      dll <- sub("\\.c$", .Platform$dynlib.ext, src)
      writeLines(
        c(
          "#define _POSIX_C_SOURCE 200809L",
          "#include <R.h>",
          "#include <Rinternals.h>",
          "#include <time.h>",
          "SEXP bench_clock_read(void) {",
          "  struct timespec mono, wall, resolution;",
          "  if (clock_gettime(CLOCK_MONOTONIC, &mono) ||",
          "      clock_gettime(CLOCK_REALTIME, &wall) ||",
          "      clock_getres(CLOCK_MONOTONIC, &resolution))",
          "    error(\"monotonic clock unavailable\");",
          "  SEXP out = PROTECT(allocVector(REALSXP, 3));",
          "  REAL(out)[0] = mono.tv_sec + mono.tv_nsec * 1e-9;",
          "  REAL(out)[1] = wall.tv_sec + wall.tv_nsec * 1e-9;",
          "  REAL(out)[2] = resolution.tv_sec + resolution.tv_nsec * 1e-9;",
          "  UNPROTECT(1); return out;",
          "}"
        ),
        src
      )
      output <- system2(
        file.path(R.home("bin"), "R"),
        c("CMD", "SHLIB", "-o", shQuote(dll), shQuote(src)),
        stdout = TRUE,
        stderr = TRUE
      )
      if (!file.exists(dll) || !is.null(attr(output, "status"))) {
        stop(
          "cannot build monotonic clock: ",
          paste(output, collapse = "\n"),
          call. = FALSE
        )
      }
      loaded <- dyn.load(dll)
      native <<- getNativeSymbolInfo("bench_clock_read", loaded)
    }
    stats::setNames(.Call(native), c("monotonic", "wall", "resolution"))
  }
})

bench_default_timer <- function(fn, clock = bench_clock) {
  started <- clock()
  value <- fn()
  ended <- clock()
  seconds <- unname(ended[["monotonic"]] - started[["monotonic"]])
  list(
    seconds = seconds,
    value = value,
    timing = data.frame(
      clock = "CLOCK_MONOTONIC",
      resolution_secs = unname(started[["resolution"]]),
      monotonic_start = unname(started[["monotonic"]]),
      monotonic_end = unname(ended[["monotonic"]]),
      wall_start = unname(started[["wall"]]),
      wall_end = unname(ended[["wall"]]),
      seconds = seconds
    )
  )
}

bench_timed <- function(
  fn,
  timer = bench_default_timer,
  record = function(row) NULL,
  phase = "elapsed",
  query = ""
) {
  measured <- timer(fn)
  timing <- measured$timing
  if (is.null(timing)) {
    timing <- data.frame(seconds = measured$seconds)
  }
  timing$phase <- phase
  timing$query <- paste(query, collapse = ";")
  record(timing)
  if (
    length(measured$seconds) != 1L ||
      !is.finite(measured$seconds) ||
      measured$seconds < 0
  ) {
    stop("invalid elapsed time for ", phase, call. = FALSE)
  }
  measured
}

# Full R garbage collection outside the timers. R escalates to a full
# collection at a fixed point in a process's allocation history, and its cost
# is set by the whole heap (attached packages, the cell-metadata shell), not by
# the read in progress. Without this, a 0.3-0.7 s collection lands in whichever
# read reaches that point first; which read that is shifts with tier, source and
# even the size of these scripts. A read that allocates enough to need a
# collection of its own still pays for it inside its timer.
bench_settle_heap <- function() {
  invisible(gc(verbose = FALSE, full = TRUE))
}

bench_measure_backend <- function(
  obj,
  plan,
  hot_iterations = 5L,
  timer = bench_default_timer,
  record = function(row) NULL,
  settle = bench_settle_heap
) {
  if (!identical(plan$schema_version, 2L)) {
    stop("unsupported query-plan schema", call. = FALSE)
  }
  panel <- plan$panel
  first_gene <- panel$gene[panel$role == "first"]
  if (length(first_gene) != 1L) {
    stop("query plan must contain exactly one first gene", call. = FALSE)
  }

  measure <- function(fn, phase, query, settle_first = TRUE) {
    if (settle_first) {
      settle()
    }
    bench_timed(
      fn,
      timer = timer,
      record = record,
      phase = phase,
      query = query
    )
  }
  first <- measure(
    function() as.numeric(obj$getExpressionRow(first_gene)),
    "first",
    first_gene
  )
  row_fingerprint <- bench_numeric_fingerprint(first$value)
  if (!identical(row_fingerprint, plan$reference_row_fingerprint)) {
    stop("first-query fingerprint mismatch", call. = FALSE)
  }

  hot_genes <- panel$gene[panel$role == "hot"]
  if (!length(hot_genes)) {
    hot_genes <- first_gene
  }
  invisible(lapply(hot_genes, function(gene) {
    as.numeric(obj$getExpressionRow(gene))
  }))
  hot_order <- rep(hot_genes, times = max(1L, as.integer(hot_iterations)))
  # One collection before the warmed loop; per-read collections would cost more
  # than the short reads they protect.
  settle()
  hot_secs <- vapply(
    hot_order,
    function(gene) {
      measure(
        function() as.numeric(obj$getExpressionRow(gene)),
        "hot",
        gene,
        settle_first = FALSE
      )$seconds
    },
    numeric(1)
  )

  block <- measure(
    function() as.matrix(obj$getExpressionBlock(panel$gene)),
    "block",
    panel$gene
  )
  block_fingerprint <- bench_numeric_fingerprint(block$value)
  if (!identical(block_fingerprint, plan$reference_block_fingerprint)) {
    stop("block fingerprint mismatch", call. = FALSE)
  }

  subset_row <- measure(
    function() {
      as.numeric(obj$getExpressionRow(first_gene, cells = plan$subset_cells))
    },
    "subset_row",
    first_gene
  )
  subset_row_fingerprint <- bench_numeric_fingerprint(subset_row$value)
  if (
    !identical(
      subset_row_fingerprint,
      plan$reference_subset_row_fingerprint
    )
  ) {
    stop("subset-row fingerprint mismatch", call. = FALSE)
  }

  subset_block <- measure(
    function() {
      as.matrix(obj$getExpressionBlock(panel$gene, cells = plan$subset_cells))
    },
    "subset_block",
    panel$gene
  )
  subset_block_fingerprint <- bench_numeric_fingerprint(subset_block$value)
  if (
    !identical(
      subset_block_fingerprint,
      plan$reference_subset_block_fingerprint
    )
  ) {
    stop("subset-block fingerprint mismatch", call. = FALSE)
  }

  list(
    access_contract = "materialized-v2",
    first_query_secs = first$seconds,
    hot_p50_secs = stats::quantile(hot_secs, 0.5, names = FALSE),
    hot_p95_secs = stats::quantile(hot_secs, 0.95, names = FALSE),
    block_secs = block$seconds,
    subset_row_secs = subset_row$seconds,
    subset_block_secs = subset_block$seconds,
    subset_n_cells = length(plan$subset_cells),
    n_hot = length(hot_secs),
    correctness = "OK",
    row_fingerprint = row_fingerprint,
    reference_row_fingerprint = plan$reference_row_fingerprint,
    block_fingerprint = block_fingerprint,
    reference_block_fingerprint = plan$reference_block_fingerprint,
    subset_row_fingerprint = subset_row_fingerprint,
    reference_subset_row_fingerprint = plan$reference_subset_row_fingerprint,
    subset_block_fingerprint = subset_block_fingerprint,
    reference_subset_block_fingerprint = plan$reference_subset_block_fingerprint,
    query_plan_fingerprint = plan$query_plan_fingerprint
  )
}

# ---- bench_utils.R ----
# Shared measurement helpers.

# Resident set size of the current process, in MB. R's own gc() figures only
# account for R's heap, which misses the BPCells/HDF5 buffers that are the whole
# point of comparing backends, so ask the OS instead.
bench_rss_mb <- function() {
  out <- suppressWarnings(system2(
    "ps",
    c("-o", "rss=", "-p", Sys.getpid()),
    stdout = TRUE,
    stderr = FALSE
  ))
  val <- suppressWarnings(as.numeric(trimws(paste(out, collapse = ""))))
  if (is.na(val)) NA_real_ else val / 1024
}

# Peak resident set size on Linux. VmHWM includes native allocations that R's
# gc() accounting misses. Other platforms return NA rather than a false proxy.
bench_peak_rss_mb <- function(status_path = "/proc/self/status") {
  if (!file.exists(status_path)) {
    return(NA_real_)
  }
  line <- grep(
    "^VmHWM:",
    readLines(status_path, warn = FALSE),
    value = TRUE
  )
  value_kb <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", line[1L])))
  if (!length(value_kb) || !is.finite(value_kb)) NA_real_ else value_kb / 1024
}

bench_path_mb <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NA_real_)
  }
  if (dir.exists(path)) {
    files <- list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE
    )
    files <- files[!dir.exists(files)]
    return(sum(file.size(files), na.rm = TRUE) / 2^20)
  }
  file.size(path) / 2^20
}

# Elapsed time includes waiting and native work, without wall-clock corrections.
bench_time <- function(expr) {
  bench_timed(function() force(expr))$seconds
}

# Append one row to a CSV, writing the header only when creating the file. Rows
# land on disk as soon as they are measured so an aborted sweep still leaves
# every tier it did finish.
bench_append_row <- function(path, row) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    row,
    path,
    sep = ",",
    row.names = FALSE,
    qmethod = "double",
    col.names = !file.exists(path),
    append = file.exists(path)
  )
  invisible(row)
}

bench_msg <- function(...) {
  message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), sprintf(...)))
}

# ---- full_source.R ----
# Lazy source helpers for scale and full-source benchmarks.

bench_validate_full_matrix <- function(matrix, spec = list()) {
  if (!inherits(matrix, "IterableMatrix")) {
    stop("full source must be a BPCells IterableMatrix", call. = FALSE)
  }
  if (
    length(dim(matrix)) != 2L ||
      any(!is.finite(dim(matrix))) ||
      any(dim(matrix) < 1L)
  ) {
    stop("full source must be a non-empty matrix", call. = FALSE)
  }
  labels <- dimnames(matrix)
  if (
    length(labels) != 2L ||
      any(vapply(labels, is.null, logical(1))) ||
      any(vapply(labels, function(x) anyNA(x) || any(!nzchar(x)), logical(1)))
  ) {
    stop("full source must have non-empty gene and cell names", call. = FALSE)
  }
  if (!is.null(spec$full_cells) && ncol(matrix) != spec$full_cells) {
    stop("full source cell count does not match source metadata", call. = FALSE)
  }
  matrix
}

bench_open_full_source <- function(spec, path) {
  if (length(spec$kind) != 1L || !spec$kind %in% c("tenx", "h5ad")) {
    stop("full source kind must be 10x or AnnData", call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop("full source file is missing: ", path, call. = FALSE)
  }
  matrix <- switch(
    spec$kind,
    tenx = BPCells::open_matrix_10x_hdf5(path),
    h5ad = BPCells::open_matrix_anndata_hdf5(path, group = "X")
  )
  # Seurat normalizes feature names on import. Use the same names in every
  # backend and in the frozen plan, without changing source values or order.
  rownames(matrix) <- bench_seurat_feature_names(rownames(matrix))
  bench_validate_full_matrix(matrix, spec)
}

bench_read_export_matrix <- function(spec, path, n_cells, profile) {
  if (identical(profile, "scale")) {
    # The frozen scale plan and disk backends use this exact prefix, not the
    # historical four dispersed chunks selected by bench_read_subset().
    return(methods::as(
      bench_open_source_tier(spec, path, n_cells),
      "dgCMatrix"
    ))
  }
  spec$local_path <- path
  bench_read_subset(spec, n_cells, n_chunks = 4L, verbose = TRUE)
}

bench_resource_limit <- function(error) {
  grepl(
    "cannot allocate vector|vector memory (limit|exhausted)|32-bit.*(index|limit)|dgCMatrix.*(2\\^31|integer.max)|more than 2\\^31-1 nonzero",
    conditionMessage(error),
    ignore.case = TRUE
  )
}

bench_open_source_tier <- function(spec, path, n_cells) {
  matrix <- bench_open_full_source(spec, path)
  if (
    length(n_cells) != 1L ||
      !is.finite(n_cells) ||
      n_cells < 1L ||
      n_cells != as.integer(n_cells) ||
      n_cells > ncol(matrix)
  ) {
    stop("source tier must be a positive available cell count", call. = FALSE)
  }
  if (n_cells < ncol(matrix)) {
    matrix <- matrix[, seq_len(as.integer(n_cells)), drop = FALSE]
  }
  bench_validate_full_matrix(matrix)
}

bench_write_full_backend <- function(matrix, backend, path) {
  bench_validate_full_matrix(matrix)
  if (backend == "bpcells") {
    if (dir.exists(path)) {
      unlink(path, recursive = TRUE)
    }
    written <- CerebroNexus:::.writeBpcellsGeneMajor(matrix, path)
  } else if (backend %in% c("h5", "h5_uncompressed")) {
    if (file.exists(path)) {
      unlink(path)
    }
    # The 10x layout needs a cell-major copy. Keep it and BPCells' sort spill
    # beside the output, inside the preflighted scratch, and drop it once the
    # H5 is written instead of leaving it in R's session tempdir.
    transposed <- tempfile("h5-transpose-", tmpdir = dirname(path))
    on.exit(unlink(transposed, recursive = TRUE), add = TRUE)
    disk_matrix <- BPCells::transpose_storage_order(
      DelayedArray::t(matrix),
      outdir = transposed,
      tmpdir = dirname(path)
    )
    # BPCells defaults to gzip_level = 0. "h5" uses the level the package's own
    # H5 exporter (HDF5Array::writeTENxMatrix) applies, so sizes and reads are
    # those of a shipped sidecar; "h5_uncompressed" keeps level 0 on purpose.
    BPCells::write_matrix_10x_hdf5(
      disk_matrix,
      path,
      gzip_level = bench_h5_gzip_level(backend),
      type = "auto"
    )
    local({
      handle <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDWR")
      on.exit(rhdf5::H5Fclose(handle), add = TRUE)
      rhdf5::H5Lmove(handle, "matrix", handle, "expression")
    })
    written <- DelayedArray::t(
      HDF5Array::TENxMatrix(path, group = "expression")
    )
  } else {
    stop("backend must be bpcells, h5 or h5_uncompressed", call. = FALSE)
  }
  if (
    !identical(dim(written), dim(matrix)) ||
      !identical(dimnames(written), dimnames(matrix))
  ) {
    stop("written backend dimensions or names changed", call. = FALSE)
  }
  invisible(path)
}

bench_build_lazy_query_plan <- function(matrix, n_genes = 12L) {
  bench_validate_full_matrix(matrix)
  stats <- BPCells::matrix_stats(matrix, row_stats = "nonzero")$row_stats
  counts <- as.numeric(stats["nonzero", ])
  panel <- bench_stratified_gene_panel(rownames(matrix), counts, n_genes)
  first_values <- as.numeric(as.matrix(matrix[panel$gene[1L], , drop = FALSE]))
  block_values <- as.matrix(matrix[panel$gene, , drop = FALSE])
  subset_cells <- bench_subset_cells(ncol(matrix))
  plan <- list(
    schema_version = 2L,
    n_cells = ncol(matrix),
    n_genes = nrow(matrix),
    nnz = sum(counts),
    panel = panel,
    subset_cells = subset_cells,
    subset_cells_fingerprint = bench_numeric_fingerprint(subset_cells),
    reference_row_fingerprint = bench_numeric_fingerprint(first_values),
    reference_block_fingerprint = bench_numeric_fingerprint(block_values),
    reference_subset_row_fingerprint = bench_numeric_fingerprint(
      first_values[subset_cells]
    ),
    reference_subset_block_fingerprint = bench_numeric_fingerprint(
      block_values[, subset_cells, drop = FALSE]
    )
  )
  plan$query_plan_fingerprint <- bench_serialized_fingerprint(plan)
  plan
}

bench_make_full_shell <- function(
  matrix,
  backend,
  location,
  source_name,
  organism,
  run_id
) {
  bench_validate_full_matrix(matrix)
  cells <- colnames(matrix)
  index <- seq_along(cells)
  metadata <- data.frame(
    cell_barcode = cells,
    nUMI = 1000 + index %% 100L,
    nGene = 500 + index %% 50L,
    sample = factor(sprintf("sample_%02d", (index - 1L) %% 8L + 1L)),
    cluster = factor(sprintf("cluster_%02d", (index - 1L) %% 20L + 1L)),
    row.names = cells
  )
  projection <- data.frame(
    x = sin(index / 1000),
    y = cos(index / 1000),
    row.names = cells
  )
  obj <- CerebroNexus::Cerebro$new()
  obj$setVersion(utils::packageVersion("CerebroNexus"))
  obj$addExperiment("experiment_name", paste0("Panel C2: ", source_name))
  obj$addExperiment("organism", organism)
  obj$addExperiment("date_of_export", Sys.Date())
  obj$addTechnicalInfo("benchmark_run_id", run_id)
  obj$addTechnicalInfo("benchmark_source", source_name)
  obj$setMetaData(metadata)
  obj$addGroup("sample", levels(metadata$sample))
  obj$addGroup("cluster", levels(metadata$cluster))
  obj$addProjection("benchmark", projection)
  if (identical(backend, "bpcells")) {
    obj$setExpression(matrix, backend = "external")
  }
  obj$setExpressionBackend(bench_storage_backend(backend), location)
  obj
}

# ---- make_seurat.R ----
# Wrap a counts matrix into the minimum Seurat object exportFromSeurat() needs.
#
# The grouping variables and the projection are synthetic on purpose: this
# benchmark measures how the three expression backends behave as the matrix
# grows, and a real clustering/UMAP would add tens of minutes per tier without
# changing a single number that is being measured. Everything that IS measured
# - the counts, the dimensions, the sparsity - comes from the real file.
#
# Seurat is not attached when this file is sourced. Every benchmark process
# sources it, and CerebroNexus does not need Seurat to read a CRB, so loading it
# here cost each process about 5 s and added about 350 MB to every backend's
# measured memory after load. Only embedded export builds a Seurat object; the
# namespaced calls below load SeuratObject (and exportFromSeurat() Seurat) then.

bench_make_seurat <- function(m, n_groups = 6L, seed = 42L) {
  set.seed(seed)
  n <- ncol(m)

  obj <- SeuratObject::CreateSeuratObject(
    counts = m,
    min.cells = 0,
    min.features = 0,
    assay = "RNA"
  )

  # Cerebro requires at least one grouping variable and uses them for every
  # per-group aggregation; two levels of granularity mirror sample + cluster.
  obj$sample <- factor(paste0(
    "sample_",
    sample.int(n_groups, n, replace = TRUE)
  ))
  obj$cluster <- factor(paste0(
    "cluster_",
    sample.int(n_groups * 2, n, replace = TRUE)
  ))
  obj$nUMI <- Matrix::colSums(m)
  obj$nGene <- Matrix::colSums(m > 0)

  # A dummy 2-D embedding stands in for UMAP. Cerebro only reads the
  # coordinates, so random ones exercise the same code paths.
  emb <- matrix(
    stats::rnorm(2 * n),
    ncol = 2,
    dimnames = list(colnames(m), c("UMAP_1", "UMAP_2"))
  )
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb,
    key = "UMAP_",
    assay = "RNA"
  )
  obj
}

# ---- remote_h5.R ----
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

bench_seurat_feature_names <- function(genes) {
  genes <- gsub("_", "-", genes, fixed = TRUE)
  genes <- gsub("|", "-", genes, fixed = TRUE)
  make.unique(genes)
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
#' The caller is responsible for deleting `dir`; `run.sh` does that from a
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

  genes <- bench_seurat_feature_names(bench_read_strings(fid, lab$genes))

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

# ---- reporting.R ----
# Aggregation and formatting helpers for benchmark reports.

bench_summarise_metrics <- function(x, group, metrics) {
  missing <- setdiff(c(group, metrics, "status"), names(x))
  if (length(missing)) {
    stop("missing summary columns: ", paste(missing, collapse = ", "))
  }
  ok <- x[x$status == "OK", , drop = FALSE]
  if (!nrow(ok)) {
    return(data.frame())
  }
  key <- interaction(ok[group], drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(ok)), key)
  out <- lapply(rows, function(index) {
    group_row <- ok[index[1], group, drop = FALSE]
    group_row$rows_n <- length(index)
    for (metric in metrics) {
      values <- ok[[metric]][index]
      values <- values[is.finite(values)]
      group_row[[paste0(metric, "_median")]] <- if (length(values)) {
        stats::median(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_min")]] <- if (length(values)) {
        min(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_max")]] <- if (length(values)) {
        max(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_n")]] <- length(values)
    }
    group_row
  })
  rownames(out) <- NULL
  do.call(rbind, out)
}

bench_format_interval <- function(median, minimum, maximum, n, digits = 2L) {
  if (
    !is.finite(median) || !is.finite(minimum) || !is.finite(maximum) || n < 1L
  ) {
    return("--")
  }
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f-%.", digits, "f], n=%d"),
    median,
    minimum,
    maximum,
    as.integer(n)
  )
}

bench_evidence_notice <- function(profile) {
  if (isTRUE(profile$article_eligible)) {
    paste0(
      "Benchmark evidence: backend comparisons use independent ",
      "process repeats and correctness fingerprints."
    )
  } else {
    paste0(
      "Exploratory evidence only: the ",
      profile$name,
      " profile is useful for harness validation but must not support the ",
      "user-facing performance conclusions."
    )
  }
}

bench_current_result_dir <- function(result_root) {
  result_root <- normalizePath(result_root, mustWork = TRUE)
  pointer <- file.path(result_root, "CURRENT")
  if (!file.exists(pointer)) {
    return(result_root)
  }
  run_id <- trimws(readLines(pointer, n = 1L, warn = FALSE))
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
    stop("unsafe CURRENT benchmark run id", call. = FALSE)
  }
  run_dir <- file.path(result_root, "runs", run_id)
  if (!dir.exists(run_dir)) {
    stop("CURRENT benchmark run directory does not exist", call. = FALSE)
  }
  normalizePath(run_dir)
}

bench_result_run_dir <- function(result_root, run_id) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
    stop("unsafe run id", call. = FALSE)
  }
  run_dir <- file.path(result_root, "runs", run_id)
  if (!dir.exists(run_dir)) {
    stop("benchmark run directory does not exist", call. = FALSE)
  }
  normalizePath(run_dir)
}

bench_manifest_values <- function(manifest) {
  if (
    !identical(names(manifest), c("key", "value")) ||
      anyDuplicated(manifest$key)
  ) {
    stop("run manifest must contain unique key/value rows", call. = FALSE)
  }
  stats::setNames(as.character(manifest$value), manifest$key)
}

bench_compare_environments <- function(
  left,
  right,
  keys = c(
    "r_version",
    "r_platform",
    "os",
    "cpu",
    "memory_mb",
    "r_vector_limit_mb",
    "benchmark_threads",
    "storage_description",
    "package_version",
    "package_Matrix",
    "package_rhdf5",
    "package_Seurat",
    "package_SeuratObject",
    "package_BPCells",
    "package_HDF5Array"
  )
) {
  left_values <- left[keys]
  right_values <- right[keys]
  missing <- keys[
    is.na(left_values) |
      !nzchar(left_values) |
      is.na(right_values) |
      !nzchar(right_values)
  ]
  different <- keys[
    !is.na(left_values) &
      nzchar(left_values) &
      !is.na(right_values) &
      nzchar(right_values) &
      left_values != right_values
  ]
  list(
    comparable = !length(missing) && !length(different),
    missing = unname(missing),
    different = unname(different)
  )
}

# Combine a full study run with supplement runs from the same host. A
# supplement build may only take the place of a study build that was skipped for
# resources; successful study measurements are never replaced. Every row keeps
# its acquisition run_id and gains source_run_id/collection_id; replaced skip
# rows are returned separately so they stay on record. No values are altered.
bench_assemble_supplement <- function(study_dir, supplement_dirs, collection_id) {
  read <- function(dir, name) {
    utils::read.csv(file.path(dir, name), stringsAsFactors = FALSE, check.names = FALSE)
  }
  manifest_of <- function(dir) bench_manifest_values(read(dir, "run_manifest.csv"))
  value <- function(values, key) {
    v <- unname(values[key])
    if (length(v) != 1L || is.na(v)) "" else v
  }
  cell <- function(d) {
    paste(d$source, sprintf("%.0f", as.numeric(d$n_cells)), d$backend, d$export_repeat, sep = "\r")
  }
  tag <- function(d, run_id) {
    d$source_run_id <- rep(run_id, nrow(d))
    d
  }
  append <- function(base, extra, what) {
    if (!setequal(names(base), names(extra))) {
      stop("supplement ", what, " columns differ from the study", call. = FALSE)
    }
    rbind(base, extra[names(base)])
  }

  study <- manifest_of(study_dir)
  if (nzchar(value(study, "result_kind")) || nzchar(value(study, "schedule_filter"))) {
    stop("the study must be a single complete run, not a collection or supplement", call. = FALSE)
  }
  study_id <- value(study, "run_id")
  exports <- tag(read(study_dir, "10_export.csv"), study_id)
  access <- tag(read(study_dir, "20_access.csv"), study_id)
  timings <- tag(read(study_dir, "21_access_timings.csv"), study_id)
  skips <- exports[0L, , drop = FALSE]
  same_keys <- c("profile", "timing_contract", "elapsed_clock", "h5_gzip_level", "h5_uncompressed_gzip_level")

  for (dir in supplement_dirs) {
    supplement <- manifest_of(dir)
    run_id <- value(supplement, "run_id")
    if (!nzchar(value(supplement, "schedule_filter"))) {
      stop(run_id, " is not a filtered supplement", call. = FALSE)
    }
    differing <- same_keys[vapply(same_keys, function(k) value(study, k) != value(supplement, k), logical(1))]
    environment <- bench_compare_environments(study, supplement)
    if (length(differing) || !environment$comparable) {
      stop(
        run_id, " was not measured like the study: ",
        paste(c(differing, environment$missing, environment$different), collapse = ", "),
        call. = FALSE
      )
    }
    extra <- read(dir, "10_export.csv")
    if (!nrow(extra) || any(extra$status != "OK")) {
      stop(run_id, " must contain only successful builds", call. = FALSE)
    }
    target <- match(cell(extra), cell(exports))
    if (anyNA(target) || anyDuplicated(target)) {
      stop(run_id, " has builds outside the study schedule", call. = FALSE)
    }
    if (
      !all(startsWith(exports$status[target], "SKIPPED(resource)")) ||
        any(cell(access) %in% cell(extra))
    ) {
      stop(run_id, " may only replace builds the study skipped for resources", call. = FALSE)
    }
    skips <- rbind(skips, exports[target, , drop = FALSE])
    exports <- append(exports[-target, , drop = FALSE], tag(extra, run_id), "export")
    access <- append(access, tag(read(dir, "20_access.csv"), run_id), "access")
    timings <- append(timings, tag(read(dir, "21_access_timings.csv"), run_id), "timing")
  }

  exports$collection_id <- collection_id
  access$collection_id <- collection_id
  timings$collection_id <- collection_id
  skips$collection_id <- rep(collection_id, nrow(skips))
  list(exports = exports, access = access, timings = timings, resource_skips = skips)
}

bench_backend_ratios <- function(summary, metric, reference) {
  required <- c("source", "n_cells", "backend", metric)
  missing <- setdiff(required, names(summary))
  if (length(missing)) {
    stop("missing ratio columns: ", paste(missing, collapse = ", "))
  }
  keys <- c("source", "n_cells")
  references <- summary[summary$backend == reference, c(keys, metric)]
  names(references)[ncol(references)] <- "reference_value"
  compared <- merge(summary, references, by = keys, all = FALSE)
  compared <- compared[compared$backend != reference, , drop = FALSE]
  compared <- compared[
    is.finite(compared[[metric]]) &
      is.finite(compared$reference_value) &
      compared$reference_value != 0,
    ,
    drop = FALSE
  ]
  if (!nrow(compared)) {
    compared$reference_backend <- character()
    compared$ratio <- numeric()
    return(compared)
  }
  compared$reference_backend <- reference
  compared$ratio <- compared[[metric]] / compared$reference_value
  compared
}

# ---- resource_planning.R ----
# Pure host-resource planning helpers for the real-data benchmark.

# Available RAM, not MemFree or total RAM. Honour the remaining allowance of
# every cgroup v2 ancestor; swap is deliberately excluded. Fail closed on hosts
# where Linux memory accounting is unavailable.
bench_available_memory_bytes <- function(
  meminfo = "/proc/meminfo", cgroup = "/proc/self/cgroup",
  cgroup_root = "/sys/fs/cgroup"
) {
  lines <- readLines(meminfo, warn = FALSE)
  available <- grep("^MemAvailable:", lines, value = TRUE)
  available <- suppressWarnings(as.numeric(gsub("[^0-9]", "", available))) * 1024
  if (length(available) != 1L || !is.finite(available) || available <= 0) {
    stop("cannot determine available RAM", call. = FALSE)
  }
  if (file.exists(cgroup)) {
    entry <- grep("^0::", readLines(cgroup, warn = FALSE), value = TRUE)
    if (length(entry) == 1L) {
      root <- normalizePath(cgroup_root, winslash = "/", mustWork = TRUE)
      directory <- normalizePath(file.path(root, sub("^0::/?", "", entry)),
                                 winslash = "/", mustWork = TRUE)
      if (!(directory == root || startsWith(directory, paste0(root, "/")))) {
        stop("cgroup path outside memory controller", call. = FALSE)
      }
      repeat {
        limit_file <- file.path(directory, "memory.max")
        if (file.exists(limit_file)) {
          limit <- trimws(readLines(limit_file, n = 1L, warn = FALSE))
          if (!identical(limit, "max")) {
            limit <- suppressWarnings(as.numeric(limit))
            used <- suppressWarnings(as.numeric(readLines(
              file.path(directory, "memory.current"), n = 1L, warn = FALSE)))
            if (!is.finite(limit) || !is.finite(used)) stop("invalid cgroup memory accounting")
            available <- min(available, max(0, limit - used))
          }
        }
        if (identical(directory, root)) break
        directory <- dirname(directory)
      }
    }
  }
  if (available <= 0) stop("no available RAM within cgroup limits", call. = FALSE)
  available
}

bench_scale_memory_budget <- function(available_bytes, vector_limit_mb = Inf) {
  if (length(available_bytes) != 1L || !is.finite(available_bytes) || available_bytes <= 0 ||
      length(vector_limit_mb) != 1L || is.na(vector_limit_mb) || vector_limit_mb <= 0) {
    stop("invalid available-memory budget", call. = FALSE)
  }
  min(floor(available_bytes * 0.99), vector_limit_mb * 2^20)
}

# Embedded export memory: 1 GiB fixed plus this many bytes per nonzero. Measured
# peak RSS was 52.6-57 bytes per nonzero from 50k cells up on both public
# sources, so 58 keeps at least 10% headroom over every measured peak. A rare
# underestimate in Scale is caught by the cgroup OOM handling as a resource skip.
BENCH_EMBEDDED_BYTES_PER_NNZ <- 58

# The launch preflight estimates nnz from the source-wide density. Its memory and
# sparse-index failures are re-decided at export time from the frozen plan's
# exact nnz; any other failure (such as disk) binds. Shared by the export
# command and result validation so both apply the same rule.
bench_embedded_preflight_binds <- function(safe, reason) {
  exact_checks <- c(
    "estimated memory exceeds safe budget",
    "estimated nnz exceeds 32-bit sparse index"
  )
  vapply(seq_along(safe), function(i) {
    if (isTRUE(safe[i])) {
      return(FALSE)
    }
    failures <- strsplit(reason[i], "; ", fixed = TRUE)[[1L]]
    length(setdiff(failures, exact_checks)) > 0L
  }, logical(1))
}

# Validate Scale embedded outcomes against their tier's preflight. A binding
# preflight must have skipped with its own reason. A memory/index-only failure
# was re-decided from the exact plan, so the export may have run, skipped on the
# exact estimate or been OOM-killed; a preflight skip must still carry this
# tier's reason. A passing tier never skips at preflight.
bench_check_embedded_outcomes <- function(status, safe, reason) {
  expected_skip <- paste0("SKIPPED(resource): preflight: ", reason)
  preflight_skip <- startsWith(status, "SKIPPED(resource): preflight:")
  binds <- bench_embedded_preflight_binds(safe, reason)
  wrong <- (binds & status != expected_skip) |
    (!binds & preflight_skip & (safe | status != expected_skip))
  if (any(wrong)) {
    stop("embedded outcomes do not match resource preflight", call. = FALSE)
  }
  invisible(TRUE)
}

bench_assess_resources <- function(
  inventory,
  plan,
  memory_mb,
  vector_limit_mb,
  free_disk_bytes,
  bytes_per_nnz = BENCH_EMBEDDED_BYTES_PER_NNZ,
  fixed_memory_mb = 1024,
  memory_fraction = 0.70,
  disk_fraction = 0.80
) {
  required_inventory <- c("source", "nnz_per_cell", "source_bytes")
  required_plan <- c("source", "n_cells")
  if (!all(required_inventory %in% names(inventory))) {
    stop("inventory is missing resource columns", call. = FALSE)
  }
  if (!all(required_plan %in% names(plan))) {
    stop("run plan is missing source or cell count", call. = FALSE)
  }
  if (anyDuplicated(inventory$source)) {
    stop("inventory sources must be unique", call. = FALSE)
  }

  planned <- unique(plan[required_plan])
  matched <- match(planned$source, inventory$source)
  if (anyNA(matched)) {
    stop(
      "resource inventory does not cover source: ",
      planned$source[is.na(matched)][1],
      call. = FALSE
    )
  }
  source <- inventory[matched, , drop = FALSE]
  limit_mb <- min(memory_mb, vector_limit_mb, na.rm = TRUE)
  memory_budget_mb <- limit_mb * memory_fraction
  disk_budget_bytes <- free_disk_bytes * disk_fraction
  estimated_nnz <- planned$n_cells * source$nnz_per_cell
  estimated_peak_mb <- fixed_memory_mb + estimated_nnz * bytes_per_nnz / 2^20
  memory_ok <- is.finite(estimated_peak_mb) &
    estimated_peak_mb <= memory_budget_mb
  index_ok <- is.finite(estimated_nnz) &
    estimated_nnz <= .Machine$integer.max
  disk_ok <- is.finite(source$source_bytes) &
    source$source_bytes <= disk_budget_bytes

  reason <- vapply(
    seq_len(nrow(planned)),
    function(i) {
      failures <- character()
      if (!memory_ok[i]) {
        failures <- c(failures, "estimated memory exceeds safe budget")
      }
      if (!index_ok[i]) {
        failures <- c(failures, "estimated nnz exceeds 32-bit sparse index")
      }
      if (!disk_ok[i]) {
        failures <- c(failures, "source download exceeds safe disk budget")
      }
      if (length(failures)) paste(failures, collapse = "; ") else "safe"
    },
    character(1)
  )

  data.frame(
    source = planned$source,
    n_cells = planned$n_cells,
    estimated_nnz = estimated_nnz,
    estimated_peak_mb = round(estimated_peak_mb),
    memory_budget_mb = round(memory_budget_mb),
    source_bytes = source$source_bytes,
    disk_budget_bytes = disk_budget_bytes,
    memory_ok = memory_ok,
    index_ok = index_ok,
    disk_ok = disk_ok,
    safe = memory_ok & index_ok & disk_ok,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

bench_require_safe_plan <- function(
  assessment,
  allow_unsafe = identical(Sys.getenv("BENCH_ALLOW_UNSAFE"), "1")
) {
  unsafe <- assessment[!assessment$safe, , drop = FALSE]
  if (!nrow(unsafe) || isTRUE(allow_unsafe)) {
    return(TRUE)
  }
  details <- paste0(
    unsafe$source,
    " @ ",
    format(unsafe$n_cells, big.mark = ",", scientific = FALSE),
    " cells: ",
    unsafe$reason
  )
  stop(
    "unsafe benchmark plan for this machine:\n- ",
    paste(details, collapse = "\n- "),
    "\nChoose a smaller profile/tier, or set BENCH_ALLOW_UNSAFE=1 ",
    "only for an intentional stress run.",
    call. = FALSE
  )
}

# Publish a completed run, then activate its CURRENT and latest image copy.
bench_publish_result <- function(stage, result_root, run_id) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
    stop("unsafe run id: ", run_id, call. = FALSE)
  }
  if (file.exists(file.path(stage, "20_access.csv"))) {
    evidence <- function(name) {
      utils::read.csv(file.path(stage, name), stringsAsFactors = FALSE)
    }
    bench_validate_timing_evidence(
      evidence("10_export.csv"),
      evidence("20_access.csv"),
      evidence("21_access_timings.csv")
    )
  }
  bench_recover_activation(result_root)

  tree_fingerprint <- function(path) {
    files <- list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    files <- files[!dir.exists(files)]
    relative <- substring(files, nchar(path) + 2L)
    order <- order(relative)
    data.frame(
      path = relative[order],
      md5 = unname(tools::md5sum(files[order])),
      stringsAsFactors = FALSE
    )
  }

  source_fingerprint <- tree_fingerprint(stage)
  if (!nrow(source_fingerprint)) {
    stop("staged result directory is empty", call. = FALSE)
  }

  runs <- file.path(result_root, "runs")
  dir.create(runs, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(runs, run_id)

  if (dir.exists(destination)) {
    if (!identical(source_fingerprint, tree_fingerprint(destination))) {
      stop("conflicting existing run: ", run_id, call. = FALSE)
    }
  } else {
    staged_destination <- tempfile(
      paste0(".stage-", run_id, "-"),
      tmpdir = runs
    )
    dir.create(staged_destination)
    on.exit(
      {
        if (dir.exists(staged_destination)) {
          unlink(staged_destination, recursive = TRUE)
        }
      },
      add = TRUE
    )
    entries <- list.files(
      stage,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    copied <- file.copy(
      entries,
      staged_destination,
      recursive = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
    if (
      !all(copied) ||
        !identical(
          source_fingerprint,
          tree_fingerprint(staged_destination)
        )
    ) {
      stop("failed to stage an exact result copy", call. = FALSE)
    }
    if (!file.rename(staged_destination, destination)) {
      stop("failed to publish immutable result directory", call. = FALSE)
    }
  }

  bench_activate_result(result_root, run_id)
  message("published immutable benchmark run ", run_id)
}

# Only image exports belong in the latest figures directory. Historical run
# contents are kept intact; QA, reports and nested directories are not copied.
bench_figure_files <- function(directory) {
  files <- list.files(
    directory,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files[
    !dir.exists(files) &
      grepl("\\.(png|pdf|svg|tiff?|jpe?g|webp)$", files, ignore.case = TRUE)
  ]
}

bench_copy_figure_directory <- function(source, destination, replace = FALSE) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  if (replace) {
    entries <- list.files(
      destination,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    unlink(entries, recursive = TRUE)
    if (length(list.files(destination, all.files = TRUE, no.. = TRUE))) {
      stop("cannot replace latest figures; files may be open", call. = FALSE)
    }
  }
  entries <- list.files(
    source,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (
    length(entries) && !all(file.copy(entries, destination, recursive = TRUE))
  ) {
    stop("cannot copy figure directory", call. = FALSE)
  }
  files <- list.files(source, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  copied <- list.files(
    destination,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (
    !identical(files, copied) ||
      !identical(
        unname(tools::md5sum(file.path(source, files))),
        unname(tools::md5sum(file.path(destination, files)))
      )
  ) {
    stop("figure directory copy differs from source", call. = FALSE)
  }
}

# A short-lived rollback directory survives a killed publisher. The next publish
# restores the previous pair before retrying. Nothing is left after success.
bench_recover_activation <- function(result_root) {
  transaction <- file.path(result_root, ".publish")
  if (!dir.exists(transaction)) {
    return(invisible(NULL))
  }
  state_path <- file.path(transaction, "state.rds")
  if (
    file.exists(state_path) &&
      file.exists(file.path(transaction, "switching")) &&
      !file.exists(file.path(transaction, "committed"))
  ) {
    state <- readRDS(state_path)
    latest <- file.path(result_root, "figures")
    backup <- file.path(transaction, "figures.previous")
    if (state$update_figures) {
      if (dir.exists(backup)) {
        bench_copy_figure_directory(backup, latest, replace = TRUE)
      } else if (!state$had_figures) {
        unlink(latest, recursive = TRUE)
      }
    }
    pointer <- file.path(result_root, "CURRENT")
    if (state$had_pointer) {
      if (
        !file.copy(
          file.path(transaction, "CURRENT.previous"),
          pointer,
          overwrite = TRUE
        )
      ) {
        stop("cannot restore previous CURRENT", call. = FALSE)
      }
    } else {
      unlink(pointer)
    }
  }
  unlink(transaction, recursive = TRUE)
  if (dir.exists(transaction)) {
    stop("cannot clean publication transaction", call. = FALSE)
  }
  invisible(NULL)
}

bench_activate_result <- function(result_root, run_id) {
  run <- bench_result_run_dir(result_root, run_id)
  source <- file.path(run, "figures")
  latest <- file.path(result_root, "figures")
  pointer <- file.path(result_root, "CURRENT")
  manifest <- file.path(run, "run_manifest.csv")
  if (file.exists(manifest)) {
    profile <- bench_manifest_values(utils::read.csv(manifest))[["profile"]]
    if (profile %in% c("panel_c2", "scale") && !dir.exists(source)) {
      stop("completed full/scale run has no figures", call. = FALSE)
    }
  }
  update_figures <- dir.exists(source)
  files <- bench_figure_files(source)
  if (update_figures && (!length(files) || any(file.info(files)$size <= 0))) {
    stop("completed run has no valid figure exports", call. = FALSE)
  }
  transaction <- file.path(result_root, ".publish")
  if (!dir.create(transaction)) {
    stop("another publication is active", call. = FALSE)
  }
  on.exit(bench_recover_activation(result_root), add = TRUE)
  if (
    file.exists(pointer) &&
      !file.copy(pointer, file.path(transaction, "CURRENT.previous"))
  ) {
    stop("cannot preserve previous CURRENT", call. = FALSE)
  }
  if (update_figures && dir.exists(latest)) {
    bench_copy_figure_directory(
      latest,
      file.path(transaction, "figures.previous")
    )
  }
  saveRDS(
    list(
      update_figures = update_figures,
      had_figures = dir.exists(latest),
      had_pointer = file.exists(pointer)
    ),
    file.path(transaction, "state.rds")
  )
  pending <- file.path(transaction, "figures.next")
  if (update_figures) {
    dir.create(pending)
    if (
      !all(file.copy(files, pending)) ||
        !identical(
          unname(tools::md5sum(files)),
          unname(tools::md5sum(file.path(pending, basename(files))))
        )
    ) {
      stop("failed to stage latest figure copies", call. = FALSE)
    }
  }
  pending_pointer <- file.path(transaction, "CURRENT.next")
  writeLines(run_id, pending_pointer, useBytes = TRUE)
  fail_at <- Sys.getenv("BENCH_PUBLISH_FAIL_AT")
  checkpoint <- function(name) {
    if (identical(fail_at, name)) stop("injected failure ", name, call. = FALSE)
  }
  checkpoint("before-pointer")
  if (!file.create(file.path(transaction, "switching"))) {
    stop("cannot start publication", call. = FALSE)
  }
  if (update_figures) {
    bench_copy_figure_directory(pending, latest, replace = TRUE)
  }
  checkpoint("after-figures")
  if (identical(fail_at, "interrupt-after-figures")) {
    stop(structure(
      list(message = "injected interrupt", call = NULL),
      class = c("interrupt", "condition")
    ))
  }
  if (identical(fail_at, "crash-after-figures")) {
    quit(status = 99L, runLast = FALSE)
  }
  if (!file.rename(pending_pointer, pointer)) {
    stop("cannot atomically update CURRENT", call. = FALSE)
  }
  checkpoint("after-pointer")
  if (!file.create(file.path(transaction, "committed"))) {
    stop("cannot commit publication", call. = FALSE)
  }
  invisible(run)
}
