bench_root <- normalizePath(testthat::test_path("../bench"), mustWork = FALSE)

skip_unless_bench_tree <- function() {
  testthat::skip_if_not(
    dir.exists(bench_root),
    "repository-only benchmark tree is unavailable"
  )
}

load_benchmark_contract <- function() {
  envir <- parent.frame()
  sys.source(file.path(bench_root, "config.R"), envir = envir)
  sys.source(file.path(bench_root, "helpers.R"), envir = envir)
}

make_valid_benchmark_outcome <- function(phase, pair_id, empty_outcome) {
  row <- empty_outcome(phase, pair_id)
  backend <- tail(strsplit(pair_id, ":", fixed = TRUE)[[1L]], 1L)
  panel <- strsplit(pair_id, ":", fixed = TRUE)[[1L]][[1L]]
  hash <- paste(rep("a", 64), collapse = "")
  row$status <- "OK"
  row$exit_status <- 0L
  row$log_path <- file.path(tempdir(), paste0(gsub(":", "_", pair_id), ".log"))
  row$package_path <- file.path(tempdir(), "library", "CerebroNexus")
  row$artifact_path <- file.path(
    tempdir(),
    paste0(gsub(":", "_", pair_id), ".crb")
  )
  row$peak_rss_mb <- 10
  row$r_heap_peak_mb <- 5
  row$elapsed_secs <- 2
  if (phase == "export") {
    row$source_open_secs <- .1
    row$seurat_shell_secs <- .2
    row$export_secs <- .3
    row$comparison_materialize_secs <- if (panel == "comparison") {
      .4
    } else {
      NA_real_
    }
    row$crb_bytes <- 100
    row$shell_sha256 <- hash
    if (backend == "embedded") {
      row$sidecar_bytes_applicable <- FALSE
      row$sidecar_bytes_reason <- "embedded_has_no_sidecar"
      row$total_bytes <- 100
    } else {
      row$sidecar_path <- file.path(tempdir(), paste0("sidecar.", backend))
      row$sidecar_bytes <- 50
      row$total_bytes <- 150
      row$sidecar_bytes_applicable <- TRUE
    }
  } else {
    row$crb_load_secs <- .1
    row$first_query_secs <- .2
    row[paste0("warmed_query_", 1:5, "_secs")] <- as.list(rep(.3, 5L))
    row$warmed_median_secs <- .3
    row$block_prepare_secs <- .4
    row$block_materialize_secs <- .5
    row$block_ready_secs <- .9
    row$expected_row_fingerprint <- hash
    row$observed_row_fingerprint <- hash
    row$expected_block_fingerprint <- hash
    row$observed_block_fingerprint <- hash
    row$correctness <- "PASS"
    if (backend == "embedded") {
      row$backend_attach_applicable <- FALSE
      row$backend_attach_reason <- "embedded_requires_no_external_attach"
    } else {
      row$backend_attach_secs <- .15
      row$backend_attach_applicable <- TRUE
    }
  }
  row
}

make_tiny_h5ad <- function(path) {
  write_group_attribute <- function(group, name, value) {
    file_id <- rhdf5::H5Fopen(path)
    on.exit(rhdf5::H5Fclose(file_id))
    group_id <- rhdf5::H5Gopen(file_id, group)
    on.exit(rhdf5::H5Gclose(group_id), add = TRUE)
    rhdf5::h5writeAttribute(value, group_id, name)
  }
  rhdf5::h5createFile(path)
  for (group in c("X", "obs", "var")) {
    rhdf5::h5createGroup(path, group)
  }
  rhdf5::h5createDataset(
    path,
    "X/data",
    c(6),
    maxdims = c(6),
    storage.mode = "double",
    chunk = 6
  )
  rhdf5::h5createDataset(
    path,
    "X/indices",
    c(6),
    maxdims = c(6),
    storage.mode = "integer",
    chunk = 6
  )
  rhdf5::h5createDataset(
    path,
    "X/indptr",
    c(6),
    maxdims = c(6),
    storage.mode = "double",
    chunk = 6
  )
  rhdf5::h5write(1:6, path, "X/data")
  rhdf5::h5write(c(0L, 2L, 3L, 1L, 3L, 0L), path, "X/indices")
  rhdf5::h5write(c(0, 2, 3, 4, 5, 6), path, "X/indptr")
  write_group_attribute("X", "encoding-type", "csr_matrix")
  write_group_attribute("X", "encoding-version", "0.1.0")
  write_group_attribute("X", "shape", c(5L, 4L))
  for (group in c("obs", "var")) {
    ids <- if (group == "obs") paste0("c", 1:5) else paste0("g", 1:4)
    rhdf5::h5createDataset(
      path,
      paste0(group, "/_index"),
      length(ids),
      storage.mode = "character",
      size = 8,
      encoding = "UTF-8"
    )
    rhdf5::h5write(ids, path, paste0(group, "/_index"))
    write_group_attribute(group, "_index", "_index")
    write_group_attribute(group, "encoding-type", "dataframe")
    write_group_attribute(group, "encoding-version", "0.2.0")
    write_group_attribute(group, "column-order", "_index")
  }
  invisible(path)
}

tiny_source_spec <- function(path) {
  list(
    expected_bytes = unname(file.info(path)$size),
    expected_sha256 = digest::digest(
      path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    n_cells = 5L,
    group = "X"
  )
}

mutate_h5_vector <- function(path, dataset, values) {
  rhdf5::h5set_extent(path, dataset, length(values))
  rhdf5::h5write(values, path, dataset)
  invisible(path)
}

replace_h5ad_indptr_with_int64 <- function(path, values) {
  input <- tempfile(fileext = ".txt")
  config <- tempfile(fileext = ".txt")
  writeLines(values, input, useBytes = TRUE)
  writeLines(
    c(
      "PATH X/indptr",
      "INPUT-CLASS TEXTIN",
      "INPUT-SIZE 64",
      "INPUT-BYTE-ORDER LE",
      "RANK 1",
      paste("DIMENSION-SIZES", length(values)),
      "OUTPUT-CLASS IN",
      "OUTPUT-SIZE 64",
      "OUTPUT-ARCHITECTURE STD",
      "OUTPUT-BYTE-ORDER LE"
    ),
    config,
    useBytes = TRUE
  )
  rhdf5::h5delete(path, "/X/indptr")
  output <- system2(
    Sys.which("h5import"),
    c(input, "-c", config, "-o", path),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("h5import failed: ", paste(output, collapse = "\n"))
  }
  invisible(path)
}

recursive_atomic_values <- function(x) {
  if (is.list(x)) {
    unlist(lapply(x, recursive_atomic_values), recursive = FALSE)
  } else {
    list(x)
  }
}

test_that("benchmark config is the sole top-level object and has exact protocol values", {
  skip_unless_bench_tree()
  config_lines <- readLines(file.path(bench_root, "config.R"), warn = FALSE)
  parsed <- parse(text = config_lines, keep.source = FALSE)
  expect_length(parsed, 1L)
  expect_identical(as.character(parsed[[1L]][[1L]]), "<-")
  expect_identical(as.character(parsed[[1L]][[2L]]), "BENCH_CONFIG")

  source(file.path(bench_root, "config.R"), local = environment())
  expect_identical(
    BENCH_CONFIG,
    list(
      schema_version = 1L,
      source = list(
        key = "human_pfc_mssm",
        url = paste0(
          "https://datasets.cellxgene.cziscience.com/",
          "0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad"
        ),
        expected_bytes = 36092176654,
        expected_sha256 = paste0(
          "c62456941372b90bcf0df38e8cb1c34d",
          "d060bc5a507270ab1d068cbe6f1dfd54"
        ),
        n_cells = 4140453L,
        group = "X",
        organism = "hg38",
        slot = "data"
      ),
      comparison_fixed_tiers = c(
        tier_1k = 1000L,
        tier_5k = 5000L,
        tier_10k = 10000L,
        tier_25k = 25000L,
        tier_100k = 100000L,
        tier_250k = 250000L
      ),
      common_target = 500000L,
      common_min_exclusive = 250000L,
      full_scale_fixed_tiers = c(
        tier_1m = 1000000L,
        tier_2m = 2000000L,
        full = 4140453L
      ),
      comparison_backends = c("embedded", "bpcells", "h5"),
      comparison_repeats = 3L,
      full_scale_repeats = 4L,
      query_genes = 5L,
      warmed_iterations = 5L,
      rss_poll_ms = 500L,
      sparse_index_limit = .Machine$integer.max
    )
  )
})

test_that("fixed-four blocks implement centered quotas including empty strata", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  expect_identical(
    bench_stratified_blocks(8L, 1L),
    data.frame(
      stratum = 1:4,
      start = c(2L, 4L, 6L, 7L),
      end = c(1L, 3L, 5L, 7L),
      n = c(0L, 0L, 0L, 1L)
    )
  )
  expect_identical(
    bench_stratified_blocks(1L, 1L),
    data.frame(
      stratum = 1:4,
      start = c(1L, 1L, 1L, 1L),
      end = c(0L, 0L, 0L, 1L),
      n = c(0L, 0L, 0L, 1L)
    )
  )
  expect_identical(
    bench_stratified_blocks(4140453L, 4140453L)$n,
    c(1035113L, 1035113L, 1035113L, 1035114L)
  )
})

test_that("fixed-four blocks remain exact at the maximum supported population", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  n_total <- .Machine$integer.max
  one <- expect_no_warning(bench_stratified_blocks(n_total, 1L))
  full <- expect_no_warning(bench_stratified_blocks(n_total, n_total))

  for (blocks in list(one, full)) {
    expect_identical(names(blocks), c("stratum", "start", "end", "n"))
    expect_equal(nrow(blocks), 4L)
    expect_false(anyNA(blocks))
    expect_true(all(blocks$start >= 1L & blocks$start <= n_total))
    expect_true(all(blocks$end >= 0L & blocks$end <= n_total))
    expect_equal(
      as.double(blocks$end),
      as.double(blocks$start) + as.double(blocks$n) - 1
    )
  }
  expect_equal(sum(one$n), 1)
  expect_equal(sum(full$n), n_total)
  expect_identical(full$start, c(1L, 536870912L, 1073741824L, 1610612736L))
  expect_identical(full$end, c(536870911L, 1073741823L, 1610612735L, n_total))
})

test_that("fixed-four selected sets are nested and complete for all small populations", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  for (n_total in 1:64) {
    previous <- integer()
    for (n_take in seq_len(n_total)) {
      selected <- bench_stratified_indices(n_total, n_take)
      expect_identical(length(selected), n_take)
      expect_identical(selected, sort(unique(selected)))
      expect_true(all(selected >= 1L & selected <= n_total))
      expect_setequal(previous, intersect(previous, selected))
      expect_length(setdiff(selected, previous), 1L)
      previous <- selected
    }
    expect_identical(previous, seq_len(n_total))
  }

  additions <- vapply(
    1:8,
    function(n_take) {
      current <- bench_stratified_indices(8L, n_take)
      prior <- if (n_take == 1L) {
        integer()
      } else {
        bench_stratified_indices(8L, n_take - 1L)
      }
      setdiff(current, prior)
    },
    integer(1L)
  )
  expect_identical(additions, c(7L, 3L, 5L, 1L, 8L, 4L, 6L, 2L))
  expect_identical(bench_stratified_indices(8L, 5L), c(1L, 3L, 5L, 7L, 8L))
})

test_that("sampler and exact nnz inputs reject invalid scalar and count values", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  invalid_totals <- list(
    NA_real_,
    Inf,
    1.5,
    0,
    c(1, 2),
    .Machine$integer.max + 1
  )
  for (value in invalid_totals) {
    expect_error(bench_stratified_blocks(value, 1L))
  }
  for (value in list(NA_real_, Inf, 1.5, -1, 0, 3)) {
    expect_error(bench_stratified_indices(2L, value))
  }
  expect_error(bench_exact_selected_nnz(3L, 2L, c(1, 2)))
  expect_error(bench_exact_selected_nnz(3L, 2L, c(1, -1, 2)))
  expect_error(bench_exact_selected_nnz(3L, 2L, c(1, 1.5, 2)))
  expect_error(bench_exact_selected_nnz(3L, 3L, c(2^53, 1, 0)), "exact")
})

test_that("exact nnz uses the same nested indices", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  nnz <- c(11, 7, 5, 3, 2, 13, 17, 19)
  for (n_take in 1:8) {
    expected <- sum(nnz[bench_stratified_indices(8L, n_take)])
    expect_identical(bench_exact_selected_nnz(8L, n_take, nnz), expected)
  }
})

test_that("common tier freezer finds the exact largest legal non-multiple-of-four size", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  nnz <- rep(4294, 500000L)
  nnz[bench_stratified_indices(500000L, 499999L)] <- 4295
  limit <- bench_exact_selected_nnz(500000L, 499997L, nnz)
  frozen <- bench_freeze_common_tier(
    target = 500000L,
    minimum_exclusive = 250000L,
    nnz_per_cell = nnz,
    limit = limit
  )

  expect_identical(
    frozen,
    list(
      common_target_actual = 499997L,
      exact_nnz = limit,
      target_reduced = TRUE
    )
  )
  expect_true(frozen$common_target_actual %% 4L != 0L)
  expect_gt(
    bench_exact_selected_nnz(500000L, frozen$common_target_actual + 1L, nnz),
    limit
  )
})

test_that("common tier freezer aborts instead of constructing a tier below the floor", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  expect_error(
    bench_freeze_common_tier(8L, 4L, rep(10, 8L), 40),
    "minimum_exclusive"
  )
})

test_that("comparison schedule exactly encodes both counterbalanced execution orders", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  tiers <- c(
    tier_1k = 1000L,
    tier_5k = 5000L,
    tier_10k = 10000L,
    tier_25k = 25000L,
    tier_100k = 100000L,
    tier_250k = 250000L,
    common = 499997L
  )
  backends <- c("embedded", "bpcells", "h5")
  schedule <- bench_comparison_schedule(tiers, backends, repeats = 3L)
  expect_identical(
    names(schedule),
    c(
      "pair_id",
      "panel",
      "repeat",
      "tier_label",
      "n_cells",
      "backend",
      "export_order",
      "access_order"
    )
  )
  expect_equal(nrow(schedule), 63L)
  expect_equal(nrow(schedule) * 2L, 126L)
  expect_identical(as.integer(table(schedule$tier_label)), rep(9L, 7L))

  export_tiers <- list(
    names(tiers),
    c(names(tiers)[-1L], names(tiers)[1L]),
    c(names(tiers)[-(1:2)], names(tiers)[1:2])
  )
  access_tiers <- list(
    c(names(tiers)[-1L], names(tiers)[1L]),
    c(names(tiers)[-(1:2)], names(tiers)[1:2]),
    c(names(tiers)[-(1:3)], names(tiers)[1:3])
  )
  export_backends <- list(
    c("embedded", "bpcells", "h5"),
    c("bpcells", "h5", "embedded"),
    c("h5", "embedded", "bpcells")
  )
  access_backends <- list(
    c("h5", "embedded", "bpcells"),
    c("embedded", "bpcells", "h5"),
    c("bpcells", "h5", "embedded")
  )
  expected_ids <- function(repeat_id, tier_order, backend_order) {
    unlist(
      lapply(
        tier_order,
        function(tier) {
          paste("comparison", repeat_id, tier, backend_order, sep = ":")
        }
      ),
      use.names = FALSE
    )
  }
  export_ids <- unlist(
    Map(expected_ids, 1:3, export_tiers, export_backends),
    use.names = FALSE
  )
  access_ids <- unlist(
    Map(expected_ids, 1:3, access_tiers, access_backends),
    use.names = FALSE
  )

  expect_identical(schedule[order(schedule$export_order), ]$pair_id, export_ids)
  expect_identical(schedule[order(schedule$access_order), ]$pair_id, access_ids)
  for (repeat_id in 1:3) {
    expect_identical(
      bench_tier_order(schedule, repeat_id, "export"),
      export_tiers[[repeat_id]]
    )
    expect_identical(
      bench_tier_order(schedule, repeat_id, "access"),
      access_tiers[[repeat_id]]
    )
    for (tier_label in export_tiers[[repeat_id]]) {
      expect_identical(
        bench_backend_order(schedule, repeat_id, tier_label, "export"),
        export_backends[[repeat_id]]
      )
      expect_identical(
        bench_backend_order(schedule, repeat_id, tier_label, "access"),
        access_backends[[repeat_id]]
      )
    }
  }
  expect_silent(bench_validate_schedule(schedule, 63L))
})

test_that("full-scale schedule exactly encodes execution orders and four full pairs", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  tiers <- c(
    common = 499997L,
    tier_1m = 1000000L,
    tier_2m = 2000000L,
    full = 4140453L
  )
  schedule <- bench_full_schedule(tiers, repeats = 4L)
  expect_equal(nrow(schedule), 16L)
  expect_equal(nrow(schedule) * 2L, 32L)
  expect_identical(sum(schedule$tier_label == "full"), 4L)
  expect_true(all(schedule$backend == "bpcells"))

  export_tiers <- list(
    c("common", "tier_1m", "tier_2m", "full"),
    c("tier_1m", "tier_2m", "full", "common"),
    c("tier_2m", "full", "common", "tier_1m"),
    c("full", "common", "tier_1m", "tier_2m")
  )
  access_tiers <- list(
    c("tier_2m", "full", "common", "tier_1m"),
    c("full", "common", "tier_1m", "tier_2m"),
    c("common", "tier_1m", "tier_2m", "full"),
    c("tier_1m", "tier_2m", "full", "common")
  )
  expected_ids <- function(repeat_id, tier_order) {
    paste("full_scale", repeat_id, tier_order, "bpcells", sep = ":")
  }
  export_ids <- unlist(Map(expected_ids, 1:4, export_tiers), use.names = FALSE)
  access_ids <- unlist(Map(expected_ids, 1:4, access_tiers), use.names = FALSE)

  expect_identical(schedule[order(schedule$export_order), ]$pair_id, export_ids)
  expect_identical(schedule[order(schedule$access_order), ]$pair_id, access_ids)
  for (repeat_id in 1:4) {
    expect_identical(
      bench_tier_order(schedule, repeat_id, "export"),
      export_tiers[[repeat_id]]
    )
    expect_identical(
      bench_tier_order(schedule, repeat_id, "access"),
      access_tiers[[repeat_id]]
    )
    for (tier_label in names(tiers)) {
      expect_identical(
        bench_backend_order(schedule, repeat_id, tier_label, "export"),
        "bpcells"
      )
      expect_identical(
        bench_backend_order(schedule, repeat_id, tier_label, "access"),
        "bpcells"
      )
    }
  }
  expect_silent(bench_validate_schedule(schedule, 16L))
})

test_that("schedule validation rejects malformed schemas, orders, ids, and repeat blocks", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  schedule <- bench_full_schedule(
    c(common = 10L, tier_1m = 20L, tier_2m = 30L, full = 40L),
    4L
  )
  expect_error(bench_validate_schedule(schedule[-1L], 16L), "schema")
  duplicate_id <- schedule
  duplicate_id$pair_id[2L] <- duplicate_id$pair_id[1L]
  expect_error(bench_validate_schedule(duplicate_id, 16L), "pair_id")
  bad_order <- schedule
  bad_order$export_order[2L] <- bad_order$export_order[1L]
  expect_error(bench_validate_schedule(bad_order, 16L), "export_order")
  interleaved_phase <- schedule
  first_r2 <- which(interleaved_phase[["repeat"]] == 2L)[[1L]]
  last_r1 <- tail(which(interleaved_phase[["repeat"]] == 1L), 1L)
  swap <- interleaved_phase$export_order[c(first_r2, last_r1)]
  interleaved_phase$export_order[c(first_r2, last_r1)] <- rev(swap)
  expect_error(bench_validate_schedule(interleaved_phase, 16L), "contiguous")
  split_repeat <- schedule[c(1L, 5L, 2:4, 6:16), ]
  expect_error(bench_validate_schedule(split_repeat, 16L), "contiguous")

  mixed_panel <- schedule
  mixed_panel$panel[[1L]] <- "comparison"
  expect_error(bench_validate_schedule(mixed_panel, 16L), "panel")
  invalid_panel <- schedule
  invalid_panel$panel[] <- "bogus"
  expect_error(bench_validate_schedule(invalid_panel, 16L), "panel")

  bogus_tier <- schedule
  bogus_tier$tier_label[[1L]] <- "bogus"
  expect_error(bench_validate_schedule(bogus_tier, 16L), "tier_label")
  bogus_backend <- schedule
  bogus_backend$backend[[1L]] <- "bogus"
  expect_error(bench_validate_schedule(bogus_backend, 16L), "backend")

  inconsistent_cells <- schedule
  inconsistent_cells$n_cells[[1L]] <- inconsistent_cells$n_cells[[1L]] + 1L
  expect_error(bench_validate_schedule(inconsistent_cells, 16L), "n_cells")

  fabricated_ids <- schedule
  fabricated_ids$pair_id <- paste0("x", seq_len(nrow(fabricated_ids)))
  expect_error(bench_validate_schedule(fabricated_ids, 16L), "pair_id")

  shifted_repeats <- schedule
  shifted_repeats[["repeat"]] <- shifted_repeats[["repeat"]] + 4L
  expect_error(bench_validate_schedule(shifted_repeats, 16L), "repeat")
})

test_that("eligibility has exact schema, vocabulary, and comparison propagation", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  tiers <- c(tier_125k = 125000L, tier_250k = 250000L, common = 499997L)
  eligibility <- bench_eligibility(
    "comparison",
    tiers,
    c(tier_125k = 100, tier_250k = 201, common = 150),
    200
  )
  expect_identical(
    names(eligibility),
    c(
      "panel",
      "tier_label",
      "n_cells",
      "backend",
      "exact_nnz",
      "status",
      "reason"
    )
  )
  expect_equal(nrow(eligibility), 9L)
  expect_setequal(
    unique(eligibility$status),
    c("SCHEDULED", "UNSUPPORTED_DGCMATRIX_INDEX")
  )
  unsupported <- eligibility$tier_label == "tier_250k"
  expect_true(all(
    eligibility$status[unsupported] == "UNSUPPORTED_DGCMATRIX_INDEX"
  ))
  expect_true(all(!is.na(eligibility$reason[unsupported])))
  expect_true(all(eligibility$status[!unsupported] == "SCHEDULED"))
  expect_true(all(is.na(eligibility$reason[!unsupported])))
})

test_that("full-scale eligibility schedules only BPCells and classifies other rows", {
  skip_unless_bench_tree()
  load_benchmark_contract()

  tiers <- c(
    common = 499997L,
    tier_1m = 1000000L,
    tier_2m = 2000000L,
    full = 4140453L
  )
  exact <- c(common = 100, tier_1m = 150, tier_2m = 250, full = 300)
  eligibility <- bench_eligibility("full_scale", tiers, exact, limit = 200)
  expect_equal(nrow(eligibility), 12L)
  expect_setequal(
    unique(eligibility$status),
    c(
      "SCHEDULED",
      "NOT_APPLICABLE_PROTOCOL",
      "UNSUPPORTED_DGCMATRIX_INDEX"
    )
  )
  expect_true(all(
    eligibility$status[eligibility$backend == "bpcells"] == "SCHEDULED"
  ))
  expect_true(all(is.na(eligibility$reason[eligibility$backend == "bpcells"])))
  common_non_bp <- eligibility$tier_label == "common" &
    eligibility$backend != "bpcells"
  expect_true(all(
    eligibility$status[common_non_bp] == "NOT_APPLICABLE_PROTOCOL"
  ))
  oversized_non_bp <- eligibility$tier_label %in%
    c("tier_2m", "full") &
    eligibility$backend != "bpcells"
  expect_true(all(
    eligibility$status[oversized_non_bp] == "UNSUPPORTED_DGCMATRIX_INDEX"
  ))
  legal_non_bp <- eligibility$tier_label == "tier_1m" &
    eligibility$backend != "bpcells"
  expect_true(all(
    eligibility$status[legal_non_bp] == "NOT_APPLICABLE_PROTOCOL"
  ))

  scheduled <- eligibility[eligibility$status == "SCHEDULED", ]
  schedule <- bench_full_schedule(tiers, 4L)
  expect_setequal(unique(schedule$tier_label), scheduled$tier_label)
  expect_true(all(schedule$backend == scheduled$backend[[1L]]))
})

test_that("SHA-256 helpers use file bytes and typed XDR-v3 object payloads", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()

  path <- tempfile()
  writeBin(as.raw(c(0, 1, 2, 255)), path)
  expect_match(bench_sha256_file(path), "^[0-9a-f]{64}$")
  expect_identical(
    bench_sha256_file(path),
    digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
  )
  object <- list(schema = "typed", n = 1L)
  expected <- serialize(object, NULL, ascii = FALSE, xdr = TRUE, version = 3)
  expect_identical(
    bench_sha256_object(object),
    digest::digest(expected, algo = "sha256", serialize = FALSE)
  )
  expect_error(bench_sha256_file(tempfile()), "existing regular file")
})

test_that("source identity fails closed before BPCells is opened", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("rhdf5")
  load_benchmark_contract()
  path <- make_tiny_h5ad(tempfile(fileext = ".h5ad"))
  spec <- tiny_source_spec(path)
  opens <- 0L
  original <- bench_open_source
  on.exit(
    assign("bench_open_source", original, envir = environment()),
    add = TRUE
  )
  assign(
    "bench_open_source",
    function(...) {
      opens <<- opens + 1L
      stop("opened")
    },
    envir = environment()
  )

  bad_bytes <- spec
  bad_bytes$expected_bytes <- bad_bytes$expected_bytes + 1
  expect_error(bench_source_inventory(path, bad_bytes), "size")
  expect_identical(opens, 0L)
  bad_hash <- spec
  bad_hash$expected_sha256 <- paste(rep("0", 64), collapse = "")
  expect_error(bench_source_inventory(path, bad_hash), "SHA-256")
  expect_identical(opens, 0L)
  expect_error(bench_validate_source_file(
    path,
    within(spec, expected_sha256 <- "ABC")
  ))
})

test_that("H5AD CSR inventory is exact, oriented, and opens only after validation", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("BPCells")
  load_benchmark_contract()
  path <- make_tiny_h5ad(tempfile(fileext = ".h5ad"))
  spec <- tiny_source_spec(path)
  expect_identical(bench_read_h5ad_indptr(path), c(0, 2, 3, 4, 5, 6))
  inventory <- bench_source_inventory(path, spec)
  expect_identical(inventory$n_cells, 5L)
  expect_identical(inventory$n_genes, 4L)
  expect_identical(inventory$nnz, 6)
  expect_identical(inventory$nnz_per_cell, c(2, 1, 1, 1, 1))
  expect_identical(dim(inventory$matrix), c(4L, 5L))
  expect_identical(rownames(inventory$matrix), paste0("g", 1:4))
  expect_identical(colnames(inventory$matrix), paste0("c", 1:5))
})

test_that("H5AD int64 indptr precision loss fails closed during read", {
  skip_unless_bench_tree()
  skip_if_not_installed("rhdf5")
  skip_if(
    !nzchar(Sys.which("h5import")),
    "h5import is required for the int64 fixture"
  )
  load_benchmark_contract()
  path <- make_tiny_h5ad(tempfile(fileext = ".h5ad"))
  replace_h5ad_indptr_with_int64(
    path,
    c("0", "2", "3", "4", "5", "9007199254740993")
  )

  expect_warning(
    rhdf5::h5read(path, "/X/indptr", bit64conversion = "double", drop = TRUE),
    "integer precision lost"
  )
  expect_error(bench_read_h5ad_indptr(path), "integer precision lost")
})

test_that("H5AD CSR structural corruption fails before source opening", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("rhdf5")
  load_benchmark_contract()

  contract_env <- environment()
  assert_bad <- function(mutator, pattern) {
    path <- make_tiny_h5ad(tempfile(fileext = ".h5ad"))
    mutator(path)
    spec <- tiny_source_spec(path)
    opens <- 0L
    original <- get("bench_open_source", envir = contract_env, inherits = FALSE)
    on.exit(
      assign("bench_open_source", original, envir = contract_env),
      add = TRUE
    )
    assign(
      "bench_open_source",
      function(...) {
        opens <<- opens + 1L
        stop("opened")
      },
      envir = contract_env
    )
    expect_error(bench_source_inventory(path, spec), pattern)
    expect_identical(opens, 0L)
  }
  replace_group_attribute <- function(path, name, value) {
    rhdf5::h5deleteAttribute(path, "/X", name)
    file_id <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDWR")
    on.exit(rhdf5::H5Fclose(file_id))
    group_id <- rhdf5::H5Gopen(file_id, "X")
    on.exit(rhdf5::H5Gclose(group_id), add = TRUE)
    rhdf5::h5writeAttribute(value, group_id, name)
  }
  assert_bad(
    function(p) mutate_h5_vector(p, "X/indptr", c(0, 2.5, 3, 4, 5, 6)),
    "integer"
  )
  assert_bad(
    function(p) mutate_h5_vector(p, "X/indptr", c(1, 2, 3, 4, 5, 6)),
    "start"
  )
  assert_bad(
    function(p) mutate_h5_vector(p, "X/indptr", c(0, 2, 1, 4, 5, 6)),
    "monotone"
  )
  assert_bad(
    function(p) mutate_h5_vector(p, "X/indptr", c(0, 2, 3, 4, 5, 2^53 + 2)),
    "2\\^53"
  )
  assert_bad(
    function(p) mutate_h5_vector(p, "X/indptr", c(0, 2, 3, 4, 6)),
    "length"
  )
  assert_bad(function(p) mutate_h5_vector(p, "X/data", 1:5), "data.*extent")
  assert_bad(
    function(p) mutate_h5_vector(p, "X/indices", 0:4),
    "indices.*extent"
  )
  assert_bad(
    function(p) replace_group_attribute(p, "encoding-type", "csc_matrix"),
    "csr_matrix"
  )
  assert_bad(
    function(p) replace_group_attribute(p, "shape", c(5, 4, 1)),
    "shape"
  )
  assert_bad(
    function(p) replace_group_attribute(p, "shape", c(5.5, 4)),
    "shape"
  )
})

test_that("source n_cells disagreement fails before source opening", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("rhdf5")
  load_benchmark_contract()
  path <- make_tiny_h5ad(tempfile(fileext = ".h5ad"))
  spec <- tiny_source_spec(path)
  spec$n_cells <- 4L
  opens <- 0L
  original <- bench_open_source
  on.exit(
    assign("bench_open_source", original, envir = environment()),
    add = TRUE
  )
  assign(
    "bench_open_source",
    function(...) {
      opens <<- opens + 1L
      stop("opened")
    },
    envir = environment()
  )
  expect_error(bench_source_inventory(path, spec), "n_cells")
  expect_identical(opens, 0L)
})

test_that("identity fingerprints canonicalize UTF-8 and reject unsafe IDs", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()
  latin <- iconv("caf\u00e9", from = "UTF-8", to = "latin1")
  Encoding(latin) <- "latin1"
  expect_identical(
    bench_identity_fingerprint(latin),
    bench_identity_fingerprint("caf\u00e9")
  )
  expect_error(bench_identity_fingerprint(c("a", NA_character_)), "missing")
  expect_error(bench_identity_fingerprint(c("a", "")), "empty")
  expect_error(bench_identity_fingerprint(c("a", "a")), "duplicate")
  bytes <- "\xff"
  Encoding(bytes) <- "bytes"
  expect_error(bench_identity_fingerprint(bytes), "bytes")
  expect_error(bench_identity_fingerprint(charToRaw("a")), "character")
})

test_that("numeric fingerprints bind values, dimensions, and ordered identities", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()
  base <- bench_numeric_fingerprint(c(x = 1, y = 2), "g1", c("c1", "c2"))
  expect_identical(
    base,
    bench_numeric_fingerprint(c(y = 1, x = 2), "g1", c("c1", "c2"))
  )
  changed <- c(
    bench_numeric_fingerprint(c(1, 3), "g1", c("c1", "c2")),
    bench_numeric_fingerprint(c(1, 2), "G1", c("c1", "c2")),
    bench_numeric_fingerprint(c(1, 2), "g1", c("c2", "c1")),
    bench_numeric_fingerprint(matrix(c(1, 2), 2, 1), c("g1", "g2"), "c1")
  )
  expect_true(all(changed != base))
  expect_false(identical(
    bench_numeric_fingerprint(c(c1 = 1, c2 = 2), "g1", c("c1", "c2")),
    bench_numeric_fingerprint(c(c2 = 2, c1 = 1), "g1", c("c2", "c1"))
  ))
  expect_false(identical(
    base,
    bench_numeric_fingerprint(c(x = 1, y = 2), "g1", c("renamed", "c2"))
  ))
  expect_error(
    bench_numeric_fingerprint(matrix(1:4, 2, 2), "g1", c("c1", "c2")),
    "dimensions"
  )
  expect_error(
    bench_numeric_fingerprint(c(1, Inf), "g1", c("c1", "c2")),
    "finite"
  )
})

test_that("five-gene selection is deterministic, sparse, and median-first", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("BPCells")
  load_benchmark_contract()
  m <- Matrix::sparseMatrix(
    i = c(1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 5, 6, 6),
    j = c(1, 1, 2, 1, 2, 3, 1, 2, 1, 2, 3, 4, 1, 2),
    x = 1,
    dims = c(7, 4),
    dimnames = list(paste0("g", 1:7), paste0("c", 1:4))
  )
  source <- BPCells::write_matrix_dir(m, tempfile())
  panel <- bench_select_query_genes(source, 1:4, 5L)
  expect_identical(panel, bench_select_query_genes(source, 1:4, 5L))
  expect_identical(
    names(panel),
    c("gene", "role", "density", "source_row", "tie_break_rank")
  )
  expect_equal(nrow(panel), 5L)
  expect_identical(panel$role, c("first", rep("block", 4)))
  expect_true(all(panel$density > 0))
  active_density <- c(1, 2, 3, 2, 4, 2) / 4
  expect_equal(
    panel$density[[1L]],
    active_density[which.min(abs(active_density - median(active_density)))]
  )
  tied <- panel[panel$density == 0.5, ]
  expect_identical(tied$source_row, sort(tied$source_row))
  expect_error(bench_select_query_genes(source, 1:4, 7L), "expressed")
})

test_that("query plans hash bounded materialization without retaining cells or values", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("BPCells")
  load_benchmark_contract()
  sys.source(file.path(bench_root, "config.R"), envir = environment())
  m <- Matrix::sparseMatrix(
    i = rep(1:5, each = 2),
    j = rep(1:2, 5),
    x = 1:10,
    dims = c(5, 5),
    dimnames = list(paste0("g", 1:5), paste0("c", 1:5))
  )
  source <- BPCells::write_matrix_dir(m, tempfile())
  genes <- data.frame(
    gene = paste0("g", 1:5),
    role = c("first", rep("block", 4)),
    density = rep(0.4, 5),
    source_row = 1:5,
    tie_break_rank = 1:5,
    stringsAsFactors = FALSE
  )
  plan <- bench_build_query_plan(source, 1:5, genes)
  expect_match(plan$query_plan_sha256, "^[0-9a-f]{64}$")
  expect_identical(plan$dimensions, c(genes = 5L, cells = 5L))
  expect_identical(plan$source_dimensions, c(genes = 5L, cells = 5L))
  expect_length(plan$boundaries, 4L)
  atoms <- recursive_atomic_values(plan)
  expect_false(any(vapply(
    atoms,
    function(x) {
      is.character(x) &&
        identical(unname(x), paste0("c", 1:5))
    },
    logical(1)
  )))
  expect_false(any(vapply(
    atoms,
    function(x) is.numeric(x) && length(x) == 25L,
    logical(1)
  )))
  changed <- source
  m[1, 1] <- 99
  changed <- BPCells::write_matrix_dir(m, tempfile())
  expect_false(identical(
    plan$query_plan_sha256,
    bench_build_query_plan(changed, 1:5, genes)$query_plan_sha256
  ))
  expect_error(
    bench_build_query_plan(source, c(1L, 3L, 2L, 4L), genes),
    "source order"
  )
  expect_error(bench_build_query_plan(source, c(1L, 1L), genes), "unique")

  wider <- Matrix::sparseMatrix(
    i = rep(1:5, each = 2),
    j = rep(c(1L, 8L), 5),
    x = 1:10,
    dims = c(5, 8),
    dimnames = list(paste0("g", 1:5), paste0("c", 1:8))
  )
  wider <- BPCells::write_matrix_dir(wider, tempfile())
  subset_indices <- bench_stratified_indices(8L, 5L)
  subset_plan <- bench_build_query_plan(wider, subset_indices, genes)
  expect_identical(subset_plan$source_dimensions, c(genes = 5L, cells = 8L))
  expect_identical(subset_plan$boundaries, bench_stratified_blocks(8L, 5L))
  expect_identical(
    subset_plan$ordered_indices_sha256,
    bench_sha256_object(list(
      schema = "bench-ordered-indices-v1",
      indices = subset_indices
    ))
  )
})

test_that("manifest writers enforce schemas and round-trip stable UTF-8 CSV", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()
  hash <- paste(rep("a", 64), collapse = "")
  sampling <- rbind(
    cbind(tier_label = "small", n_cells = 4L, bench_stratified_blocks(4L, 4L)),
    cbind(tier_label = "large", n_cells = 8L, bench_stratified_blocks(8L, 8L))
  )
  sampling <- data.frame(
    sampling,
    exact_nnz = rep(c(3, 6), each = 4L),
    indices_sha256 = hash,
    cell_identity_sha256 = hash,
    shell_sha256 = NA_character_,
    stringsAsFactors = FALSE
  )
  sampling_path <- tempfile(fileext = ".csv")
  expect_silent(bench_write_sampling_manifest(sampling_path, sampling))
  expect_identical(
    names(read.csv(sampling_path, stringsAsFactors = FALSE)),
    names(sampling)
  )
  expect_error(
    bench_write_sampling_manifest(tempfile(), sampling[-1L]),
    "schema"
  )
  expect_error(
    bench_write_sampling_manifest(
      tempfile(),
      rbind(sampling[1, ], sampling[1, ])
    ),
    "unique"
  )
  missing_stratum <- sampling[-1L, ]
  expect_error(
    bench_write_sampling_manifest(tempfile(), missing_stratum),
    "strata"
  )
  bad_boundary <- sampling
  bad_boundary$end[[1L]] <- bad_boundary$end[[1L]] + 1L
  expect_error(
    bench_write_sampling_manifest(tempfile(), bad_boundary),
    "boundary"
  )
  bad_sum <- sampling
  bad_sum$n[[1L]] <- 0L
  bad_sum$end[[1L]] <- bad_sum$start[[1L]] - 1L
  expect_error(bench_write_sampling_manifest(tempfile(), bad_sum), "sum")
  inconsistent <- sampling
  inconsistent$exact_nnz[[1L]] <- inconsistent$exact_nnz[[1L]] + 1
  expect_error(
    bench_write_sampling_manifest(tempfile(), inconsistent),
    "consistent"
  )

  plan <- list(
    schema = "bench-query-plan-v1",
    source_sha256 = hash,
    sampling_sha256 = hash,
    dimensions = c(genes = 5L, cells = 2L),
    source_dimensions = c(genes = 5L, cells = 2L),
    genes = data.frame(
      gene = c("g\u00e9", paste0("g", 2:5)),
      role = c("first", rep("block", 4L)),
      density = rep(0.5, 5L),
      source_row = 1:5,
      tie_break_rank = 1:5
    ),
    ordered_indices_sha256 = hash,
    cell_identity_sha256 = hash,
    first_row_numeric_sha256 = hash,
    block_numeric_sha256 = hash,
    boundaries = bench_stratified_blocks(2L, 2L)
  )
  plan$query_plan_sha256 <- bench_sha256_object(plan)
  query_path <- tempfile(fileext = ".csv")
  expect_silent(bench_write_query_manifest(query_path, list(tiny = plan)))
  query <- read.csv(
    query_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  expect_identical(query$gene[[1L]], "g\u00e9")
  expect_error(
    bench_write_query_manifest(tempfile(), list(a = plan, a = plan)),
    "unique"
  )
  expect_error(
    bench_write_query_manifest(
      tempfile(),
      list(tiny = within(plan, genes <- genes[-1L]))
    ),
    "gene"
  )
  bad_roles <- plan
  bad_roles$genes$role[[2L]] <- "first"
  expect_error(
    bench_write_query_manifest(tempfile(), list(tiny = bad_roles)),
    "role"
  )
  bad_dimensions <- plan
  bad_dimensions$dimensions[["genes"]] <- 4L
  expect_error(
    bench_write_query_manifest(tempfile(), list(tiny = bad_dimensions)),
    "five"
  )
  bad_schema <- plan
  bad_schema$schema <- "other"
  expect_error(
    bench_write_query_manifest(tempfile(), list(tiny = bad_schema)),
    "schema"
  )
  bad_boundaries <- plan
  bad_boundaries$boundaries$end[[4L]] <- 1L
  expect_error(
    bench_write_query_manifest(tempfile(), list(tiny = bad_boundaries)),
    "boundar"
  )
  bad_hash <- plan
  bad_hash$query_plan_sha256 <- hash
  expect_error(
    bench_write_query_manifest(tempfile(), list(tiny = bad_hash)),
    "query_plan_sha256"
  )
})

test_that("synthetic shells preserve dgCMatrix and BPCells representations with one contract", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("BPCells")
  load_benchmark_contract()

  dense <- matrix(
    c(1, 0, 2, 0, 0, 3, 0, 4, 5, 0, 6, 0, 0, 7, 0, 8, 9, 0, 10, 0),
    nrow = 4L,
    dimnames = list(paste0("g", 1:4), paste0("c", 1:5))
  )
  sparse <- methods::as(Matrix::Matrix(dense, sparse = TRUE), "dgCMatrix")
  iterable <- BPCells::write_matrix_dir(sparse, tempfile())
  source_indices <- c(11L, 43L, 47L, 103L, 211L)

  set.seed(20260807)
  rng_before <- .Random.seed
  sparse_shell <- bench_make_seurat_shell(sparse, source_indices)
  expect_identical(.Random.seed, rng_before)
  iterable_shell <- bench_make_seurat_shell(iterable, source_indices)
  expect_identical(.Random.seed, rng_before)

  expect_identical(SeuratObject::Layers(sparse_shell[["RNA"]]), "data")
  expect_identical(SeuratObject::Layers(iterable_shell[["RNA"]]), "data")
  expect_s4_class(
    SeuratObject::LayerData(sparse_shell[["RNA"]], layer = "data"),
    "dgCMatrix"
  )
  expect_true(inherits(
    SeuratObject::LayerData(iterable_shell[["RNA"]], layer = "data"),
    "IterableMatrix"
  ))
  expect_identical(colnames(sparse_shell), colnames(iterable_shell))

  expected_sample <- factor(paste0("sample_", source_indices %% 8L + 1L))
  expected_cluster <- factor(paste0("cluster_", source_indices %% 32L + 1L))
  metadata <- sparse_shell[[]]
  expect_identical(metadata$sample, expected_sample)
  expect_identical(metadata$cluster, expected_cluster)
  expect_identical(metadata$nUMI, rep(0, 5L))
  expect_identical(metadata$nGene, rep(0, 5L))
  expected_embedding <- cbind(
    UMAP_1 = sin(source_indices / 1000),
    UMAP_2 = cos(source_indices / 1000)
  )
  rownames(expected_embedding) <- colnames(sparse)
  expect_equal(
    SeuratObject::Embeddings(sparse_shell[["umap"]]),
    expected_embedding
  )
  expect_identical(
    bench_shell_fingerprint(sparse_shell, source_indices),
    bench_shell_fingerprint(iterable_shell, source_indices)
  )
  extra_factor_level <- sparse_shell
  extra_factor_level$orig.ident <- factor(
    as.character(extra_factor_level$orig.ident),
    levels = c(levels(extra_factor_level$orig.ident), "another_project")
  )
  expect_false(identical(
    bench_shell_fingerprint(extra_factor_level, source_indices),
    bench_shell_fingerprint(sparse_shell, source_indices)
  ))
  reordered_metadata <- sparse_shell
  reordered_metadata@meta.data <- reordered_metadata@meta.data[
    c(2L, 1L, 3L, 4L, 5L),
    ,
    drop = FALSE
  ]
  expect_error(
    bench_shell_fingerprint(reordered_metadata, source_indices),
    "metadata row identities"
  )

  changed_values <- sparse
  changed_values[1L, 1L] <- 999
  expect_identical(
    bench_shell_fingerprint(
      bench_make_seurat_shell(changed_values, source_indices),
      source_indices
    ),
    bench_shell_fingerprint(sparse_shell, source_indices)
  )
  shifted_indices <- source_indices + 1L
  expect_false(identical(
    bench_shell_fingerprint(
      bench_make_seurat_shell(sparse, shifted_indices),
      shifted_indices
    ),
    bench_shell_fingerprint(sparse_shell, source_indices)
  ))
  reversed <- rev(seq_len(ncol(sparse)))
  expect_false(identical(
    bench_shell_fingerprint(
      bench_make_seurat_shell(sparse[, reversed], source_indices[reversed]),
      source_indices[reversed]
    ),
    bench_shell_fingerprint(sparse_shell, source_indices)
  ))
})

test_that("synthetic shells reject incomplete identities and invalid source indices", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("SeuratObject")
  load_benchmark_contract()

  expression <- methods::as(
    Matrix::Matrix(matrix(1:20, 4L, 5L), sparse = TRUE),
    "dgCMatrix"
  )
  dimnames(expression) <- list(paste0("g", 1:4), paste0("c", 1:5))
  expect_error(bench_make_seurat_shell(expression, 1:4), "length")
  expect_error(bench_make_seurat_shell(expression, c(1, 2, 2, 4, 5)), "unique")
  expect_error(bench_make_seurat_shell(expression, c(1, 2, 3, 4, NA)), "finite")
  unnamed <- expression
  colnames(unnamed) <- NULL
  expect_error(bench_make_seurat_shell(unnamed, 1:5), "identit")
  duplicated <- expression
  colnames(duplicated)[[5L]] <- colnames(duplicated)[[1L]]
  expect_error(bench_make_seurat_shell(duplicated, 1:5), "duplicate")
})

test_that("query measurement times exactly six rows then lazy preparation and materialization", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("Matrix")
  load_benchmark_contract()

  timed_value <- bench_timed_value(42L)
  expect_identical(timed_value$value, 42L)
  expect_true(
    is.numeric(timed_value$seconds) &&
      length(timed_value$seconds) == 1L &&
      is.finite(timed_value$seconds) &&
      timed_value$seconds >= 0
  )

  expression <- methods::as(
    Matrix::Matrix(matrix(seq_len(25), 5L, 5L), sparse = TRUE),
    "dgCMatrix"
  )
  dimnames(expression) <- list(paste0("g", 1:5), paste0("c", 1:5))
  plan <- list(
    genes = data.frame(
      gene = paste0("g", 1:5),
      role = c("first", rep("block", 4L))
    ),
    first_row_numeric_sha256 = bench_numeric_fingerprint(
      as.matrix(expression[1L, , drop = FALSE]),
      "g1",
      colnames(expression)
    ),
    block_numeric_sha256 = bench_numeric_fingerprint(
      as.matrix(expression),
      rownames(expression),
      colnames(expression)
    )
  )
  object <- Cerebro_v1.3$new()
  object$expression <- expression
  calls <- list()
  timer <- function(expr) {
    value <- force(expr)
    calls[[length(calls) + 1L]] <<- value
    list(seconds = length(calls) / 10, value = value)
  }
  original_fingerprint <- bench_numeric_fingerprint
  fingerprint_call_counts <- integer()
  on.exit(
    assign(
      "bench_numeric_fingerprint",
      original_fingerprint,
      envir = environment()
    ),
    add = TRUE
  )
  assign(
    "bench_numeric_fingerprint",
    function(...) {
      fingerprint_call_counts <<- c(fingerprint_call_counts, length(calls))
      original_fingerprint(...)
    },
    envir = environment()
  )

  measurement <- bench_measure_queries(object, plan, timer = timer)
  expect_length(calls, 8L)
  expect_true(all(vapply(calls[1:6], is.numeric, logical(1))))
  expect_true(all(vapply(calls[1:6], identical, logical(1), calls[[1L]])))
  expect_s4_class(calls[[7L]], "dgCMatrix")
  expect_true(is.matrix(calls[[8L]]))
  expect_identical(fingerprint_call_counts, c(8L, 8L))
  expect_identical(measurement$first_query_secs, 0.1)
  expect_identical(measurement$warmed_secs, (2:6) / 10)
  expect_identical(measurement$warmed_median_secs, 0.4)
  expect_identical(measurement$block_prepare_secs, 0.7)
  expect_identical(measurement$block_materialize_secs, 0.8)
  expect_identical(measurement$block_ready_secs, 1.5)
  expect_false("block_secs" %in% names(measurement))
  expect_silent(bench_validate_query_measurement(measurement, plan))

  invalid_scalar_timings <- list(
    "0.1",
    NA_real_,
    -Inf,
    list(0.1),
    -0.1,
    c(0.1, 0.2)
  )
  for (value in invalid_scalar_timings) {
    invalid <- measurement
    invalid$first_query_secs <- value
    expect_error(bench_validate_query_measurement(invalid, plan), "timing")
  }
  invalid_warmed_timings <- list(
    as.character(measurement$warmed_secs),
    replace(measurement$warmed_secs, 1L, NA_real_),
    replace(measurement$warmed_secs, 1L, -Inf),
    as.list(measurement$warmed_secs),
    replace(measurement$warmed_secs, 1L, -0.1),
    measurement$warmed_secs[-1L]
  )
  for (value in invalid_warmed_timings) {
    invalid <- measurement
    invalid$warmed_secs <- value
    expect_error(bench_validate_query_measurement(invalid, plan), "warmed_secs")
  }
  invalid_median <- measurement
  invalid_median$warmed_median_secs <- invalid_median$warmed_median_secs + 0.01
  expect_error(bench_validate_query_measurement(invalid_median, plan), "median")
  invalid_ready <- measurement
  invalid_ready$block_ready_secs <- invalid_ready$block_ready_secs + 0.01
  expect_error(
    bench_validate_query_measurement(invalid_ready, plan),
    "block_ready"
  )
  duplicate_schema <- measurement
  names(duplicate_schema)[[2L]] <- names(duplicate_schema)[[1L]]
  expect_error(
    bench_validate_query_measurement(duplicate_schema, plan),
    "schema"
  )

  duplicate_timer <- function(expr) {
    value <- force(expr)
    structure(list(0, value), names = c("seconds", "seconds"))
  }
  expect_error(
    bench_measure_queries(object, plan, timer = duplicate_timer),
    "timer"
  )

  wrong_row <- plan
  wrong_row$first_row_numeric_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    bench_validate_query_measurement(measurement, wrong_row),
    "row fingerprint"
  )
  wrong_block <- plan
  wrong_block$block_numeric_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    bench_validate_query_measurement(measurement, wrong_block),
    "block fingerprint"
  )
})

test_that("query measurement preserves BPCells block laziness until the eighth timed call", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("BPCells")
  load_benchmark_contract()

  matrix <- methods::as(
    Matrix::Matrix(matrix(seq_len(25), 5L, 5L), sparse = TRUE),
    "dgCMatrix"
  )
  dimnames(matrix) <- list(paste0("g", 1:5), paste0("c", 1:5))
  expression <- BPCells::write_matrix_dir(matrix, tempfile())
  object <- Cerebro_v1.3$new()
  object$expression <- expression
  plan <- list(
    genes = data.frame(
      gene = paste0("g", 1:5),
      role = c("first", rep("block", 4L))
    ),
    first_row_numeric_sha256 = bench_numeric_fingerprint(
      as.matrix(matrix[1L, , drop = FALSE]),
      "g1",
      colnames(matrix)
    ),
    block_numeric_sha256 = bench_numeric_fingerprint(
      as.matrix(matrix),
      rownames(matrix),
      colnames(matrix)
    )
  )
  classes <- list()
  is_iterable <- logical()
  timer <- function(expr) {
    value <- force(expr)
    classes[[length(classes) + 1L]] <<- class(value)
    is_iterable[[length(is_iterable) + 1L]] <<- inherits(
      value,
      "IterableMatrix"
    )
    list(seconds = 0, value = value)
  }
  measurement <- bench_measure_queries(object, plan, timer = timer)
  expect_true(is_iterable[[7L]])
  expect_identical(classes[[8L]][[1L]], "matrix")
  expect_silent(bench_validate_query_measurement(measurement, plan))
})

test_that("worker scratch stages are atomic and cleanup is fail closed", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  scratch <- tempfile("bench-scratch-")
  dir.create(scratch)
  writeLines(
    "bench-scratch-v1",
    file.path(scratch, ".cerebro-benchmark-scratch")
  )
  job <- bench_make_job_dir(scratch, "comparison-1")
  expect_identical(bench_read_stage(job), "startup")
  bench_write_stage(job, "seurat_shell")
  expect_identical(bench_read_stage(job), "seurat_shell")
  expect_error(bench_write_stage(job, "bogus"), "stage")
  expect_silent(bench_remove_job_dir(job, scratch))
  expect_false(dir.exists(job))

  unmarked <- file.path(scratch, "unmarked")
  dir.create(unmarked)
  expect_error(bench_remove_job_dir(unmarked, scratch), "marker")
  outside <- tempfile("outside-")
  dir.create(outside)
  writeLines("outside", file.path(outside, ".cerebro-benchmark-job"))
  expect_error(bench_remove_job_dir(outside, scratch), "scratch")
  prefix_root <- paste0(scratch, "-evil")
  dir.create(prefix_root)
  prefix_job <- file.path(prefix_root, "prefix-job")
  dir.create(prefix_job)
  writeLines("prefix-job", file.path(prefix_job, ".cerebro-benchmark-job"))
  expect_error(bench_remove_job_dir(prefix_job, scratch), "exact child")

  real_job <- bench_make_job_dir(scratch, "real-job")
  linked_job <- file.path(scratch, "linked-job")
  if (file.symlink(real_job, linked_job)) {
    expect_error(bench_remove_job_dir(linked_job, scratch), "symlink")
    unlink(linked_job)
  }
  marker_job <- bench_make_job_dir(scratch, "marker-link")
  marker <- file.path(marker_job, ".cerebro-benchmark-job")
  marker_target <- file.path(scratch, "marker-target")
  writeLines("marker-link", marker_target)
  unlink(marker)
  if (file.symlink(marker_target, marker)) {
    expect_error(bench_remove_job_dir(marker_job, scratch), "marker")
    unlink(marker)
    writeLines("marker-link", marker)
  }
  bench_remove_job_dir(real_job, scratch)
  bench_remove_job_dir(marker_job, scratch)
  expect_error(bench_make_job_dir(scratch, "../escape"), "job_id")
})

test_that("heap, RSS, and atomic outcome helpers preserve typed failure data", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  heap <- bench_r_heap_peak_mb()
  expect_true(is.double(heap) && length(heap) == 1L && is.finite(heap))
  bad_gc <- gc()
  colnames(bad_gc)[ncol(bad_gc) - 1L] <- "not max"
  expect_error(.bench_r_heap_peak_from_gc(bad_gc), "max used")
  bad_gc[1L, ncol(bad_gc)] <- NA_real_
  colnames(bad_gc)[ncol(bad_gc) - 1L] <- "max used"
  expect_error(.bench_r_heap_peak_from_gc(bad_gc), "finite")
  original_heap <- bench_r_heap_peak_mb
  on.exit(
    assign("bench_r_heap_peak_mb", original_heap, envir = environment()),
    add = TRUE
  )
  assign(
    "bench_r_heap_peak_mb",
    function() stop("gc broken"),
    envir = environment()
  )
  expect_identical(bench_safe_r_heap_peak_mb(), NA_real_)
  expect_true(is.double(bench_rss_mb(Sys.getpid())))
  expect_identical(bench_rss_mb(-1L), NA_real_)
  expect_identical(bench_rss_mb(.Machine$integer.max), NA_real_)
  expect_identical(.bench_update_peak_rss(NA_real_, NA_real_), NA_real_)
  expect_identical(.bench_update_peak_rss(NA_real_, 0), 0)
  expect_identical(.bench_update_peak_rss(0, NA_real_), 0)

  schema <- bench_export_schema()
  row <- bench_empty_outcome("export", "pair:1")
  row$status <- "FAILED_startup"
  row$failure_stage <- "startup"
  row$error <- "fixture"
  path <- tempfile(fileext = ".rds")
  expect_silent(bench_write_outcome_atomic(path, row, schema))
  expect_identical(readRDS(path), row)
  expect_error(
    bench_write_outcome_atomic(path, rbind(row, row), schema),
    "one row"
  )
  expect_error(
    bench_write_outcome_atomic(path, row[names(row)[-1L]], schema),
    "schema"
  )
  expect_true(all(
    c(
      "source_open_secs",
      "comparison_materialize_secs",
      "seurat_shell_secs",
      "export_secs",
      "crb_bytes",
      "sidecar_bytes",
      "total_bytes",
      "sidecar_bytes_applicable",
      "sidecar_bytes_reason"
    ) %in%
      bench_export_schema()
  ))
  expect_true(all(
    c(
      "crb_load_secs",
      "backend_attach_secs",
      "backend_attach_applicable",
      paste0("warmed_query_", 1:5, "_secs"),
      "warmed_median_secs",
      "expected_row_fingerprint",
      "observed_row_fingerprint",
      "expected_block_fingerprint",
      "observed_block_fingerprint",
      "correctness"
    ) %in%
      bench_access_schema()
  ))
  not_run <- bench_not_run_access_row(
    list(pair_id = "pair:not-run", artifact_path = "/tmp/not-created.crb"),
    "export failed"
  )
  expect_identical(not_run$status, "NOT_RUN_EXPORT_FAILED")
  expect_silent(.bench_validate_outcome_row(
    not_run,
    bench_access_schema(),
    "pair:not-run"
  ))
  expect_error(
    .bench_validate_worker_payload(list(matrix = matrix(1)), "job"),
    "paths, scalars"
  )
})

test_that("run-local package origin uses component boundaries before scientific work", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  root <- tempfile("run-lib-")
  library <- file.path(root, "library")
  package <- file.path(library, "CerebroNexus")
  collision <- file.path(root, "library-evil", "CerebroNexus")
  dir.create(package, recursive = TRUE)
  dir.create(collision, recursive = TRUE)
  writeLines("run-library-v1", file.path(library, ".cerebro-benchmark-library"))
  expect_identical(
    .bench_validate_package_origin(list(library = library), package),
    normalizePath(package)
  )
  scientific_calls <- 0L
  expect_error(
    {
      .bench_validate_package_origin(list(library = library), collision)
      scientific_calls <- scientific_calls + 1L
    },
    "outside"
  )
  expect_identical(scientific_calls, 0L)

  scratch <- tempfile("origin-worker-")
  dir.create(scratch)
  writeLines(
    "bench-scratch-v1",
    file.path(scratch, ".cerebro-benchmark-scratch")
  )
  job <- list(
    pair_id = "pair:origin",
    phase = "export",
    job_dir = bench_make_job_dir(scratch, "origin-job")
  )
  original_origin <- .bench_validate_package_origin
  original_worker <- bench_export_worker
  on.exit(
    assign(
      ".bench_validate_package_origin",
      original_origin,
      envir = environment()
    ),
    add = TRUE
  )
  on.exit(
    assign("bench_export_worker", original_worker, envir = environment()),
    add = TRUE
  )
  assign(
    ".bench_validate_package_origin",
    function(...) stop("origin prefix collision"),
    envir = environment()
  )
  assign(
    "bench_export_worker",
    function(...) {
      scientific_calls <<- scientific_calls + 1L
      bench_empty_outcome("export", "pair:origin")
    },
    envir = environment()
  )
  origin_failure <- bench_worker_entry("bench_export_worker", job, list())
  expect_identical(scientific_calls, 0L)
  expect_identical(origin_failure$status, "FAILED_startup")
  expect_match(origin_failure$error, "origin prefix collision")
})

test_that("worker entry returns exactly one fixed-schema row after ordinary errors", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  scratch <- tempfile("bench-scratch-")
  dir.create(scratch)
  writeLines(
    "bench-scratch-v1",
    file.path(scratch, ".cerebro-benchmark-scratch")
  )
  job_dir <- bench_make_job_dir(scratch, "job-ordinary-error")
  job <- list(pair_id = "pair:error", phase = "export", job_dir = job_dir)
  on.exit(rm(".bench_ordinary_error_worker", envir = environment()), add = TRUE)
  assign(
    ".bench_ordinary_error_worker",
    function(job, run_context) {
      bench_write_stage(job$job_dir, "seurat_shell")
      stop("synthetic ordinary error")
    },
    envir = environment()
  )
  row <- bench_worker_entry(".bench_ordinary_error_worker", job, list())
  expect_identical(nrow(row), 1L)
  expect_identical(names(row), bench_export_schema())
  expect_identical(row$status, "FAILED_seurat_shell")
  expect_identical(row$failure_stage, "seurat_shell")
  expect_match(row$error, "synthetic ordinary error")
})

test_that("stage-aware query timing writes eight lifecycle stages before correctness", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("Matrix")
  load_benchmark_contract()
  expression <- methods::as(
    Matrix::Matrix(matrix(seq_len(25), 5L, 5L), sparse = TRUE),
    "dgCMatrix"
  )
  dimnames(expression) <- list(paste0("g", 1:5), paste0("c", 1:5))
  object <- Cerebro_v1.3$new()
  object$expression <- expression
  plan <- list(
    genes = data.frame(
      gene = paste0("g", 1:5),
      role = c("first", rep("block", 4L))
    ),
    first_row_numeric_sha256 = bench_numeric_fingerprint(
      as.matrix(expression[1L, , drop = FALSE]),
      "g1",
      colnames(expression)
    ),
    block_numeric_sha256 = bench_numeric_fingerprint(
      as.matrix(expression),
      rownames(expression),
      colnames(expression)
    )
  )
  stages <- character()
  timer <- bench_stage_timer(
    function(stage) stages <<- c(stages, stage),
    function(expr) list(seconds = 0, value = force(expr))
  )
  measurement <- bench_measure_queries(object, plan, timer = timer)
  expect_identical(
    stages,
    c(
      "first_query",
      rep("warmed_queries", 5L),
      "block_prepare",
      "block_materialize"
    )
  )
  bench_validate_query_measurement(measurement, plan)
  expect_error(
    bench_validate_query_measurement(
      within(
        measurement,
        row_fingerprint <- paste(rep("0", 64), collapse = "")
      ),
      plan
    ),
    "row fingerprint"
  )
})

test_that("worker result classification distinguishes crash from collector diagnostics", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  job <- list(pair_id = "pair:classify", phase = "export")
  crashed <- bench_classify_worker_result(
    job,
    list(ok = FALSE, error = "no result"),
    9L,
    "export",
    12.5,
    "job.log"
  )
  expect_identical(crashed$row$status, "FAILED_export")
  expect_identical(crashed$row$exit_status, 9L)
  expect_identical(crashed$row$peak_rss_mb, 12.5)
  malformed <- bench_classify_worker_result(
    job,
    list(ok = TRUE, value = data.frame(x = 1)),
    0L,
    "complete",
    NA_real_,
    "job.log"
  )
  expect_null(malformed$row)
  expect_match(malformed$diagnostic, "schema")
  wrong_pair <- bench_empty_outcome("export", "other")
  mismatch <- bench_classify_worker_result(
    job,
    list(ok = TRUE, value = wrong_pair),
    0L,
    "complete",
    NA_real_,
    "job.log"
  )
  expect_null(mismatch$row)
  expect_match(mismatch$diagnostic, "pair_id")
  two <- rbind(
    bench_empty_outcome("export", job$pair_id),
    bench_empty_outcome("export", job$pair_id)
  )
  duplicate <- bench_classify_worker_result(
    job,
    list(ok = TRUE, value = two),
    0L,
    "complete",
    NA_real_,
    "job.log"
  )
  expect_null(duplicate$row)
  expect_match(duplicate$diagnostic, "one row")
  zero <- bench_empty_outcome("export", job$pair_id)[FALSE, ]
  absent <- bench_classify_worker_result(
    job,
    list(ok = TRUE, value = zero),
    0L,
    "complete",
    NA_real_,
    "job.log"
  )
  expect_null(absent$row)
  expect_match(absent$diagnostic, "one row")
})

test_that("measured orchestration preserves parent collector diagnostics for NULL rows", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  schedule <- data.frame(
    pair_id = "comparison:1:tiny:embedded",
    panel = "comparison",
    `repeat` = 1L,
    tier_label = "tiny",
    n_cells = 1L,
    backend = "embedded",
    export_order = 1L,
    access_order = 1L,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  make_paths <- function() {
    root <- tempfile("collector-orchestration-")
    dir.create(root)
    scratch <- file.path(root, "scratch")
    logs <- file.path(root, "logs")
    dir.create(scratch)
    dir.create(logs)
    writeLines(
      "bench-scratch-v1",
      file.path(scratch, ".cerebro-benchmark-scratch")
    )
    list(output = root, scratch = scratch, logs = logs)
  }
  helper_environment <- environment(.bench_run_measured_schedule)
  original_runner <- get("bench_run_worker", envir = helper_environment)
  on.exit(
    assign("bench_run_worker", original_runner, envir = helper_environment),
    add = TRUE
  )

  export_paths <- make_paths()
  assign(
    "bench_run_worker",
    function(worker, job, run_context, log_path, poll_ms) {
      writeLines("child export log", log_path)
      list(
        row = NULL,
        diagnostic = "export result had wrong pair_id",
        exit_status = 0L,
        log_path = log_path,
        peak_rss_mb = 1
      )
    },
    envir = helper_environment
  )
  export_result <- .bench_run_measured_schedule(
    schedule,
    list(tiny = list()),
    "unused.h5ad",
    export_paths,
    list(bench_root = bench_root, library = tempdir())
  )
  expect_identical(nrow(export_result$exports), 0L)
  expect_identical(export_result$access$status, "NOT_RUN_EXPORT_FAILED")
  export_log <- readLines(
    file.path(export_paths$logs, "comparison_1_tiny_embedded-export.log"),
    warn = FALSE
  )
  expect_true(any(export_log == "=== PARENT COLLECTOR DIAGNOSTIC ==="))
  expect_true(any(grepl(
    "pair_id: comparison:1:tiny:embedded",
    export_log,
    fixed = TRUE
  )))
  expect_true(any(grepl("exit_status: 0", export_log, fixed = TRUE)))
  expect_true(any(grepl(
    "export result had wrong pair_id",
    export_log,
    fixed = TRUE
  )))
  fingerprints <- data.frame(
    pair_id = schedule$pair_id,
    expected_row_fingerprint = paste(rep("a", 64), collapse = ""),
    expected_block_fingerprint = paste(rep("b", 64), collapse = ""),
    stringsAsFactors = FALSE
  )
  export_validation <- bench_raw_outcome_gate(
    schedule,
    export_result$exports,
    export_result$access,
    fingerprints,
    "comparison"
  )
  expect_identical(
    export_validation$status[
      export_validation$check_id == paste0("export_result:", schedule$pair_id)
    ],
    "MISSING_RESULT"
  )

  access_paths <- make_paths()
  calls <- 0L
  assign(
    "bench_run_worker",
    function(worker, job, run_context, log_path, poll_ms) {
      calls <<- calls + 1L
      writeLines(
        if (calls == 1L) "child export log" else "child access log",
        log_path
      )
      if (calls == 1L) {
        row <- bench_empty_outcome("export", job$pair_id)
        row$status <- "OK"
        return(list(row = row, diagnostic = NA_character_))
      }
      list(
        row = NULL,
        diagnostic = "access result contained duplicate rows",
        exit_status = 0L,
        log_path = log_path,
        peak_rss_mb = 1
      )
    },
    envir = helper_environment
  )
  access_result <- .bench_run_measured_schedule(
    schedule,
    list(tiny = list()),
    "unused.h5ad",
    access_paths,
    list(bench_root = bench_root, library = tempdir())
  )
  expect_identical(nrow(access_result$exports), 1L)
  expect_identical(nrow(access_result$access), 0L)
  access_log <- readLines(
    file.path(access_paths$logs, "comparison_1_tiny_embedded-access.log"),
    warn = FALSE
  )
  expect_true(any(access_log == "=== PARENT COLLECTOR DIAGNOSTIC ==="))
  expect_true(any(grepl(
    "pair_id: comparison:1:tiny:embedded",
    access_log,
    fixed = TRUE
  )))
  expect_true(any(grepl("exit_status: 0", access_log, fixed = TRUE)))
  expect_true(any(grepl(
    "access result contained duplicate rows",
    access_log,
    fixed = TRUE
  )))
  access_validation <- bench_raw_outcome_gate(
    schedule,
    access_result$exports,
    access_result$access,
    fingerprints,
    "comparison"
  )
  expect_identical(
    access_validation$status[
      access_validation$check_id == paste0("access_result:", schedule$pair_id)
    ],
    "MISSING_RESULT"
  )
})

test_that("parent collector diagnostic rejects symlinked log roots before normalization", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  root <- tempfile("collector-symlink-")
  external <- tempfile("collector-external-")
  dir.create(root)
  dir.create(external)
  result <- list(row = NULL, diagnostic = "must not escape", exit_status = 0L)

  linked_root <- file.path(root, "logs-link")
  expect_true(file.symlink(external, linked_root))
  escaped <- file.path(external, "escape.log")
  expect_false(suppressMessages(.bench_append_parent_collector_diagnostic(
    result,
    "comparison:1:tiny:embedded",
    file.path(linked_root, "escape.log"),
    linked_root
  )))
  expect_false(file.exists(escaped))

  real_logs <- file.path(root, "real-logs")
  dir.create(real_logs)
  linked_parent <- file.path(root, "parent-link")
  expect_true(file.symlink(real_logs, linked_parent))
  protected <- file.path(real_logs, "escape.log")
  writeLines("preserve me", protected)
  expect_false(suppressMessages(.bench_append_parent_collector_diagnostic(
    result,
    "comparison:1:tiny:embedded",
    file.path(linked_parent, "escape.log"),
    real_logs
  )))
  expect_identical(readLines(protected, warn = FALSE), "preserve me")
})

test_that("monitored workers preserve success, ordinary failure, and killed-worker evidence", {
  skip_unless_bench_tree()
  skip_if_not_installed("callr")
  load_benchmark_contract()
  root <- tempfile("worker-fixture-")
  dir.create(root)
  file.copy(file.path(bench_root, "helpers.R"), file.path(root, "helpers.R"))
  fixture <- c(
    ".bench_success_worker <- function(job, run_context) {",
    "  bench_write_stage(job$job_dir, 'complete')",
    "  row <- bench_empty_outcome('export', job$pair_id); row$status <- 'OK'; row",
    "}",
    ".bench_error_worker <- function(job, run_context) {",
    "  bench_write_stage(job$job_dir, 'seurat_shell'); stop('ordinary fixture failure')",
    "}",
    ".bench_kill_worker <- function(job, run_context) {",
    "  bench_write_stage(job$job_dir, 'export'); Sys.sleep(0.25)",
    "  tools::pskill(Sys.getpid(), signal = 9L); Sys.sleep(10)",
    "}"
  )
  write(fixture, file.path(root, "helpers.R"), append = TRUE)
  scratch <- file.path(root, "scratch")
  dir.create(scratch)
  writeLines(
    "bench-scratch-v1",
    file.path(scratch, ".cerebro-benchmark-scratch")
  )
  context <- list(bench_root = root, library = .libPaths()[[1L]])
  run_case <- function(id, worker) {
    job_dir <- bench_make_job_dir(scratch, id)
    job <- list(
      pair_id = paste0("pair:", id),
      phase = "export",
      job_dir = job_dir
    )
    bench_run_worker(
      worker,
      job,
      context,
      file.path(root, paste0(id, ".log")),
      10L
    )
  }
  success <- run_case("success", ".bench_success_worker")
  expect_identical(success$row$status, "OK")
  expect_identical(success$row$exit_status, 0L)
  ordinary <- run_case("ordinary", ".bench_error_worker")
  expect_identical(ordinary$row$status, "FAILED_seurat_shell")
  expect_identical(ordinary$row$exit_status, 0L)
  killed <- run_case("killed", ".bench_kill_worker")
  expect_identical(killed$row$status, "FAILED_export")
  expect_true(!is.na(killed$row$exit_status) && killed$row$exit_status != 0L)
  expect_true(file.exists(killed$row$log_path))
  expect_true(is.finite(killed$row$peak_rss_mb))
})

test_that("raw outcome gate has fixed types and fails every control-plane mutation", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  schedule <- data.frame(
    pair_id = c(
      "comparison:1:tier_125k:embedded",
      "comparison:1:tier_125k:bpcells"
    ),
    panel = "comparison",
    stringsAsFactors = FALSE
  )
  exports <- do.call(
    rbind,
    lapply(schedule$pair_id, function(id) {
      make_valid_benchmark_outcome("export", id, bench_empty_outcome)
    })
  )
  accesses <- do.call(
    rbind,
    lapply(schedule$pair_id, function(id) {
      make_valid_benchmark_outcome("access", id, bench_empty_outcome)
    })
  )
  fingerprints <- data.frame(
    pair_id = schedule$pair_id,
    expected_row_fingerprint = accesses$expected_row_fingerprint,
    expected_block_fingerprint = accesses$expected_block_fingerprint,
    stringsAsFactors = FALSE
  )
  empty <- bench_empty_validation()
  expect_identical(names(empty), bench_validation_schema())
  expect_identical(nrow(empty), 0L)
  expect_true(all(vapply(empty, typeof, character(1L)) == "character"))

  valid <- bench_raw_outcome_gate(
    schedule,
    exports,
    accesses,
    fingerprints,
    "comparison"
  )
  expect_identical(tail(valid$check_id, 1L), "panel_valid")
  expect_identical(tail(valid$status, 1L), "VALID")
  expect_identical(sum(valid$check_id == "panel_valid"), 1L)
  expect_true(all(valid$status[-nrow(valid)] == "PASS"))

  missing_ps_sample_exports <- exports
  missing_ps_sample_accesses <- accesses
  missing_ps_sample_exports$peak_rss_mb[[1L]] <- NA_real_
  missing_ps_sample_accesses$peak_rss_mb[[2L]] <- NA_real_
  valid_without_ps_sample <- bench_raw_outcome_gate(
    schedule,
    missing_ps_sample_exports,
    missing_ps_sample_accesses,
    fingerprints,
    "comparison"
  )
  expect_identical(tail(valid_without_ps_sample$status, 1L), "VALID")
  expect_true(all(
    valid_without_ps_sample$status[-nrow(valid_without_ps_sample)] == "PASS"
  ))

  assert_invalid <- function(
    export_rows = exports,
    access_rows = accesses,
    expected = fingerprints,
    expected_status = NULL
  ) {
    result <- bench_raw_outcome_gate(
      schedule,
      export_rows,
      access_rows,
      expected,
      "comparison"
    )
    expect_identical(tail(result$check_id, 1L), "panel_valid")
    expect_identical(tail(result$status, 1L), "INVALID")
    expect_identical(sum(result$check_id == "panel_valid"), 1L)
    if (!is.null(expected_status)) {
      expect_true(expected_status %in% result$status)
    }
  }
  assert_invalid(exports[-1L, ], accesses, expected_status = "MISSING_RESULT")
  assert_invalid(rbind(exports, exports[1L, ]), accesses)
  unscheduled_export <- exports
  unscheduled_export$pair_id[[1L]] <- "other"
  assert_invalid(unscheduled_export, accesses)
  failed_export <- exports
  failed_export$status[[1L]] <- "FAILED_export"
  failed_export$failure_stage[[1L]] <- "export"
  failed_export$error[[1L]] <- "x"
  assert_invalid(failed_export, accesses)
  not_run <- accesses
  not_run$status[[1L]] <- "NOT_RUN_EXPORT_FAILED"
  not_run$correctness[[1L]] <- NA_character_
  assert_invalid(exports, not_run)
  mismatch <- accesses
  mismatch$observed_row_fingerprint[[1L]] <- "wrong"
  assert_invalid(exports, mismatch)
  wrong_block <- accesses
  wrong_block$observed_block_fingerprint[[2L]] <- "wrong"
  assert_invalid(exports, wrong_block)
  wrong_correctness <- accesses
  wrong_correctness$correctness[[1L]] <- "FAIL"
  assert_invalid(exports, wrong_correctness)
  wrong_type <- exports
  wrong_type$peak_rss_mb <- as.integer(c(1, 2))
  assert_invalid(wrong_type, accesses)
  ok_with_error <- exports
  ok_with_error$error[[1L]] <- "should be empty"
  assert_invalid(ok_with_error, accesses)
  failed_without_error <- failed_export
  failed_without_error$error[[1L]] <- NA_character_
  assert_invalid(failed_without_error, accesses)
  failed_wrong_stage <- failed_export
  failed_wrong_stage$failure_stage[[1L]] <- "source_open"
  assert_invalid(failed_wrong_stage, accesses)
  illegal_not_run <- not_run
  illegal_not_run$correctness[[1L]] <- "PASS"
  assert_invalid(exports, illegal_not_run)
  for (column in c(
    "exit_status",
    "log_path",
    "package_path",
    "artifact_path",
    "peak_rss_mb",
    "r_heap_peak_mb",
    "elapsed_secs",
    "source_open_secs",
    "comparison_materialize_secs",
    "seurat_shell_secs",
    "export_secs",
    "crb_bytes",
    "total_bytes",
    "shell_sha256"
  )) {
    mutated <- exports
    mutated[[column]][[1L]] <- if (column == "exit_status") {
      1L
    } else if (
      column %in% c("log_path", "package_path", "artifact_path", "shell_sha256")
    ) {
      NA_character_
    } else {
      -1
    }
    assert_invalid(mutated, accesses)
  }
  infinite_rss <- exports
  infinite_rss$peak_rss_mb[[1L]] <- Inf
  assert_invalid(infinite_rss, accesses)
  contradictory_sidecar <- exports
  contradictory_sidecar$sidecar_bytes_applicable[[1L]] <- TRUE
  assert_invalid(contradictory_sidecar, accesses)
  missing_sidecar <- exports
  missing_sidecar$sidecar_bytes[
    missing_sidecar$pair_id == schedule$pair_id[[2L]]
  ] <- NA_real_
  assert_invalid(missing_sidecar, accesses)
  for (column in c(
    "exit_status",
    "log_path",
    "package_path",
    "artifact_path",
    "peak_rss_mb",
    "r_heap_peak_mb",
    "elapsed_secs",
    "crb_load_secs",
    "first_query_secs",
    "warmed_query_1_secs",
    "warmed_median_secs",
    "block_prepare_secs",
    "block_materialize_secs",
    "block_ready_secs",
    "expected_row_fingerprint",
    "observed_block_fingerprint"
  )) {
    mutated <- accesses
    mutated[[column]][[1L]] <- if (column == "exit_status") {
      1L
    } else if (
      column %in%
        c(
          "log_path",
          "package_path",
          "artifact_path",
          "expected_row_fingerprint",
          "observed_block_fingerprint"
        )
    ) {
      NA_character_
    } else {
      -1
    }
    assert_invalid(exports, mutated)
  }
  contradictory_attach <- accesses
  contradictory_attach$backend_attach_applicable[[1L]] <- TRUE
  assert_invalid(exports, contradictory_attach)
  empty_exports <- exports[FALSE, ]
  empty_exports$peak_rss_mb <- integer()
  assert_invalid(empty_exports, accesses, expected_status = "MISSING_RESULT")
  expect_false(any(exports$status == "MISSING_RESULT"))
  expect_false(any(accesses$status == "MISSING_RESULT"))
})

test_that("runtime helper loading is outside attach timing and descriptor mismatch fails closed", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  package_path <- tempfile("runtime-package-")
  dir.create(file.path(package_path, "viewer"), recursive = TRUE)
  file.copy(
    testthat::test_path("../../inst/viewer/utility_functions.R"),
    file.path(package_path, "viewer", "utility_functions.R")
  )
  runtime <- .bench_load_runtime_helpers(package_path)
  expect_true(is.function(runtime$.readRuntimeBackendDescriptor))
  expect_true(is.function(runtime$.attachExternalExpression))

  object <- Cerebro_v1.3$new()
  object$setExpressionBackend("embedded")
  crb <- tempfile(fileext = ".crb")
  saveRDS(object, crb)
  expect_error(
    .bench_validate_artifact_backend(runtime, readRDS(crb), crb, "bpcells"),
    "does not match"
  )
  expect_identical(
    .bench_validate_artifact_backend(
      runtime,
      readRDS(crb),
      crb,
      "embedded"
    )$type,
    "embedded"
  )

  calls <- character()
  fake_runtime <- new.env(parent = emptyenv())
  fake_runtime$.attachExternalExpression <- function(object, path) {
    calls <<- c(calls, "attach")
    object
  }
  timer <- function(expr) {
    calls <<- c(calls, "timer_start")
    value <- force(expr)
    calls <<- c(calls, "timer_end")
    list(seconds = 0.25, value = value)
  }
  timed <- .bench_timed_external_attach(fake_runtime, object, crb, timer)
  expect_identical(calls, c("timer_start", "attach", "timer_end"))
  expect_identical(timed$seconds, 0.25)
})

test_that("real callr child enforces and records marked run-local package origin", {
  skip_unless_bench_tree()
  skip_if_not_installed("callr")
  load_benchmark_contract()
  fixture_root <- tempfile("origin-callr-")
  dir.create(fixture_root)
  proxy_root <- file.path(fixture_root, "bench")
  dir.create(proxy_root)
  file.copy(
    file.path(bench_root, "helpers.R"),
    file.path(proxy_root, "helpers.R")
  )
  proxy <- c(
    "bench_export_worker <- function(job, run_context) {",
    "  writeLines('scientific', job$sentinel_path)",
    "  row <- bench_empty_outcome('export', job$pair_id)",
    "  row$package_path <- normalizePath(find.package('CerebroNexus'))",
    "  row$status <- 'OK'; bench_write_stage(job$job_dir, 'complete'); row",
    "}"
  )
  write(proxy, file.path(proxy_root, "helpers.R"), append = TRUE)
  scratch <- file.path(fixture_root, "scratch")
  dir.create(scratch)
  writeLines(
    "bench-scratch-v1",
    file.path(scratch, ".cerebro-benchmark-scratch")
  )
  make_library <- function(name, copy_package) {
    library <- file.path(fixture_root, name)
    dir.create(library)
    writeLines(
      "run-library-v1",
      file.path(library, ".cerebro-benchmark-library")
    )
    if (copy_package) {
      package_target <- file.path(library, "CerebroNexus")
      dir.create(package_target)
      package_entries <- list.files(
        find.package("CerebroNexus"),
        all.files = TRUE,
        full.names = TRUE,
        no.. = TRUE
      )
      expect_true(all(file.copy(
        package_entries,
        package_target,
        recursive = TRUE
      )))
    }
    library
  }
  run_origin <- function(id, library) {
    sentinel <- file.path(fixture_root, paste0(id, ".sentinel"))
    job <- list(
      pair_id = paste0("pair:", id),
      phase = "export",
      job_dir = bench_make_job_dir(scratch, id),
      sentinel_path = sentinel
    )
    result <- bench_run_worker(
      "bench_export_worker",
      job,
      list(bench_root = proxy_root, library = library),
      file.path(fixture_root, paste0(id, ".log")),
      10L
    )
    list(result = result, sentinel = sentinel)
  }
  local <- run_origin("local", make_library("local-library", TRUE))
  expect_identical(local$result$row$status, "OK")
  expect_true(.bench_descendant(
    local$result$row$package_path,
    file.path(fixture_root, "local-library")
  ))
  expect_true(file.exists(local$sentinel))

  fallback <- run_origin("fallback", make_library("empty-library", FALSE))
  expect_identical(fallback$result$row$status, "FAILED_startup")
  expect_match(
    fallback$result$row$error,
    "outside the marked run-local library"
  )
  expect_false(file.exists(fallback$sentinel))
})

test_that("benchmark entry points expose only qualified positional CLIs and inert dry-runs", {
  skip_unless_bench_tree()
  comparison <- testthat::test_path("../bench/run_comparison.R")
  full <- testthat::test_path("../bench/run_full_scale.R")
  run <- function(script, args) {
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script), args),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(output, "status", exact = TRUE)
    list(
      output = paste(output, collapse = "\n"),
      status = if (is.null(status)) 0L else status
    )
  }
  a <- run(comparison, "--dry-run")
  expect_identical(a$status, 0L)
  expect_match(a$output, "63 technical pairs / 126 workers")
  expect_match(a$output, "UNQUALIFIED")
  b <- run(full, "--dry-run")
  expect_identical(b$status, 0L)
  expect_match(b$output, "16 technical pairs / 32 workers")
  expect_match(b$output, "4140453 x 4")
  expect_match(b$output, "UNQUALIFIED")
  malformed <- run(comparison, "only-one-argument")
  expect_false(identical(malformed$status, 0L))
})

test_that("output candidates fail closed before creating worktree or Panel A descendants", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  repo <- normalizePath(testthat::test_path("../.."))
  outside <- withr::local_tempdir()
  candidate <- file.path(outside, "new-run")
  expect_identical(
    bench_validate_output_candidate(candidate, repo),
    file.path(normalizePath(outside), "new-run")
  )
  expect_false(file.exists(candidate))
  expect_error(
    bench_validate_output_candidate(file.path(repo, "new-run"), repo),
    "outside the Git worktree"
  )
  panel_a <- file.path(outside, "panel-a")
  dir.create(panel_a)
  expect_error(
    bench_validate_output_candidate(
      file.path(panel_a, "panel-b"),
      repo,
      panel_a
    ),
    "not be nested"
  )
  nested <- file.path(outside, "new-parent", "run")
  validated_nested <- bench_validate_output_candidate(nested, repo)
  prepared <- bench_prepare_output(validated_nested)
  expect_identical(prepared$output, normalizePath(nested))
  expect_true(file.exists(file.path(nested, ".cerebro-benchmark-run")))
  expect_true(file.exists(file.path(
    nested,
    "scratch",
    ".cerebro-benchmark-scratch"
  )))
})

test_that("source snapshots separate lightweight identity checks from the final SHA proof", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()
  root <- withr::local_tempdir()
  source <- file.path(root, "source.h5ad")
  writeBin(charToRaw("AAAA"), source)
  bytes <- unname(file.info(source)$size)
  sha256 <- bench_sha256_file(source)

  snapshot <- .bench_source_snapshot(source, bytes, sha256)
  expect_identical(names(snapshot), c("path", "bytes", "sha256", "identity"))
  expect_silent(.bench_assert_source_identity(snapshot))
  expect_silent(.bench_assert_source_snapshot(snapshot))

  writeBin(charToRaw("BBBB"), source)
  expect_error(.bench_assert_source_identity(snapshot), "identity")
  identity_only_match <- snapshot
  identity_only_match$identity <- .bench_source_identity(source)
  expect_silent(.bench_assert_source_identity(identity_only_match))
  expect_error(.bench_assert_source_snapshot(identity_only_match), "SHA-256")

  link <- file.path(root, "source-link.h5ad")
  expect_true(file.symlink(source, link))
  expect_error(
    .bench_source_snapshot(link, bytes, bench_sha256_file(source)),
    "symlink"
  )
})

test_that("per-worker source guards never stream the complete source hash", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()
  root <- withr::local_tempdir()
  source <- file.path(root, "source.h5ad")
  writeBin(charToRaw("fixture"), source)
  snapshot <- .bench_source_snapshot(
    source,
    unname(file.info(source)$size),
    bench_sha256_file(source)
  )
  paths <- bench_prepare_output(file.path(root, "run"))
  schedule <- data.frame(
    pair_id = "comparison:1:tiny:embedded",
    panel = "comparison",
    `repeat` = 1L,
    tier_label = "tiny",
    n_cells = 1L,
    backend = "embedded",
    export_order = 1L,
    access_order = 1L,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  helper_environment <- environment(.bench_run_measured_schedule)
  original_runner <- get("bench_run_worker", envir = helper_environment)
  original_hash <- get("bench_sha256_file", envir = helper_environment)
  on.exit(
    {
      assign("bench_run_worker", original_runner, envir = helper_environment)
      assign("bench_sha256_file", original_hash, envir = helper_environment)
    },
    add = TRUE
  )
  assign(
    "bench_run_worker",
    function(worker, job, run_context, log_path, poll_ms) {
      row <- make_valid_benchmark_outcome(
        job$phase,
        job$pair_id,
        bench_empty_outcome
      )
      row$artifact_path <- job$artifact_path
      list(row = row, diagnostic = NA_character_)
    },
    envir = helper_environment
  )
  assign(
    "bench_sha256_file",
    function(path) {
      stop("complete source SHA must not run inside a per-worker guard")
    },
    envir = helper_environment
  )

  measured <- .bench_run_measured_schedule(
    schedule,
    list(tiny = list()),
    snapshot$path,
    paths,
    list(
      bench_root = bench_root,
      library = paths$library,
      source_snapshot = snapshot
    )
  )
  expect_identical(measured$exports$status, "OK")
  expect_identical(measured$access$status, "OK")
})

test_that("harness freezing closes the copy window and subprocess source window", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  skip_if_not_installed("callr")
  load_benchmark_contract()
  root <- withr::local_tempdir()
  harness_root <- file.path(root, "source-harness")
  dir.create(harness_root)
  writeLines(
    "BENCH_CONFIG <- list(schema_version = 1L)",
    file.path(harness_root, "config.R")
  )
  file.copy(
    file.path(bench_root, "helpers.R"),
    file.path(harness_root, "helpers.R")
  )
  write(
    c(
      ".bench_mutating_path <- Sys.getenv('BENCH_MUTATE_FROZEN_HELPER')",
      "if (nzchar(.bench_mutating_path)) cat('\\n# source-time mutation\\n', file = .bench_mutating_path, append = TRUE)",
      "rm(.bench_mutating_path)",
      ".bench_harness_fixture_worker <- function(job, run_context) {",
      "  row <- bench_empty_outcome('export', job$pair_id)",
      "  row$status <- 'OK'",
      "  bench_write_stage(job$job_dir, 'complete')",
      "  row",
      "}"
    ),
    file.path(harness_root, "helpers.R"),
    append = TRUE
  )

  prepared <- bench_prepare_output(file.path(root, "run"))
  frozen <- bench_freeze_harness(harness_root, prepared$scratch)
  frozen_helpers <- readLines(frozen$paths[["helpers.R"]], warn = FALSE)
  write(
    "# original changed after freeze",
    file.path(harness_root, "helpers.R"),
    append = TRUE
  )
  expect_identical(
    readLines(frozen$paths[["helpers.R"]], warn = FALSE),
    frozen_helpers
  )
  expect_silent(bench_assert_frozen_harness(frozen))

  withr::local_envvar(BENCH_MUTATE_FROZEN_HELPER = frozen$paths[["helpers.R"]])
  job_dir <- bench_make_job_dir(prepared$scratch, "source-window")
  result <- bench_run_worker(
    ".bench_harness_fixture_worker",
    list(
      pair_id = "comparison:1:tiny:embedded",
      phase = "export",
      job_dir = job_dir
    ),
    list(
      bench_root = frozen$root,
      library = prepared$library,
      output = prepared$output,
      harness_snapshot = frozen
    ),
    file.path(prepared$logs, "source-window.log"),
    10L
  )
  expect_false(!is.null(result$row) && identical(result$row$status, "OK"))

  hook_environment <- environment(bench_freeze_harness)
  expect_true(exists(
    ".bench_harness_after_copy",
    envir = hook_environment,
    inherits = FALSE
  ))
  original_hook <- get(".bench_harness_after_copy", envir = hook_environment)
  on.exit(
    assign(
      ".bench_harness_after_copy",
      original_hook,
      envir = hook_environment
    ),
    add = TRUE
  )
  assign(
    ".bench_harness_after_copy",
    function(source_paths, frozen_paths) {
      write(
        "# copy-window mutation",
        source_paths[["helpers.R"]],
        append = TRUE
      )
      invisible(NULL)
    },
    envir = hook_environment
  )
  prepared_race <- bench_prepare_output(file.path(root, "run-race"))
  expect_error(
    bench_freeze_harness(harness_root, prepared_race$scratch),
    "changed while freezing"
  )
})

test_that("post-worker control mutation retains raw evidence and writes INVALID", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()
  root <- withr::local_tempdir()
  paths <- bench_prepare_output(file.path(root, "run"))
  for (name in .bench_local_evidence_names) {
    writeLines(paste("frozen", name), file.path(paths$output, name))
  }
  snapshot <- .bench_snapshot_local_evidence(paths$output)
  schedule <- data.frame(
    pair_id = "comparison:1:tiny:embedded",
    panel = "comparison",
    `repeat` = 1L,
    tier_label = "tiny",
    n_cells = 1L,
    backend = "embedded",
    export_order = 1L,
    access_order = 1L,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  helper_environment <- environment(.bench_run_measured_schedule)
  original_runner <- get("bench_run_worker", envir = helper_environment)
  on.exit(
    assign("bench_run_worker", original_runner, envir = helper_environment),
    add = TRUE
  )
  assign(
    "bench_run_worker",
    function(worker, job, run_context, log_path, poll_ms) {
      write(
        "tampered after worker",
        run_context$local_evidence_snapshot$paths[["queries.csv"]],
        append = TRUE
      )
      row <- make_valid_benchmark_outcome(
        "export",
        job$pair_id,
        bench_empty_outcome
      )
      row$artifact_path <- job$artifact_path
      list(row = row, diagnostic = NA_character_)
    },
    envir = helper_environment
  )

  condition <- tryCatch(
    {
      .bench_with_integrity_failure_evidence(paths$output, "comparison", {
        .bench_run_measured_schedule(
          schedule,
          list(tiny = list()),
          "/tmp/source.h5ad",
          paths,
          list(
            bench_root = bench_root,
            library = paths$library,
            local_evidence_snapshot = snapshot
          )
        )
      })
      NULL
    },
    error = identity
  )
  expect_s3_class(condition, "bench_integrity_error")
  raw <- utils::read.csv(
    file.path(paths$output, "export.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(raw), 1L)
  expect_identical(raw$pair_id, schedule$pair_id)
  validation <- .bench_read_typed_validation(file.path(
    paths$output,
    "validation.csv"
  ))
  expect_identical(validation$status, c("FAIL", "INVALID"))
  expect_identical(tail(validation$check_id, 1L), "panel_valid")
})

test_that("clean Git state is explicit in environment and manifest evidence", {
  skip_unless_bench_tree()
  load_benchmark_contract()
  repo <- tempfile("clean-git-fixture-")
  dir.create(repo)
  run_git <- function(args) {
    output <- suppressWarnings(system2(
      "git",
      c("-C", shQuote(repo), args),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(output, "status", exact = TRUE)
    if (is.null(status)) 0L else as.integer(status)
  }
  expect_identical(run_git(c("init", "--quiet")), 0L)
  writeLines("fixture", file.path(repo, "tracked.txt"))
  expect_identical(run_git(c("add", "tracked.txt")), 0L)
  expect_identical(
    run_git(c(
      "-c",
      "user.name=BenchmarkTest",
      "-c",
      "user.email=benchmark@example.invalid",
      "commit",
      "--quiet",
      "-m",
      "fixture"
    )),
    0L
  )
  environment <- bench_record_environment(repo, "fixture command")
  expect_identical(environment$git_dirty, "false")
  expect_silent(bench_assert_environment_unchanged(environment))
  setup <- list(
    setup_elapsed_secs = 1,
    setup_peak_rss_mb = 2,
    setup_r_heap_peak_mb = 3,
    runtime_sha256 = paste(rep("a", 64), collapse = ""),
    common_target_actual = 4L,
    source_path = "/tmp/source.h5ad",
    harness_config_sha256 = paste(rep("b", 64), collapse = ""),
    harness_helpers_sha256 = paste(rep("c", 64), collapse = ""),
    harness_sha256 = bench_sha256_object(.bench_harness_payload(
      paste(rep("b", 64), collapse = ""),
      paste(rep("c", 64), collapse = "")
    )),
    package_version = "4.2",
    package_path = "/tmp/run/CerebroNexus",
    runtime = data.frame(
      package = "CerebroNexus",
      version = "4.2",
      package_path = "/tmp/run/CerebroNexus"
    )
  )
  manifest <- .bench_manifest_rows(environment, setup)
  expect_identical(manifest$value[manifest$key == "git_dirty"], "false")
  write("changed during run", file.path(repo, "tracked.txt"), append = TRUE)
  expect_error(bench_assert_environment_unchanged(environment), "changed")
})

test_that("Panel A static evidence and linkage reject every frozen control mutation", {
  skip_unless_bench_tree()
  skip_if_not_installed("digest")
  load_benchmark_contract()
  root <- withr::local_tempdir()
  schedule <- bench_comparison_schedule(
    c(tier_125k = 2L, tier_250k = 3L, common = 4L),
    c("embedded", "bpcells", "h5"),
    3L
  )
  exports <- do.call(
    rbind,
    lapply(schedule$pair_id, function(id) {
      make_valid_benchmark_outcome("export", id, bench_empty_outcome)
    })
  )
  accesses <- do.call(
    rbind,
    lapply(schedule$pair_id, function(id) {
      make_valid_benchmark_outcome("access", id, bench_empty_outcome)
    })
  )
  exports$package_path <- "/tmp/run-library/CerebroNexus"
  accesses$package_path <- "/tmp/run-library/CerebroNexus"
  hash <- function(label) bench_sha256_object(label)
  genes <- data.frame(
    gene = paste0("g", 1:5),
    role = c("first", rep("block", 4)),
    density = rep(.5, 5),
    source_row = 1:5,
    tie_break_rank = 1:5
  )
  make_plan <- function(n) {
    plan <- list(
      schema = "bench-query-plan-v1",
      source_sha256 = hash("source"),
      sampling_sha256 = hash(paste0("sampling", n)),
      source_dimensions = c(genes = 5L, cells = 4L),
      dimensions = c(genes = 5L, cells = as.integer(n)),
      genes = genes,
      ordered_indices_sha256 = hash(paste0("indices", n)),
      cell_identity_sha256 = hash(paste0("cells", n)),
      first_row_numeric_sha256 = hash(paste0("row", n)),
      block_numeric_sha256 = hash(paste0("block", n)),
      boundaries = bench_stratified_blocks(4L, n)
    )
    plan$query_plan_sha256 <- bench_sha256_object(plan)
    plan
  }
  plans <- list(
    tier_125k = make_plan(2L),
    tier_250k = make_plan(3L),
    common = make_plan(4L)
  )
  common <- plans$common
  source <- data.frame(
    source_key = "fixture",
    source_url = "local",
    expected_bytes = 1,
    actual_bytes = 1,
    expected_sha256 = hash("source"),
    actual_sha256 = hash("source"),
    n_cells = 4L,
    n_genes = 5L,
    exact_nnz = 5
  )
  BENCH_CONFIG$source$key <- source$source_key
  BENCH_CONFIG$source$url <- source$source_url
  BENCH_CONFIG$source$expected_bytes <- source$expected_bytes
  BENCH_CONFIG$source$expected_sha256 <- source$expected_sha256
  BENCH_CONFIG$source$n_cells <- source$n_cells
  BENCH_CONFIG$comparison_fixed_tiers <- c(tier_125k = 2L, tier_250k = 3L)
  BENCH_CONFIG$common_target <- 4L
  BENCH_CONFIG$common_min_exclusive <- 3L
  runtime_packages <- sort(c(
    "CerebroNexus",
    "Matrix",
    "SeuratObject",
    "Seurat",
    "BPCells",
    "HDF5Array",
    "rhdf5",
    "digest",
    "callr"
  ))
  runtime_versions <- setNames(
    rep("1.0", length(runtime_packages)),
    runtime_packages
  )
  runtime_versions[["CerebroNexus"]] <- "4.2"
  runtime_paths <- setNames(
    file.path("/tmp/run-library", runtime_packages),
    runtime_packages
  )
  runtime_paths[["CerebroNexus"]] <- "/tmp/run-library/CerebroNexus"
  runtime_sha256 <- bench_sha256_object(list(
    r_version = "fixture-R",
    platform = "fixture-platform",
    packages = paste(runtime_packages, runtime_versions, sep = "=")
  ))
  harness_config_sha256 <- hash("harness-config")
  harness_helpers_sha256 <- hash("harness-helpers")
  harness_sha256 <- bench_sha256_object(.bench_harness_payload(
    harness_config_sha256,
    harness_helpers_sha256
  ))
  manifest <- data.frame(
    key = c(
      "git_sha",
      "schema_version",
      "config_sha256",
      "runtime_sha256",
      "common_target_actual",
      "package_version",
      "package_path",
      "r_version",
      "platform",
      "git_dirty",
      "source_path",
      "harness_config_sha256",
      "harness_helpers_sha256",
      "harness_sha256",
      paste0("package.", runtime_packages)
    ),
    value = c(
      paste(rep("a", 40), collapse = ""),
      as.character(BENCH_CONFIG$schema_version),
      bench_sha256_object(BENCH_CONFIG),
      runtime_sha256,
      "4",
      "4.2",
      "/tmp/run-library/CerebroNexus",
      "fixture-R",
      "fixture-platform",
      "false",
      "/tmp/source.h5ad",
      harness_config_sha256,
      harness_helpers_sha256,
      harness_sha256,
      paste(runtime_versions, runtime_paths, sep = "|")
    )
  )
  tier_values <- c(tier_125k = 2L, tier_250k = 3L, common = 4L)
  sampling <- do.call(
    rbind,
    lapply(names(tier_values), function(tier) {
      blocks <- bench_stratified_blocks(4L, tier_values[[tier]])
      data.frame(
        tier_label = tier,
        n_cells = tier_values[[tier]],
        stratum = blocks$stratum,
        start = blocks$start,
        end = blocks$end,
        n = blocks$n,
        exact_nnz = tier_values[[tier]],
        indices_sha256 = plans[[tier]]$ordered_indices_sha256,
        cell_identity_sha256 = plans[[tier]]$cell_identity_sha256,
        shell_sha256 = hash(paste0("shell", tier_values[[tier]]))
      )
    })
  )
  queries <- data.frame(
    schema = "bench-query-plan-v1",
    tier_label = rep(names(tier_values), each = 5),
    source_sha256 = source$actual_sha256,
    sampling_sha256 = rep(
      unlist(lapply(plans, `[[`, "sampling_sha256"), use.names = FALSE),
      each = 5
    ),
    n_genes = 5L,
    n_cells = rep(tier_values, each = 5),
    gene = rep(paste0("g", 1:5), 3),
    role = rep(c("first", rep("block", 4)), 3),
    density = .5,
    source_row = rep(1:5, 3),
    tie_break_rank = rep(1:5, 3),
    ordered_indices_sha256 = rep(
      unlist(lapply(plans, `[[`, "ordered_indices_sha256"), use.names = FALSE),
      each = 5
    ),
    cell_identity_sha256 = rep(
      unlist(lapply(plans, `[[`, "cell_identity_sha256"), use.names = FALSE),
      each = 5
    ),
    first_row_numeric_sha256 = rep(
      unlist(
        lapply(plans, `[[`, "first_row_numeric_sha256"),
        use.names = FALSE
      ),
      each = 5
    ),
    block_numeric_sha256 = rep(
      unlist(lapply(plans, `[[`, "block_numeric_sha256"), use.names = FALSE),
      each = 5
    ),
    query_plan_sha256 = rep(
      unlist(lapply(plans, `[[`, "query_plan_sha256"), use.names = FALSE),
      each = 5
    )
  )
  eligibility <- bench_eligibility(
    "comparison",
    tier_values,
    setNames(as.double(tier_values), names(tier_values)),
    .Machine$integer.max
  )
  for (i in seq_len(nrow(schedule))) {
    id <- schedule$pair_id[[i]]
    tier <- schedule$tier_label[[i]]
    exports$shell_sha256[exports$pair_id == id] <- unique(sampling$shell_sha256[
      sampling$tier_label == tier
    ])
    accesses$expected_row_fingerprint[accesses$pair_id == id] <- plans[[
      tier
    ]]$first_row_numeric_sha256
    accesses$observed_row_fingerprint[accesses$pair_id == id] <- plans[[
      tier
    ]]$first_row_numeric_sha256
    accesses$expected_block_fingerprint[accesses$pair_id == id] <- plans[[
      tier
    ]]$block_numeric_sha256
    accesses$observed_block_fingerprint[accesses$pair_id == id] <- plans[[
      tier
    ]]$block_numeric_sha256
  }
  frozen_evidence <- list(
    manifest = manifest,
    source = source,
    sampling = sampling,
    eligibility = eligibility,
    queries = queries,
    plans = plans,
    config = BENCH_CONFIG
  )
  validation <- bench_validate_panel(
    schedule,
    eligibility,
    exports,
    accesses,
    sampling,
    plans,
    evidence = frozen_evidence
  )
  expect_identical(length(unique(queries$query_plan_sha256)), 3L)
  bad_evidence <- frozen_evidence
  bad_evidence$queries$density[[1L]] <- 0.25
  invalid <- bench_validate_panel(
    schedule,
    eligibility,
    exports,
    accesses,
    sampling,
    plans,
    evidence = bad_evidence
  )
  expect_identical(
    invalid$status[invalid$check_id == "frozen_queries_contract"],
    "FAIL"
  )
  expect_identical(tail(invalid$status, 1L), "INVALID")
  bad_origin_evidence <- frozen_evidence
  bad_origin_evidence$manifest$value[
    bad_origin_evidence$manifest$key == "package_path"
  ] <-
    "/tmp/different/CerebroNexus"
  invalid_origin <- bench_validate_panel(
    schedule,
    eligibility,
    exports,
    accesses,
    sampling,
    plans,
    evidence = bad_origin_evidence
  )
  expect_identical(
    invalid_origin$status[
      invalid_origin$check_id == "frozen_package_origin_contract"
    ],
    "FAIL"
  )
  expect_identical(tail(invalid_origin$status, 1L), "INVALID")
  bench_write <- function(name, object) {
    path <- file.path(root, name)
    if (grepl("rds$", name)) {
      saveRDS(object, path, version = 3)
    } else {
      utils::write.csv(object, path, row.names = FALSE, na = "NA")
    }
  }
  bench_write("manifest.csv", manifest)
  bench_write("source.csv", source)
  bench_write("sampling.csv", sampling)
  bench_write("eligibility.csv", eligibility)
  bench_write("queries.csv", queries)
  bench_write("query-plan.rds", plans)
  bench_write("schedule.csv", schedule)
  bench_write("export.csv", exports)
  bench_write("access.csv", accesses)
  bench_write("validation.csv", validation)
  evidence <- bench_validate_panel_a_evidence(root)
  hook_environment <- environment(bench_validate_panel_a_evidence)
  original_read_hook <- get(
    ".bench_panel_a_after_first_read",
    envir = hook_environment
  )
  assign(
    ".bench_panel_a_after_first_read",
    function(paths) {
      write("", paths[["validation.csv"]], append = TRUE)
      invisible(NULL)
    },
    envir = hook_environment
  )
  expect_error(
    bench_validate_panel_a_evidence(root),
    "changed while being read"
  )
  assign(
    ".bench_panel_a_after_first_read",
    original_read_hook,
    envir = hook_environment
  )
  bench_write("validation.csv", validation)
  current <- list(
    source_sha256 = source$actual_sha256,
    git_sha = manifest$value[[1L]],
    schema_version = as.character(BENCH_CONFIG$schema_version),
    config_sha256 = manifest$value[[3L]],
    runtime_sha256 = manifest$value[[4L]],
    harness_sha256 = harness_sha256,
    common_target_actual = 4L,
    common_sampling_sha256 = common$sampling_sha256,
    common_shell_sha256 = hash("shell4"),
    common_cell_identity_sha256 = common$cell_identity_sha256,
    common_query_plan_sha256 = common$query_plan_sha256
  )
  expect_silent(bench_validate_panel_a_linkage(evidence, current))

  calls <- 0L
  write("tamper", file.path(root, "queries.csv"), append = TRUE)
  expect_error(
    {
      .bench_assert_frozen_snapshot(evidence)
      calls <- calls + 1L
    },
    "changed"
  )
  expect_identical(calls, 0L)
  bench_write("queries.csv", queries)
  expect_silent(.bench_assert_frozen_snapshot(evidence))

  mutated_manifest <- manifest
  mutated_manifest$value[mutated_manifest$key == "config_sha256"] <- hash(
    "wrong-config"
  )
  bench_write("manifest.csv", mutated_manifest)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("manifest.csv", manifest)
  mutated_dirty <- manifest
  mutated_dirty$value[mutated_dirty$key == "git_dirty"] <- "true"
  bench_write("manifest.csv", mutated_dirty)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("manifest.csv", manifest)
  mutated_source <- source
  mutated_source$actual_sha256 <- hash("wrong-source")
  bench_write("source.csv", mutated_source)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("source.csv", source)
  mutated_sampling <- sampling
  mutated_sampling$end[[1L]] <- mutated_sampling$end[[1L]] + 1L
  bench_write("sampling.csv", mutated_sampling)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("sampling.csv", sampling)
  mutated_queries <- queries
  mutated_queries$density[[1L]] <- 0.25
  bench_write("queries.csv", mutated_queries)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("queries.csv", queries)
  mutated_eligibility <- eligibility
  mutated_eligibility$reason[[1L]] <- "invented"
  bench_write("eligibility.csv", mutated_eligibility)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("eligibility.csv", eligibility)
  mutated_plans <- plans
  mutated_plans$common$query_plan_sha256 <- hash("wrong-plan")
  bench_write("query-plan.rds", mutated_plans)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("query-plan.rds", plans)
  mutated_origin <- exports
  mutated_origin$package_path[[1L]] <- "/tmp/other/CerebroNexus"
  bench_write("export.csv", mutated_origin)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("export.csv", exports)

  for (key in names(current)) {
    changed <- current
    changed[[key]] <- if (key == "common_target_actual") 5L else hash(key)
    expect_error(bench_validate_panel_a_linkage(evidence, changed), "linkage")
  }
  mutated_schedule <- schedule[-1L, ]
  bench_write("schedule.csv", mutated_schedule)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("schedule.csv", schedule)
  mutated_export <- exports
  mutated_export$status[[1L]] <- "FAILED_export"
  bench_write("export.csv", mutated_export)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("export.csv", exports)
  mutated_access <- accesses
  mutated_access$status[[1L]] <- "NOT_RUN_EXPORT_FAILED"
  bench_write("access.csv", mutated_access)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("access.csv", accesses)
  mutated_validation <- validation
  mutated_validation$status[[2L]] <- "INVALID"
  bench_write("validation.csv", mutated_validation)
  expect_error(bench_validate_panel_a_evidence(root))
  bench_write("validation.csv", validation[-1L, ])
  expect_error(bench_validate_panel_a_evidence(root), "canonical|VALID")
  bench_write(
    "validation.csv",
    rbind(
      validation[-nrow(validation), ],
      validation[1L, ],
      validation[nrow(validation), ]
    )
  )
  expect_error(bench_validate_panel_a_evidence(root), "canonical")
  extra_gate <- bench_validation_row(
    "invented_gate",
    "comparison",
    "panel",
    "x",
    "x",
    "PASS"
  )
  bench_write(
    "validation.csv",
    rbind(
      validation[-nrow(validation), ],
      extra_gate,
      validation[nrow(validation), ]
    )
  )
  expect_error(bench_validate_panel_a_evidence(root), "canonical")
})
