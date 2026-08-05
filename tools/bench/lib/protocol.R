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
      include_scale_tiers = FALSE,
      comparison_tier_mode = "all",
      article_eligible = FALSE
    ),
    publication = list(
      name = "publication",
      export_repeats = 3L,
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
      "; expected quick, standard, publication, or stress",
      call. = FALSE
    )
  }
  profile
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

.bench_result_key <- function(x) {
  paste(x$source, x$n_cells, x$backend, x$export_repeat, sep = "|")
}

.bench_access_key <- function(result_key, access_repeat) {
  paste(result_key, access_repeat, sep = "|")
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

  successful <- exports[
    identical(exports$status, "OK") | exports$status == "OK",
    ,
    drop = FALSE
  ]
  comparison_keys <- expected[schedule$comparison]
  if (!all(comparison_keys %in% .bench_result_key(successful))) {
    stop("comparison tier did not complete every backend", call. = FALSE)
  }

  if (nrow(access)) {
    mismatch <-
      access$correctness != "OK" |
      access$row_fingerprint != access$reference_row_fingerprint |
      access$block_fingerprint != access$reference_block_fingerprint
    if (any(is.na(mismatch) | mismatch)) {
      stop("backend correctness fingerprint mismatch", call. = FALSE)
    }
  }

  required_access_columns <- c(
    "source",
    "n_cells",
    "backend",
    "export_repeat",
    "access_repeat"
  )
  if (!all(required_access_columns %in% names(access))) {
    stop("access results are missing identity columns", call. = FALSE)
  }
  successful_keys <- .bench_result_key(successful)
  successful_schedule <- schedule[
    match(successful_keys, expected),
    ,
    drop = FALSE
  ]
  expected_access_keys <- unlist(
    Map(
      function(key, repeats) {
        .bench_access_key(key, seq_len(repeats))
      },
      successful_keys,
      successful_schedule$access_repeats
    ),
    use.names = FALSE
  )
  access_keys <- if (nrow(access)) {
    .bench_access_key(.bench_result_key(access), access$access_repeat)
  } else {
    character()
  }
  if (
    anyNA(access_keys) ||
      anyDuplicated(access_keys) ||
      !setequal(access_keys, expected_access_keys)
  ) {
    stop(
      "access measurement identities do not match the scheduled repetitions",
      call. = FALSE
    )
  }
  TRUE
}

bench_require_article_profile <- function(profile) {
  if (is.character(profile)) {
    profile <- bench_profile(profile)
  }
  if (!isTRUE(profile$article_eligible)) {
    stop(
      "the user-facing article requires results from the publication profile",
      call. = FALSE
    )
  }
  TRUE
}
