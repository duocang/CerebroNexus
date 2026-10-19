#!/usr/bin/env Rscript

command_args <- commandArgs(trailingOnly = TRUE)
if (!length(command_args)) {
  stop("need a benchmark command", call. = FALSE)
}
command <- command_args[[1L]]
command_args <- command_args[-1L]
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/benchmarks/expression-backends/scripts")
}
source(file.path(here, "benchmark.R"))

if (identical(command, "inspect")) {
  # Inspect every configured source over ROS3 and record what it contains.
  #
  # Usage: Rscript benchmark_cli.R inspect <result>
  #
  # Costs a few HTTP range requests per source and no bulk transfer at all, so it
  # is safe to run before committing to a sweep. The `dgc_representable` column is
  # the interesting one: a source whose non-zero count exceeds 2^31 - 1 cannot be
  # held in a dgCMatrix at full size no matter how much RAM the host has.

  args <- command_args
  result <- if (length(args) >= 1) {
    args[1]
  } else {
    "tests/benchmarks/results/expression-backends/00_probe.csv"
  }

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }

  for (nm in bench_active_sources()) {
    spec <- BENCH_SOURCES[[nm]]
    bench_msg("probing %s", nm)
    p <- tryCatch(bench_probe(spec), error = function(e) {
      bench_msg("  FAILED: %s", conditionMessage(e))
      NULL
    })
    if (is.null(p)) {
      next
    }
    bench_append_row(
      result,
      data.frame(
        source = nm,
        label = p$label,
        kind = p$kind,
        n_cells = p$n_cells,
        n_genes = p$n_genes,
        nnz = p$nnz,
        nnz_per_cell = round(p$nnz_per_cell, 1),
        source_bytes = spec$expected_bytes,
        dgc_gb_full = round(p$dgc_gb_full, 2),
        dgc_representable = p$dgc_representable,
        tiers = paste(
          format(spec$tiers, scientific = FALSE, trim = TRUE),
          collapse = "|"
        ),
        stringsAsFactors = FALSE
      )
    )
    bench_msg(
      "  %s cells x %s genes, nnz %.4e (%.0f/cell), full dgCMatrix %.1f GB%s",
      format(p$n_cells, big.mark = ","),
      format(p$n_genes, big.mark = ","),
      p$nnz,
      p$nnz_per_cell,
      p$dgc_gb_full,
      if (p$dgc_representable) "" else " -- EXCEEDS the 32-bit index limit"
    )
  }
} else if (identical(command, "environment")) {
  # Write benchmark run provenance.

  args <- command_args
  if (length(args) < 1L) {
    stop("need <result.csv>", call. = FALSE)
  }
  result <- args[1]
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }
  repo <- normalizePath(file.path(here, "..", "..", "..", ".."))

  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }

  capture_command <- function(command, args = character()) {
    out <- suppressWarnings(system2(
      command,
      vapply(args, shQuote, character(1)),
      stdout = TRUE,
      stderr = FALSE
    ))
    paste(trimws(out), collapse = " ")
  }

  git_value <- function(...) {
    capture_command("git", c("-C", repo, ...))
  }

  cpu_name <- function() {
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
      value <- capture_command("sysctl", c("-n", "machdep.cpu.brand_string"))
      if (nzchar(value)) return(value)
    }
    if (file.exists("/proc/cpuinfo")) {
      lines <- readLines("/proc/cpuinfo", warn = FALSE)
      model <- sub(
        "^[^:]+:[[:space:]]*",
        "",
        grep("^model name", lines, value = TRUE)
      )
      if (length(model)) return(model[1])
    }
    Sys.info()[["machine"]]
  }

  memory_mb <- function() {
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
      bytes <- suppressWarnings(as.numeric(capture_command(
        "sysctl",
        c("-n", "hw.memsize")
      )))
      if (is.finite(bytes)) return(bytes / 2^20)
    }
    if (file.exists("/proc/meminfo")) {
      line <- grep(
        "^MemTotal:",
        readLines("/proc/meminfo", warn = FALSE),
        value = TRUE
      )
      kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", line[1])))
      if (is.finite(kb)) return(kb / 1024)
    }
    NA_real_
  }

  package_version_or_na <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      return(NA_character_)
    }
    as.character(utils::packageVersion(package))
  }

  description <- read.dcf(file.path(repo, "DESCRIPTION"))
  tracked_status <- git_value(
    "status",
    "--porcelain",
    "--untracked-files=no",
    "--",
    ".",
    ":(exclude,glob)tests/benchmarks/results/expression-backends/**"
  )
  untracked_status <- git_value(
    "ls-files",
    "--others",
    "--exclude-standard",
    "--",
    ".",
    ":(exclude,glob)tests/benchmarks/results/expression-backends/**"
  )
  status <- paste0(tracked_status, untracked_status)
  scratch <- Sys.getenv("BENCH_SCRATCH")
  scratch_df <- if (nzchar(scratch) && dir.exists(scratch)) {
    capture_command("df", c("-P", scratch))
  } else {
    ""
  }
  manifest <- c(
    study_id = Sys.getenv("BENCH_STUDY_ID"),
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "quick"),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    timing_contract = "monotonic-v1",
    elapsed_clock = "CLOCK_MONOTONIC",
    clock_resolution_secs = as.character(bench_clock()[["resolution"]]),
    # Full/scale runs without this key wrote "h5" at BPCells' gzip level 0.
    h5_gzip_level = as.character(bench_h5_gzip_level("h5")),
    h5_uncompressed_gzip_level = as.character(bench_h5_gzip_level("h5_uncompressed")),
    git_sha = git_value("rev-parse", "HEAD"),
    git_branch = git_value("branch", "--show-current"),
    git_dirty = if (nzchar(status)) "true" else "false",
    package_version = unname(description[1, "Version"]),
    r_version = R.version.string,
    r_platform = R.version$platform,
    os = paste(Sys.info()[c("sysname", "release", "version")], collapse = " "),
    cpu = cpu_name(),
    logical_cores = as.character(parallel::detectCores(logical = TRUE)),
    benchmark_threads = Sys.getenv("BENCH_THREADS", "1"),
    slurm_job_id = Sys.getenv("SLURM_JOB_ID"),
    slurm_node_list = Sys.getenv("SLURM_NODELIST"),
    slurm_cpus_per_task = Sys.getenv("SLURM_CPUS_PER_TASK"),
    slurm_memory_per_node = Sys.getenv("SLURM_MEM_PER_NODE"),
    scratch_df = scratch_df,
    storage_description = Sys.getenv("BENCH_STORAGE_DESCRIPTION"),
    memory_mb = format(memory_mb(), scientific = FALSE, trim = TRUE),
    memory_available_bytes = if (identical(Sys.getenv("BENCH_PROFILE"), "scale"))
      Sys.getenv("BENCH_AVAILABLE_MEMORY_BYTES", format(bench_available_memory_bytes(), scientific = FALSE, trim = TRUE)) else "not_recorded",
    memory_limit_bytes = Sys.getenv("BENCH_MEMORY_LIMIT_BYTES", "not_enforced"),
    memory_policy = if (identical(Sys.getenv("BENCH_PROFILE"), "scale")) {
      if (nzchar(Sys.getenv("BENCH_MEMORY_LIMIT_BYTES")))
        "99pct-available-at-start; cgroup-v2; swap-disabled" else
        "99pct-available-preflight; no runner hard cap recorded"
    } else "legacy-70pct-total-preflight",
    r_vector_limit_mb = format(mem.maxVSize(), scientific = FALSE, trim = TRUE)
  )
  packages <- c(
    "Matrix",
    "rhdf5",
    "Seurat",
    "SeuratObject",
    "BPCells",
    "HDF5Array",
    "CerebroNexus"
  )
  package_versions <- vapply(packages, package_version_or_na, character(1))
  # Provenance is recorded before the branch-under-test is installed into its
  # isolated library. Its authoritative version is therefore DESCRIPTION, not a
  # possibly absent or stale package on the caller's library path.
  package_versions[["CerebroNexus"]] <- unname(description[1, "Version"])
  manifest <- c(
    manifest,
    stats::setNames(
      package_versions,
      paste0("package_", packages)
    )
  )

  dir.create(dirname(result), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      key = names(manifest),
      value = unname(manifest),
      stringsAsFactors = FALSE
    ),
    result,
    row.names = FALSE,
    na = ""
  )
  message("wrote run provenance to ", result)
} else if (identical(command, "plan")) {
  # Write the deterministic benchmark schedule.

  args <- command_args
  if (length(args) < 1L) {
    stop("need <result.csv>", call. = FALSE)
  }
  result <- args[1]
  tsv_result <- if (length(args) >= 2L) args[2] else NULL
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }

  profile <- bench_profile(Sys.getenv("BENCH_PROFILE", "quick"))
  schedule <- if (identical(profile$name, "scale")) {
    bench_scale_schedule(BENCH_SOURCES)
  } else {
    bench_schedule(
      BENCH_SOURCES,
      profile,
      sources = bench_active_sources()
    )
  }
  dir.create(dirname(result), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(schedule, result, row.names = FALSE)
  if (!is.null(tsv_result)) {
    tsv_schedule <- schedule
    tsv_schedule$n_cells <- sprintf("%.0f", tsv_schedule$n_cells)
    utils::write.table(
      tsv_schedule,
      tsv_result,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
  }
  message(
    sprintf(
      "wrote %d scheduled export cells for the %s profile to %s",
      nrow(schedule),
      profile$name,
      result
    )
  )
} else if (identical(command, "full-resources")) {
  # Preflight a scale or full-source out-of-core schedule.

  args <- command_args
  if (length(args) < 4L) {
    stop(
      "need <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>",
      call. = FALSE
    )
  }
  inventory <- utils::read.csv(args[1L], stringsAsFactors = FALSE)
  plan <- utils::read.csv(args[2L], stringsAsFactors = FALSE)
  manifest <- utils::read.csv(args[3L], stringsAsFactors = FALSE)
  output <- args[4L]
  values <- stats::setNames(as.character(manifest$value), manifest$key)

  free_disk_bytes <- function(path) {
    override <- suppressWarnings(as.numeric(Sys.getenv(
      "BENCH_FREE_DISK_BYTES"
    )))
    if (is.finite(override) && override > 0) {
      return(override)
    }
    lines <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE)
    fields <- strsplit(trimws(tail(lines, 1L)), "[[:space:]]+")[[1L]]
    as.numeric(fields[4L]) * 1024
  }

  planned <- unique(plan[c("source", "n_cells")])
  matched <- match(planned$source, inventory$source)
  if (anyNA(matched)) {
    stop("inventory does not cover the full-source plan", call. = FALSE)
  }
  source <- inventory[matched, , drop = FALSE]
  memory_mb <- min(
    as.numeric(values[["memory_mb"]]),
    as.numeric(values[["r_vector_limit_mb"]]),
    na.rm = TRUE
  )
  memory_fraction <- 0.70
  if (identical(Sys.getenv("BENCH_PROFILE"), "scale")) {
    available <- suppressWarnings(as.numeric(values["memory_available_bytes"]))
    if (length(available) != 1L || !is.finite(available) || available <= 0)
      stop("scale requires a recorded available-memory snapshot", call. = FALSE)
    memory_mb <- bench_scale_memory_budget(available, memory_mb) / 2^20
    enforced <- suppressWarnings(as.numeric(values["memory_limit_bytes"]))
    if (length(enforced) == 1L && is.finite(enforced) && enforced > 0)
      memory_mb <- min(memory_mb, enforced / 2^20)
    memory_fraction <- 1
  }
  disk_free <- free_disk_bytes(dirname(output))
  # Only 12 queried rows are materialised. Four GiB covers R, native buffers and
  # the full-cell metadata shell with a conservative margin.
  estimated_peak_mb <- 4096 + planned$n_cells * 12 * 8 / 2^20
  # The cached source coexists with one staged backend. An H5 build also holds a
  # cell-major BPCells copy (and briefly its sort spill) while writing. Measured
  # sizes: uncompressed H5 about 12.2 bytes and BPCells at most about 4.8 bytes
  # per nonzero, so the largest build needs source + 18 bytes per nonzero.
  estimated_nnz <- source$nnz * planned$n_cells / source$n_cells
  required_disk <- source$source_bytes + estimated_nnz * 18
  assessment <- data.frame(
    source = planned$source,
    n_cells = planned$n_cells,
    estimated_nnz = estimated_nnz,
    estimated_peak_mb = round(estimated_peak_mb),
    memory_budget_mb = round(memory_mb * memory_fraction),
    source_bytes = source$source_bytes,
    disk_budget_bytes = disk_free * 0.80,
    memory_ok = estimated_peak_mb <= memory_mb * memory_fraction,
    index_ok = TRUE,
    disk_ok = required_disk <= disk_free * 0.80,
    stringsAsFactors = FALSE
  )
  assessment$safe <- with(assessment, memory_ok & index_ok & disk_ok)
  assessment$reason <- ifelse(
    assessment$safe,
    "safe out-of-core plan",
    paste0(
      ifelse(assessment$memory_ok, "", "insufficient memory; "),
      ifelse(assessment$disk_ok, "", "insufficient disk")
    )
  )
  # The same tier also runs embedded in scale, which materialises the matrix.
  # Keep tier-level rows for the streaming backends and record embedded limits
  # separately so a skipped embedded build does not suppress H5/BPCells.
  embedded <- bench_assess_resources(
    inventory,
    planned,
    memory_mb,
    memory_mb,
    disk_free,
    memory_fraction = memory_fraction
  )
  assessment$embedded_peak_mb <- embedded$estimated_peak_mb
  assessment$embedded_safe <- embedded$safe
  assessment$embedded_reason <- embedded$reason
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(assessment, output, row.names = FALSE)
  if (any(!assessment$safe)) {
    stop("unsafe full-source out-of-core plan", call. = FALSE)
  }
  message("full-source out-of-core resource plan is safe")
} else if (identical(command, "resources")) {
  # Refuse a benchmark plan that does not fit the current host.
  #
  # Usage: Rscript benchmark_cli.R resources \
  #   <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>

  args <- command_args
  if (length(args) < 4L) {
    stop(
      "need <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>",
      call. = FALSE
    )
  }
  inventory_path <- args[1]
  plan_path <- args[2]
  manifest_path <- args[3]
  output_path <- args[4]
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }

  free_disk_bytes <- function(path) {
    override <- suppressWarnings(as.numeric(Sys.getenv(
      "BENCH_FREE_DISK_BYTES"
    )))
    if (is.finite(override) && override > 0) {
      return(override)
    }
    lines <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE)
    fields <- strsplit(trimws(tail(lines, 1L)), "[[:space:]]+")[[1]]
    available_kb <- suppressWarnings(as.numeric(fields[4]))
    if (!is.finite(available_kb)) {
      stop("could not determine free disk space", call. = FALSE)
    }
    available_kb * 1024
  }

  inventory <- utils::read.csv(inventory_path, stringsAsFactors = FALSE)
  plan <- utils::read.csv(plan_path, stringsAsFactors = FALSE)
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
  memory_mb <- suppressWarnings(as.numeric(manifest_values[["memory_mb"]]))
  vector_limit_mb <- suppressWarnings(
    as.numeric(manifest_values[["r_vector_limit_mb"]])
  )
  if (
    !is.finite(memory_mb) ||
      memory_mb <= 0 ||
      is.na(vector_limit_mb) ||
      vector_limit_mb <= 0
  ) {
    stop("run manifest has no usable memory limits", call. = FALSE)
  }

  assessment <- bench_assess_resources(
    inventory,
    plan,
    memory_mb = memory_mb,
    vector_limit_mb = vector_limit_mb,
    free_disk_bytes = free_disk_bytes(dirname(output_path))
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(assessment, output_path, row.names = FALSE)

  for (i in seq_len(nrow(assessment))) {
    message(sprintf(
      "%s @ %s cells: %s (estimated %.0f MB; budget %.0f MB)",
      assessment$source[i],
      format(assessment$n_cells[i], big.mark = ",", scientific = FALSE),
      toupper(if (assessment$safe[i]) "safe" else "unsafe"),
      assessment$estimated_peak_mb[i],
      assessment$memory_budget_mb[i]
    ))
  }
  bench_require_safe_plan(assessment)
} else if (identical(command, "query-plan")) {
  # Prepare one immutable query plan before any timed backend build.

  args <- command_args
  if (length(args) < 6L) {
    stop(
      paste(
        "need <source> <n_cells> <scratch> <query_plan>",
        "<result> <query_panel_result>"
      ),
      call. = FALSE
    )
  }
  src_name <- args[1L]
  n_cells <- as.numeric(args[2L])
  scratch <- args[3L]
  query_plan_path <- args[4L]
  result <- args[5L]
  query_panel_result <- args[6L]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }

  spec <- BENCH_SOURCES[[src_name]]
  if (is.null(spec)) {
    stop("unknown source: ", src_name, call. = FALSE)
  }
  source_path <- file.path(
    scratch,
    "sources",
    basename(sub("\\?.*$", "", spec$url))
  )
  if (!file.exists(source_path)) {
    stop("cached benchmark source is missing", call. = FALSE)
  }

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE"),
    source = src_name,
    n_cells = n_cells,
    n_genes = NA_real_,
    nnz = NA_real_,
    source_prepare_secs = NA_real_,
    query_plan_secs = NA_real_,
    peak_rss_mb = NA_real_,
    subset_n_cells = NA_real_,
    subset_cells_fingerprint = NA_character_,
    query_plan_fingerprint = NA_character_,
    status = "OK",
    stringsAsFactors = FALSE
  )

  fail <- function(stage, error) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(error))
    bench_append_row(result, row)
    bench_msg(
      "query-plan preparation failed at %s: %s",
      stage,
      conditionMessage(error)
    )
    quit(status = 1L)
  }

  source_matrix <- NULL
  profile <- Sys.getenv("BENCH_PROFILE")
  row$source_prepare_secs <- tryCatch(
    bench_time({
      source_matrix <- if (identical(profile, "scale")) {
        bench_open_source_tier(spec, source_path, n_cells)
      } else {
        spec$local_path <- source_path
        bench_read_subset(spec, n_cells, n_chunks = 4L, verbose = FALSE)
      }
    }),
    error = function(error) fail("source", error)
  )
  if (ncol(source_matrix) != n_cells) {
    fail(
      "source",
      simpleError("prepared source cell count differs from schedule")
    )
  }
  row$n_genes <- nrow(source_matrix)
  plan <- NULL
  row$query_plan_secs <- tryCatch(
    bench_time({
      plan <- if (identical(profile, "scale")) {
        bench_build_lazy_query_plan(
          source_matrix,
          bench_profile(profile)$query_genes
        )
      } else {
        bench_build_query_plan(
          source_matrix,
          bench_profile(profile)$query_genes
        )
      }
    }),
    error = function(error) fail("query plan", error)
  )
  row$nnz <- plan$nnz
  row$query_plan_fingerprint <- plan$query_plan_fingerprint
  row$subset_n_cells <- length(plan$subset_cells)
  row$subset_cells_fingerprint <- plan$subset_cells_fingerprint
  row$peak_rss_mb <- bench_peak_rss_mb()

  dir.create(dirname(query_plan_path), recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile("query-plan-", tmpdir = dirname(query_plan_path))
  tryCatch(
    saveRDS(plan, staged, version = 3),
    error = function(error) fail("write", error)
  )
  if (!file.rename(staged, query_plan_path)) {
    unlink(staged)
    fail("write", simpleError("could not publish frozen query plan"))
  }
  panel_rows <- data.frame(
    run_id = row$run_id,
    profile = row$profile,
    source = row$source,
    n_cells = row$n_cells,
    panel_index = seq_len(nrow(plan$panel)),
    gene = plan$panel$gene,
    nnz = plan$panel$nnz,
    role = plan$panel$role,
    query_plan_fingerprint = plan$query_plan_fingerprint,
    reference_row_fingerprint = plan$reference_row_fingerprint,
    reference_block_fingerprint = plan$reference_block_fingerprint,
    subset_n_cells = length(plan$subset_cells),
    subset_cells_fingerprint = plan$subset_cells_fingerprint,
    reference_subset_row_fingerprint = plan$reference_subset_row_fingerprint,
    reference_subset_block_fingerprint = plan$reference_subset_block_fingerprint,
    stringsAsFactors = FALSE
  )
  bench_append_row(query_panel_result, panel_rows)
  bench_append_row(result, row)
  bench_msg(
    "prepared %s / %.0f cells query plan in %.1fs",
    src_name,
    n_cells,
    row$source_prepare_secs + row$query_plan_secs
  )
} else if (identical(command, "export")) {
  # Export one (source, tier, backend) cell of the grid.
  #
  # Usage: Rscript benchmark_cli.R export <source> <n_cells> <backend> \
  #   <export_repeat> <order_position> <scratch> <result> <query_plan>
  #
  # Each grid cell gets a fresh process. Embedded checks memory and sparse-index
  # limits before materialising its matrix. Each process writes its own row, so
  # an aborted run keeps everything already measured.

  args <- command_args
  if (length(args) < 8) {
    stop(
      paste(
        "need <source> <n_cells> <backend> <export_repeat>",
        "<order_position> <scratch> <result> <query_plan>"
      )
    )
  }
  src_name <- args[1]
  n_cells <- as.numeric(args[2])
  backend <- args[3]
  export_repeat <- as.integer(args[4])
  order_position <- as.integer(args[5])
  scratch <- args[6]
  result <- args[7]
  query_plan_path <- args[8]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }
  # BENCH_LIB holds the branch under test, installed into the scratch directory so
  # the numbers describe this worktree rather than whatever version happens to be
  # in the user's global library.
  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages(library(CerebroNexus))

  spec <- BENCH_SOURCES[[src_name]]
  if (is.null(spec)) {
    stop("unknown source: ", src_name)
  }
  cached <- file.path(scratch, "sources", basename(sub("\\?.*$", "", spec$url)))
  if (file.exists(cached)) {
    spec$local_path <- cached
  }

  out_dir <- file.path(
    scratch,
    "export",
    sprintf("%s_%.0f_%s_r%d", src_name, n_cells, backend, export_repeat)
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  crb <- file.path(out_dir, "bench.crb")

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "quick"),
    source = src_name,
    label = spec$label,
    n_cells = n_cells,
    n_genes = NA_real_,
    nnz = NA_real_,
    backend = backend,
    export_repeat = export_repeat,
    order_position = order_position,
    status = "OK",
    read_secs = NA_real_,
    seurat_secs = NA_real_,
    export_secs = NA_real_,
    shell_secs = NA_real_,
    serialize_secs = NA_real_,
    crb_mb = NA_real_,
    sibling_mb = NA_real_,
    total_mb = NA_real_,
    rss_mb = NA_real_,
    peak_rss_mb = NA_real_,
    r_peak_mb = NA_real_,
    query_plan_fingerprint = NA_character_,
    stringsAsFactors = FALSE
  )

  fail <- function(stage, e) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(e))
    if (
      identical(row$profile, "scale") &&
        identical(backend, "embedded") &&
        bench_resource_limit(e)
    ) {
      row$status <- paste0("SKIPPED(resource): ", conditionMessage(e))
    }
    bench_append_row(result, row)
    bench_msg("FAILED at %s: %s", stage, conditionMessage(e))
    quit(status = 0) # the failure IS the measurement; do not fail the sweep
  }

  gc(reset = TRUE)
  if (identical(row$profile, "scale") && identical(backend, "embedded")) {
    # Under the Scale cgroup cap the kernel kills an oversized build instead of
    # failing its allocation. run.sh re-enters here after confirming that kill.
    killed <- Sys.getenv("BENCH_EXPORT_OOM_KILLED")
    if (nzchar(killed)) {
      exact_plan <- readRDS(query_plan_path)
      row$status <- paste0("SKIPPED(resource): ", killed)
      row$nnz <- exact_plan$nnz
      row$query_plan_fingerprint <- exact_plan$query_plan_fingerprint
      bench_append_row(result, row)
      bench_msg("%s / %.0f cells / embedded: %s", src_name, n_cells, row$status)
      quit(status = 0L)
    }
    assessment <- utils::read.csv(file.path(
      scratch,
      "result",
      "resource_check.csv"
    ))
    tier <- assessment[
      assessment$source == src_name & assessment$n_cells == n_cells,
      ,
      drop = FALSE
    ]
    if (
      nrow(tier) != 1L ||
        !all(c("embedded_safe", "embedded_reason") %in% names(tier)) ||
        is.na(tier$embedded_safe)
    ) {
      stop("missing embedded resource assessment", call. = FALSE)
    }
    # The preflight estimates nnz from the source-wide density, which can over-
    # or underestimate a cell prefix. The frozen plan holds this tier's exact
    # nnz, so it decides memory and the sparse-index limit; any other preflight
    # failure (such as disk) still skips.
    exact_plan <- readRDS(query_plan_path)
    exact_peak_mb <- 1024 + exact_plan$nnz * BENCH_EMBEDDED_BYTES_PER_NNZ / 2^20
    exact_fits <- exact_peak_mb <= tier$memory_budget_mb &&
      exact_plan$nnz <= .Machine$integer.max
    if (
      bench_embedded_preflight_binds(tier$embedded_safe, tier$embedded_reason) ||
        (!tier$embedded_safe && !exact_fits)
    ) {
      row$status <- paste0(
        "SKIPPED(resource): preflight: ",
        tier$embedded_reason
      )
      row$nnz <- tier$estimated_nnz
      bench_append_row(result, row)
      bench_msg("%s / %.0f cells / embedded: %s", src_name, n_cells, row$status)
      quit(status = 0L)
    }
    if (
      exact_peak_mb > tier$memory_budget_mb ||
        exact_plan$nnz > .Machine$integer.max
    ) {
      row$status <- sprintf(
        "SKIPPED(resource): source-tier estimate %.0f MiB exceeds budget %.0f MiB or sparse-index limit",
        exact_peak_mb,
        tier$memory_budget_mb
      )
      row$nnz <- exact_plan$nnz
      row$query_plan_fingerprint <- exact_plan$query_plan_fingerprint
      bench_append_row(result, row)
      bench_msg("%s", row$status)
      quit(status = 0L)
    }
  }
  bench_msg("%s / %.0f cells / %s: reading", src_name, n_cells, backend)

  m <- NULL
  row$read_secs <- tryCatch(
    bench_time(
      m <- bench_read_export_matrix(spec, cached, n_cells, row$profile)
    ),
    error = function(e) fail("read", e)
  )
  row$n_genes <- nrow(m)
  row$nnz <- length(m@x)
  bench_msg("read done: %d x %d, nnz %.3e", nrow(m), ncol(m), length(m@x))

  query_plan <- tryCatch(
    readRDS(query_plan_path),
    error = function(e) fail("query plan", e)
  )
  if (query_plan$n_cells != ncol(m) || query_plan$n_genes != nrow(m)) {
    fail("query plan", simpleError("frozen query-plan dimensions changed"))
  }
  row$query_plan_fingerprint <- query_plan$query_plan_fingerprint

  obj <- NULL
  row$seurat_secs <- tryCatch(
    bench_time(obj <- bench_make_seurat(m)),
    error = function(e) fail("seurat", e)
  )
  rm(m)
  gc(verbose = FALSE)

  bench_msg("exporting %s", backend)
  row$export_secs <- tryCatch(
    bench_time(CerebroNexus::exportFromSeurat(
      object = obj,
      assay = "RNA",
      slot = "counts",
      file = crb,
      experiment_name = sprintf("%s_%.0f", src_name, n_cells),
      organism = spec$organism,
      groups = c("sample", "cluster"),
      nUMI = "nUMI",
      nGene = "nGene",
      expression_matrix_mode = backend,
      viewer_binary = "never",
      verbose = FALSE
    )),
    error = function(e) fail("export", e)
  )

  sibling <- switch(
    backend,
    bpcells = sub("\\.crb$", ".bpcells", crb),
    h5 = sub("\\.crb$", ".h5", crb),
    NULL
  )
  row$crb_mb <- bench_path_mb(crb)
  row$sibling_mb <- bench_path_mb(sibling)
  row$total_mb <- sum(c(row$crb_mb, row$sibling_mb), na.rm = TRUE)
  row$rss_mb <- bench_rss_mb()
  row$peak_rss_mb <- bench_peak_rss_mb()
  # gc() alternates (count, Mb) columns and the count named "max used" is NOT the
  # figure wanted; the Mb that follows it is. The column index is not fixed
  # either: an R with a vector memory limit set inserts a "limit (Mb)" column, so
  # the layout is 7 wide here and 6 wide elsewhere. The peak in MB is always the
  # last column.
  g <- gc()
  row$r_peak_mb <- sum(g[, ncol(g)], na.rm = TRUE)

  bench_append_row(result, row)
  bench_msg(
    "OK %s: total %.1f MB in %.1fs (peak R heap %.0f MB)",
    backend,
    row$total_mb,
    row$export_secs,
    row$r_peak_mb
  )
} else if (identical(command, "build-full")) {
  # Build one full-source out-of-core backend and its portable Cerebro shell.

  args <- command_args
  if (length(args) < 8L) {
    stop(
      paste(
        "need <source> <n_cells> <backend> <build_repeat>",
        "<order_position> <scratch> <result> <query_plan>"
      ),
      call. = FALSE
    )
  }
  src_name <- args[1L]
  n_cells <- as.numeric(args[2L])
  backend <- args[3L]
  build_repeat <- as.integer(args[4L])
  order_position <- as.integer(args[5L])
  scratch <- args[6L]
  result <- args[7L]
  query_plan_path <- args[8L]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }
  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages(library(CerebroNexus))

  spec <- BENCH_SOURCES[[src_name]]
  if (is.null(spec)) {
    stop("unknown source: ", src_name, call. = FALSE)
  }
  source_path <- file.path(
    scratch,
    "sources",
    basename(sub("\\?.*$", "", spec$url))
  )
  out_dir <- file.path(
    scratch,
    "export",
    sprintf("%s_%.0f_%s_r%d", src_name, n_cells, backend, build_repeat)
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  crb <- file.path(out_dir, "bench.crb")
  sibling <- if (backend == "bpcells") {
    file.path(out_dir, "bench.bpcells")
  } else {
    file.path(out_dir, "bench.h5")
  }

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "scale"),
    source = src_name,
    label = spec$label,
    n_cells = n_cells,
    n_genes = NA_real_,
    nnz = NA_real_,
    backend = backend,
    export_repeat = build_repeat,
    order_position = order_position,
    status = "OK",
    read_secs = NA_real_,
    seurat_secs = 0,
    export_secs = NA_real_,
    shell_secs = NA_real_,
    serialize_secs = NA_real_,
    crb_mb = NA_real_,
    sibling_mb = NA_real_,
    total_mb = NA_real_,
    rss_mb = NA_real_,
    peak_rss_mb = NA_real_,
    r_peak_mb = NA_real_,
    query_plan_fingerprint = NA_character_,
    stringsAsFactors = FALSE
  )

  fail <- function(stage, error) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(error))
    bench_append_row(result, row)
    bench_msg("FAILED at %s: %s", stage, conditionMessage(error))
    quit(status = 0L)
  }

  source_matrix <- NULL
  row$read_secs <- tryCatch(
    bench_time(
      source_matrix <- bench_open_source_tier(spec, source_path, n_cells)
    ),
    error = function(error) fail("open", error)
  )
  row$n_cells <- ncol(source_matrix)
  row$n_genes <- nrow(source_matrix)
  if (row$n_cells != n_cells) {
    fail("open", simpleError("scheduled and source cell counts differ"))
  }

  query_plan <- tryCatch(
    readRDS(query_plan_path),
    error = function(error) fail("query plan", error)
  )
  if (
    query_plan$n_cells != ncol(source_matrix) ||
      query_plan$n_genes != nrow(source_matrix)
  ) {
    fail("query plan", simpleError("frozen query-plan dimensions changed"))
  }
  row$nnz <- query_plan$nnz
  row$query_plan_fingerprint <- query_plan$query_plan_fingerprint

  row$export_secs <- tryCatch(
    bench_time(bench_write_full_backend(source_matrix, backend, sibling)),
    error = function(error) fail("build", error)
  )
  obj <- NULL
  row$shell_secs <- tryCatch(
    bench_time(
      obj <- bench_make_full_shell(
        source_matrix,
        backend,
        basename(sibling),
        src_name,
        spec$organism,
        Sys.getenv("BENCH_RUN_ID")
      )
    ),
    error = function(error) fail("shell", error)
  )
  row$serialize_secs <- tryCatch(
    bench_time(saveCerebro(obj, crb)),
    error = function(error) fail("shell", error)
  )

  row$crb_mb <- bench_path_mb(crb)
  row$sibling_mb <- bench_path_mb(sibling)
  row$total_mb <- row$crb_mb + row$sibling_mb
  row$rss_mb <- bench_rss_mb()
  row$peak_rss_mb <- bench_peak_rss_mb()
  heap <- gc()
  row$r_peak_mb <- sum(heap[, ncol(heap)], na.rm = TRUE)
  bench_append_row(result, row)
  bench_msg(
    "OK %s: %.0f x %.0f, %.1f MB in %.1fs",
    backend,
    row$n_genes,
    row$n_cells,
    row$total_mb,
    row$export_secs
  )
} else if (identical(command, "access")) {
  # Measure hydrated startup, memory, and query latency for one exported .crb.
  #
  # Usage: Rscript benchmark_cli.R access <source> <n_cells> <backend> \
  #   <export_repeat> <order_position> <access_repeat> <crb> <result> <query_plan>
  #
  # Runs in its own process so the resident-set reading describes this backend
  # only. Reads go through getExpressionRow() / getExpressionBlock() and the
  # reads use the public readCerebro() and expression getter paths.

  args <- command_args
  if (length(args) < 9) {
    stop(
      paste(
        "need <source> <n_cells> <backend> <export_repeat> <order_position>",
        "<access_repeat> <crb> <result> <query_plan>"
      )
    )
  }
  src_name <- args[1]
  n_cells <- as.numeric(args[2])
  backend <- args[3]
  export_repeat <- as.integer(args[4])
  order_position <- as.integer(args[5])
  access_repeat <- as.integer(args[6])
  crb <- args[7]
  result <- args[8]
  query_plan_path <- args[9]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }
  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages({
    library(CerebroNexus)
    library(Matrix)
  })

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "quick"),
    source = src_name,
    n_cells = n_cells,
    backend = backend,
    export_repeat = export_repeat,
    order_position = order_position,
    access_repeat = access_repeat,
    status = "OK",
    startup_secs = NA_real_,
    rss_mb = NA_real_,
    peak_rss_mb = NA_real_,
    access_contract = "materialized-v2",
    timing_contract = "monotonic-v1",
    first_query_secs = NA_real_,
    hot_p50_secs = NA_real_,
    hot_p95_secs = NA_real_,
    block_secs = NA_real_,
    subset_row_secs = NA_real_,
    subset_block_secs = NA_real_,
    subset_n_cells = NA_real_,
    n_hot = NA_integer_,
    correctness = NA_character_,
    row_fingerprint = NA_character_,
    reference_row_fingerprint = NA_character_,
    block_fingerprint = NA_character_,
    reference_block_fingerprint = NA_character_,
    subset_row_fingerprint = NA_character_,
    reference_subset_row_fingerprint = NA_character_,
    subset_block_fingerprint = NA_character_,
    reference_subset_block_fingerprint = NA_character_,
    query_plan_fingerprint = NA_character_,
    stringsAsFactors = FALSE
  )

  fail <- function(stage, e) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(e))
    bench_append_row(result, row)
    bench_msg("FAILED at %s: %s", stage, conditionMessage(e))
    quit(status = 0)
  }

  timing_state <- new.env(parent = emptyenv())
  timing_state$sequence <- 0L
  record_timing <- function(timing) {
    timing_state$sequence <- timing_state$sequence + 1L
    timing <- cbind(
      row[c(
        "run_id",
        "source",
        "n_cells",
        "backend",
        "export_repeat",
        "access_repeat"
      )],
      sequence = timing_state$sequence,
      timing
    )
    bench_append_row(
      file.path(dirname(result), "21_access_timings.csv"),
      timing
    )
  }

  obj <- NULL
  query_plan <- tryCatch(
    readRDS(query_plan_path),
    error = function(e) fail("query plan", e)
  )
  startup <- tryCatch(
    bench_timed(
      function() readCerebro(crb),
      record = record_timing,
      phase = "startup"
    ),
    error = function(e) fail("startup", e)
  )
  obj <- startup$value
  row$startup_secs <- startup$seconds
  row$rss_mb <- bench_rss_mb()

  metrics <- tryCatch(
    bench_measure_backend(
      obj,
      query_plan,
      hot_iterations = bench_profile(Sys.getenv(
        "BENCH_PROFILE",
        "quick"
      ))$hot_iterations,
      record = record_timing
    ),
    error = function(e) fail("correctness/access", e)
  )
  row$first_query_secs <- metrics$first_query_secs
  row$hot_p50_secs <- metrics$hot_p50_secs
  row$hot_p95_secs <- metrics$hot_p95_secs
  row$block_secs <- metrics$block_secs
  row$subset_row_secs <- metrics$subset_row_secs
  row$subset_block_secs <- metrics$subset_block_secs
  row$subset_n_cells <- metrics$subset_n_cells
  row$n_hot <- metrics$n_hot
  row$correctness <- metrics$correctness
  row$row_fingerprint <- metrics$row_fingerprint
  row$reference_row_fingerprint <- metrics$reference_row_fingerprint
  row$block_fingerprint <- metrics$block_fingerprint
  row$reference_block_fingerprint <- metrics$reference_block_fingerprint
  row$subset_row_fingerprint <- metrics$subset_row_fingerprint
  row$reference_subset_row_fingerprint <-
    metrics$reference_subset_row_fingerprint
  row$subset_block_fingerprint <- metrics$subset_block_fingerprint
  row$reference_subset_block_fingerprint <-
    metrics$reference_subset_block_fingerprint
  row$query_plan_fingerprint <- metrics$query_plan_fingerprint
  row$peak_rss_mb <- bench_peak_rss_mb()

  bench_append_row(result, row)
  bench_msg(
    "%s: hydrated startup %.2fs, rss %.0f MB, hot p50 %.4fs",
    backend,
    row$startup_secs,
    row$rss_mb,
    row$hot_p50_secs
  )
} else if (identical(command, "validate")) {
  # Validate a staged benchmark result set.

  args <- command_args
  if (length(args) < 1L) {
    stop("need <result_dir>", call. = FALSE)
  }
  result_dir <- args[1]
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }

  read_required <- function(name) {
    path <- file.path(result_dir, name)
    if (!file.exists(path)) {
      stop("missing staged result file: ", name, call. = FALSE)
    }
    utils::read.csv(path, stringsAsFactors = FALSE)
  }

  schedule <- read_required("05_schedule.csv")
  exports <- read_required("10_export.csv")
  access <- read_required("20_access.csv")
  crashes <- read_required("crashes.csv")
  manifest <- read_required("run_manifest.csv")
  source_manifest <- read_required("source_manifest.csv")
  resource_check <- read_required("resource_check.csv")
  preparation <- read_required("query_plan_manifest.csv")
  query_panel <- read_required("query_panel.csv")
  profile <- bench_profile(Sys.getenv("BENCH_PROFILE", "quick"))

  tier_key <- function(source, n_cells) {
    paste(source, sprintf("%.0f", as.numeric(n_cells)), sep = "\r")
  }

  resource_keys <- tier_key(resource_check$source, resource_check$n_cells)
  schedule_keys <- unique(tier_key(schedule$source, schedule$n_cells))
  if (!setequal(resource_keys, schedule_keys) || anyDuplicated(resource_keys)) {
    stop("resource_check.csv does not cover the scheduled tiers", call. = FALSE)
  }
  bench_require_safe_plan(resource_check)

  if (identical(Sys.getenv("BENCH_PROFILE"), "scale")) {
    if (
      !all(c("embedded_safe", "embedded_reason") %in% names(resource_check)) ||
        anyNA(resource_check$embedded_safe)
    ) {
      stop("scale results require embedded resource assessments", call. = FALSE)
    }
    embedded_rows <- exports[exports$backend == "embedded", , drop = FALSE]
    checks <- resource_check[
      match(
        tier_key(embedded_rows$source, embedded_rows$n_cells),
        resource_keys
      ),
      ,
      drop = FALSE
    ]
    bench_check_embedded_outcomes(
      embedded_rows$status,
      checks$embedded_safe,
      checks$embedded_reason
    )
  }

  if (
    !identical(names(manifest), c("key", "value")) ||
      anyDuplicated(manifest$key)
  ) {
    stop("run manifest must contain unique key/value rows", call. = FALSE)
  }
  manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
  # Runs created before the manifest-name fix inherited the DCF column name and
  # wrote `package_version.Version`. Accept that exact legacy spelling so a fully
  # completed acquisition can be finalized without repeating timed measurements.
  if (
    !"package_version" %in% names(manifest_values) &&
      "package_version.Version" %in% names(manifest_values)
  ) {
    manifest_values[["package_version"]] <-
      manifest_values[["package_version.Version"]]
  }
  required_manifest <- c(
    "study_id",
    "run_id",
    "profile",
    "generated_at",
    "git_sha",
    "git_branch",
    "git_dirty",
    "package_version",
    "r_version",
    "r_platform",
    "os",
    "cpu",
    "logical_cores",
    "benchmark_threads",
    "scratch_df",
    "storage_description",
    "memory_mb",
    "r_vector_limit_mb",
    "package_Matrix",
    "package_rhdf5",
    "package_BPCells",
    "package_HDF5Array",
    "package_CerebroNexus"
  )
  if (!all(required_manifest %in% names(manifest_values))) {
    stop("run manifest is missing required provenance", call. = FALSE)
  }
  required_values <- manifest_values[required_manifest]
  if (any(is.na(required_values) | !nzchar(trimws(required_values)))) {
    stop(
      "run manifest contains blank required provenance: ",
      paste(
        names(required_values)[
          is.na(required_values) | !nzchar(trimws(required_values))
        ],
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  if (!identical(manifest_values[["profile"]], profile$name)) {
    stop(
      "run manifest profile does not match validation profile",
      call. = FALSE
    )
  }
  if (!grepl("^[0-9a-f]{40}$", manifest_values[["git_sha"]])) {
    stop("run manifest has an invalid Git SHA", call. = FALSE)
  }
  if (
    isTRUE(profile$article_eligible) &&
      !identical(manifest_values[["git_dirty"]], "false")
  ) {
    stop(
      "benchmark evidence requires a clean Git worktree",
      call. = FALSE
    )
  }

  required_source_columns <- c("run_id", "source", "url", "bytes", "sha256")
  if (!all(required_source_columns %in% names(source_manifest))) {
    stop("source manifest is missing required columns", call. = FALSE)
  }
  if (!setequal(unique(schedule$source), unique(source_manifest$source))) {
    stop("source manifest does not cover the scheduled sources", call. = FALSE)
  }
  if (
    any(!is.finite(source_manifest$bytes) | source_manifest$bytes <= 0) ||
      any(!grepl("^[0-9a-fA-F]{64}$", source_manifest$sha256))
  ) {
    stop("source SHA-256 or byte size is invalid", call. = FALSE)
  }

  run_id <- manifest_values[["run_id"]]
  if (
    any(exports$run_id != run_id) ||
      any(access$run_id != run_id) ||
      any(source_manifest$run_id != run_id) ||
      any(preparation$run_id != run_id)
  ) {
    stop("result rows do not share the manifest run id", call. = FALSE)
  }

  plan_keys <- tier_key(schedule$source, schedule$n_cells)
  preparation_keys <- tier_key(preparation$source, preparation$n_cells)
  if (
    !setequal(unique(plan_keys), preparation_keys) ||
      anyDuplicated(preparation_keys) ||
      any(preparation$status != "OK") ||
      any(preparation$profile != profile$name)
  ) {
    stop("query-plan preparation does not cover the schedule", call. = FALSE)
  }
  required_panel_columns <- c(
    "run_id",
    "profile",
    "source",
    "n_cells",
    "panel_index",
    "gene",
    "nnz",
    "role",
    "query_plan_fingerprint",
    "reference_row_fingerprint",
    "reference_block_fingerprint"
  )
  subset_panel_columns <- c(
    "subset_n_cells",
    "subset_cells_fingerprint",
    "reference_subset_row_fingerprint",
    "reference_subset_block_fingerprint"
  )
  if (!all(required_panel_columns %in% names(query_panel))) {
    stop("query panel is missing required columns", call. = FALSE)
  }
  panel_keys <- tier_key(query_panel$source, query_panel$n_cells)
  panel_groups <- split(query_panel, panel_keys)
  validate_subset <- all(subset_panel_columns %in% names(query_panel))
  if (
    any(query_panel$run_id != run_id) ||
      any(query_panel$profile != profile$name) ||
      !setequal(names(panel_groups), unique(plan_keys)) ||
      any(vapply(panel_groups, nrow, integer(1)) != profile$query_genes) ||
      any(vapply(
        panel_groups,
        function(rows) {
          !identical(rows$panel_index, seq_len(nrow(rows))) ||
            anyDuplicated(rows$gene) ||
            any(is.na(rows$gene) | !nzchar(rows$gene)) ||
            sum(rows$role == "first") != 1L ||
            any(!rows$role %in% c("first", "hot")) ||
            any(!is.finite(rows$nnz) | rows$nnz <= 0) ||
            any(
              is.na(rows$query_plan_fingerprint) |
                !nzchar(rows$query_plan_fingerprint)
            ) ||
            any(
              is.na(rows$reference_row_fingerprint) |
                !nzchar(rows$reference_row_fingerprint)
            ) ||
            any(
              is.na(rows$reference_block_fingerprint) |
                !nzchar(rows$reference_block_fingerprint)
            ) ||
            length(unique(rows$query_plan_fingerprint)) != 1L ||
            length(unique(rows$reference_row_fingerprint)) != 1L ||
            length(unique(rows$reference_block_fingerprint)) != 1L ||
            (validate_subset &&
              (length(unique(rows$subset_n_cells)) != 1L ||
                any(
                  !is.finite(rows$subset_n_cells) | rows$subset_n_cells < 1L
                ) ||
                length(unique(rows$subset_cells_fingerprint)) != 1L ||
                length(unique(rows$reference_subset_row_fingerprint)) != 1L ||
                length(unique(rows$reference_subset_block_fingerprint)) != 1L))
        },
        logical(1)
      ))
  ) {
    stop("query panel does not match the fixed study protocol", call. = FALSE)
  }
  fingerprint_for <- function(data) {
    keys <- tier_key(data$source, data$n_cells)
    observed <- split(as.character(data$query_plan_fingerprint), keys)
    if (any(lengths(lapply(observed, unique)) != 1L)) {
      stop("query-plan fingerprint drifted within a tier", call. = FALSE)
    }
    sort(vapply(observed, function(x) unique(x)[1L], character(1)))
  }
  prepared_fingerprints <- fingerprint_for(preparation)
  successful_exports <- exports[
    !is.na(exports$status) & exports$status == "OK",
    ,
    drop = FALSE
  ]
  successful_access <- access[
    !is.na(access$status) & access$status == "OK",
    ,
    drop = FALSE
  ]
  fingerprints <- list(
    preparation = prepared_fingerprints,
    query_panel = fingerprint_for(query_panel),
    build = fingerprint_for(successful_exports),
    access = fingerprint_for(successful_access)
  )
  fingerprint_keys <- Reduce(union, lapply(fingerprints, names))
  fingerprint_matrix <- do.call(
    cbind,
    lapply(
      fingerprints,
      function(x) unname(x[match(fingerprint_keys, names(x))])
    )
  )
  fingerprint_mismatches <- fingerprint_keys[apply(
    fingerprint_matrix,
    1L,
    function(values) anyNA(values) || length(unique(values)) != 1L
  )]
  if (length(fingerprint_mismatches)) {
    stop(
      "query-plan fingerprint differs across preparation/build/access: ",
      paste(
        gsub("\r", " @ ", fingerprint_mismatches, fixed = TRUE),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (!identical(manifest_values[["timing_contract"]], "monotonic-v1")) {
    stop("new publication requires a monotonic-v1 rerun", call. = FALSE)
  }
  bench_validate_timing_evidence(
    exports,
    access,
    read_required("21_access_timings.csv")
  )
  bench_validate_results(schedule, exports, access, crashes, profile)
  message(
    sprintf(
      "validated %d exports and %d access processes for profile %s",
      nrow(exports),
      nrow(access),
      profile$name
    )
  )
} else if (identical(command, "report")) {
  # Generate an uncertainty-aware Markdown report from a staged or published run.
  #
  # Usage: Rscript benchmark_cli.R report <result_dir>

  args <- command_args
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/benchmarks/expression-backends/scripts")
  }

  result_dir <- if (length(args) >= 1L) {
    normalizePath(args[1], mustWork = TRUE)
  } else {
    bench_current_result_dir(file.path(
      here,
      "..",
      "..",
      "results",
      "expression-backends",
      "full"
    ))
  }

  read_if <- function(name) {
    path <- file.path(result_dir, name)
    if (file.exists(path)) {
      utils::read.csv(path, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  }

  probe <- read_if("00_probe.csv")
  exports <- read_if("10_export.csv")
  access <- read_if("20_access.csv")
  crashes <- read_if("crashes.csv")
  manifest <- read_if("run_manifest.csv")
  source_manifest <- read_if("source_manifest.csv")

  if (is.null(manifest)) {
    stop(
      "run_manifest.csv is required for a scientific benchmark report",
      call. = FALSE
    )
  }
  manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
  profile <- bench_profile(manifest_values[["profile"]])

  interval <- function(row, metric, digits = 2L) {
    bench_format_interval(
      row[[paste0(metric, "_median")]],
      row[[paste0(metric, "_min")]],
      row[[paste0(metric, "_max")]],
      row[[paste0(metric, "_n")]],
      digits = digits
    )
  }

  out <- c(
    "# Expression-backend benchmark on real public datasets",
    "",
    sprintf("**Run:** `%s`  ", manifest_values[["run_id"]]),
    sprintf("**Profile:** `%s`  ", profile$name),
    sprintf("**Git:** `%s`", manifest_values[["git_sha"]]),
    "",
    paste0("> **Evidence status.** ", bench_evidence_notice(profile)),
    ""
  )

  if (!is.null(probe) && nrow(probe)) {
    out <- c(
      out,
      "## Sources",
      "",
      "| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |",
      "|---|---:|---:|---:|---:|---:|:--:|"
    )
    for (i in seq_len(nrow(probe))) {
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %.3e | %.0f | %.1f GB | %s |",
          probe$label[i],
          format(probe$n_cells[i], big.mark = ","),
          format(probe$n_genes[i], big.mark = ","),
          probe$nnz[i],
          probe$nnz_per_cell[i],
          probe$dgc_gb_full[i],
          if (probe$dgc_representable[i]) "yes" else "**no**"
        )
      )
    }
    out <- c(out, "")
  }

  if (!is.null(exports) && nrow(exports)) {
    if (!"peak_rss_mb" %in% names(exports)) {
      exports$peak_rss_mb <- NA_real_
    }
    for (name in c("shell_secs", "serialize_secs")) {
      if (!name %in% names(exports)) {
        exports[[name]] <- NA_real_
      }
    }
    exports$r_peak_mb[exports$r_peak_mb > 4e6] <- NA_real_
    export_summary <- bench_summarise_metrics(
      exports,
      group = c("source", "n_cells", "backend"),
      metrics = c(
        "crb_mb",
        "sibling_mb",
        "total_mb",
        "export_secs",
        "shell_secs",
        "serialize_secs",
        "r_peak_mb",
        "peak_rss_mb"
      )
    )
    export_summary <- export_summary[
      order(
        export_summary$source,
        export_summary$n_cells,
        export_summary$backend
      ),
      ,
      drop = FALSE
    ]
    out <- c(
      out,
      "## Export",
      "",
      "Values are median [minimum-maximum], followed by the number of independent export processes.",
      "Embedded export includes the Seurat exporter, aggregations and serialization; disk-backend construction is timed separately from shell construction and serialization. These are different operation boundaries, not a direct backend-writer speed comparison.",
      "",
      paste0(
        "| source | cells | backend | total MiB | build/export s | shell s | ",
        "CRB serialization s | peak R heap MiB | peak process RSS MiB |"
      ),
      "|---|---:|---|---:|---:|---:|---:|---:|---:|"
    )
    for (i in seq_len(nrow(export_summary))) {
      row <- export_summary[i, , drop = FALSE]
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %s | %s | %s | %s | %s | %s |",
          row$source,
          format(row$n_cells, big.mark = ","),
          row$backend,
          interval(row, "total_mb", 1L),
          interval(row, "export_secs", 1L),
          interval(row, "shell_secs", 2L),
          interval(row, "serialize_secs", 2L),
          interval(row, "r_peak_mb", 0L),
          interval(row, "peak_rss_mb", 0L)
        )
      )
    }
    out <- c(out, "")

    failed <- exports[exports$status != "OK", , drop = FALSE]
    if (nrow(failed)) {
      out <- c(
        out,
        "### Caught export failures",
        "",
        "| source | cells | backend | repeat | status |",
        "|---|---:|---|---:|---|"
      )
      for (i in seq_len(nrow(failed))) {
        out <- c(
          out,
          sprintf(
            "| %s | %s | %s | %d | %s |",
            failed$source[i],
            format(failed$n_cells[i], big.mark = ","),
            failed$backend[i],
            failed$export_repeat[i],
            substr(failed$status[i], 1L, 100L)
          )
        )
      }
      out <- c(out, "")
    }
  }

  if (!is.null(access) && nrow(access)) {
    if (!"peak_rss_mb" %in% names(access)) {
      access$peak_rss_mb <- NA_real_
    }
    if (!"startup_secs" %in% names(access)) {
      access$startup_secs <- access$load_secs + access$attach_secs
    }
    if (!"subset_row_secs" %in% names(access)) {
      access$subset_row_secs <- NA_real_
      access$subset_block_secs <- NA_real_
    }
    access_summary <- bench_summarise_metrics(
      access,
      group = c("source", "n_cells", "backend"),
      metrics = c(
        "startup_secs",
        "rss_mb",
        "peak_rss_mb",
        "first_query_secs",
        "hot_p50_secs",
        "hot_p95_secs",
        "block_secs",
        "subset_row_secs",
        "subset_block_secs"
      )
    )
    access_summary <- access_summary[
      order(
        access_summary$source,
        access_summary$n_cells,
        access_summary$backend
      ),
      ,
      drop = FALSE
    ]
    out <- c(
      out,
      "## Runtime access",
      "",
      "Access counts describe fresh processes nested within independently built artifacts (two processes per build), not independent build replicates.",
      if (
        "access_contract" %in%
          names(access) &&
          all(
            !is.na(access$access_contract) &
              access$access_contract == "materialized-v2"
          )
      ) {
        "Timing contract: materialized-v2. All timed results are materialized before the timer stops."
      } else {
        "Historical timing contract: lazy blocks may have been materialized after the timer stopped. These block timings cannot support a completed-read comparison."
      },
      paste0(
        "The first-query metric is the first getter call in a fresh R process. ",
        "The operating-system file cache is uncontrolled, so it is not a cold-disk measurement."
      ),
      "",
      paste0(
        "| source | cells | backend | startup s | RSS MiB | ",
        "peak process RSS MiB | first query s | warmed p50 s | warmed p95 s | ",
        "12-gene full block s | shuffled 100k row s | shuffled 100k block s |"
      ),
      "|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
    )
    for (i in seq_len(nrow(access_summary))) {
      row <- access_summary[i, , drop = FALSE]
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
          row$source,
          format(row$n_cells, big.mark = ","),
          row$backend,
          interval(row, "startup_secs", 2L),
          interval(row, "rss_mb", 0L),
          interval(row, "peak_rss_mb", 0L),
          interval(row, "first_query_secs", 4L),
          interval(row, "hot_p50_secs", 4L),
          interval(row, "hot_p95_secs", 4L),
          interval(row, "block_secs", 3L),
          interval(row, "subset_row_secs", 4L),
          interval(row, "subset_block_secs", 3L)
        )
      )
    }
    out <- c(
      out,
      "",
      sprintf(
        "Correctness: %d/%d access processes matched both source-matrix fingerprints.",
        sum(access$correctness == "OK", na.rm = TRUE),
        nrow(access)
      ),
      ""
    )
  }

  if (!is.null(exports) && nrow(exports)) {
    usable <- exports[
      exports$backend == "embedded" &
        exports$status == "OK" &
        is.finite(exports$r_peak_mb) &
        !is.na(exports$nnz),
      ,
      drop = FALSE
    ]
    if (nrow(usable)) {
      ceiling_points <- bench_summarise_metrics(
        usable,
        group = c("source", "n_cells", "nnz"),
        metrics = "r_peak_mb"
      )
      ceiling_points$bytes_per_nnz <-
        ceiling_points$r_peak_mb_median * 2^20 / ceiling_points$nnz
      estimate <- stats::median(ceiling_points$bytes_per_nnz)
      out <- c(
        out,
        "## Host-specific scale-limit estimate",
        "",
        sprintf(
          paste0(
            "Across %d distinct source/tier points, the median observed peak was ",
            "%.1f bytes per non-zero (range %.1f-%.1f). This is a descriptive ",
            "estimate for this exporter and host, not a universal memory law."
          ),
          nrow(ceiling_points),
          estimate,
          min(ceiling_points$bytes_per_nnz),
          max(ceiling_points$bytes_per_nnz)
        ),
        ""
      )
    }
  }

  if (!is.null(crashes) && nrow(crashes)) {
    out <- c(
      out,
      "## Processes killed outright",
      "",
      "| source | cells | backend | repeat | stage | exit |",
      "|---|---:|---|---:|---|---:|"
    )
    for (i in seq_len(nrow(crashes))) {
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %d | %s | %d |",
          crashes$source[i],
          format(crashes$n_cells[i], big.mark = ","),
          crashes$backend[i],
          crashes$export_repeat[i],
          crashes$stage[i],
          crashes$exit_code[i]
        )
      )
    }
    out <- c(out, "")
  }

  out <- c(out, "## Provenance", "")
  provenance_keys <- c(
    "generated_at",
    "git_branch",
    "git_dirty",
    "package_version",
    "r_version",
    "os",
    "cpu",
    "logical_cores",
    "benchmark_threads",
    "slurm_job_id",
    "slurm_node_list",
    "slurm_cpus_per_task",
    "slurm_memory_per_node",
    "memory_mb",
    "r_vector_limit_mb"
  )
  out <- c(out, "| key | value |", "|---|---|")
  for (key in provenance_keys[provenance_keys %in% names(manifest_values)]) {
    out <- c(out, sprintf("| %s | %s |", key, manifest_values[[key]]))
  }
  if (!is.null(source_manifest)) {
    for (i in seq_len(nrow(source_manifest))) {
      out <- c(
        out,
        sprintf(
          "| source `%s` | %s bytes; SHA-256 `%s` |",
          source_manifest$source[i],
          format(source_manifest$bytes[i], big.mark = ","),
          source_manifest$sha256[i]
        )
      )
    }
  }
  out <- c(out, "")

  path <- file.path(result_dir, "summary.md")
  writeLines(out, path, useBytes = TRUE)
  cat(paste(out, collapse = "\n"), "\n")
  message("\nwrote ", path)
} else if (identical(command, "evidence")) {
  # Write a deterministic checksum inventory for a completed evidence package.

  args <- command_args
  if (length(args) != 1L) {
    stop("usage: benchmark_cli.R evidence <stage_dir>", call. = FALSE)
  }
  stage <- normalizePath(args[[1]], mustWork = TRUE)
  output <- file.path(stage, "evidence_manifest.csv")
  files <- list.files(
    stage,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[
    !dir.exists(files) &
      normalizePath(files) != normalizePath(output, mustWork = FALSE)
  ]
  relative <- substring(files, nchar(stage) + 2L)
  # Local process logs are gitignored and are not part of the portable evidence.
  portable <- !grepl("^logs/", gsub("\\\\", "/", relative))
  files <- files[portable]
  relative <- relative[portable]
  order <- order(relative, method = "radix")
  files <- files[order]
  relative <- relative[order]
  if (!length(files)) {
    stop("cannot inventory an empty evidence package", call. = FALSE)
  }
  info <- file.info(files)
  manifest <- data.frame(
    path = gsub("\\\\", "/", relative),
    bytes = as.numeric(info$size),
    md5 = unname(tools::md5sum(files)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, output, row.names = FALSE, na = "")
  message("wrote evidence inventory for ", nrow(manifest), " files")
} else if (identical(command, "check")) {
  # Check the complete staged evidence package before publishing it.
  #
  # Usage: Rscript benchmark_cli.R check <stage_dir>

  args <- command_args
  if (length(args) < 1L) {
    stop("need <stage_dir>", call. = FALSE)
  }
  stage <- normalizePath(args[1], mustWork = TRUE)
  required <- c(
    "00_probe.csv",
    "05_schedule.csv",
    "10_export.csv",
    "20_access.csv",
    "crashes.csv",
    "query_panel.csv",
    "query_plan_manifest.csv",
    "resource_check.csv",
    "run_manifest.csv",
    "source_manifest.csv",
    "summary.md",
    "evidence_manifest.csv"
  )
  paths <- file.path(stage, required)
  missing <- !file.exists(paths) |
    is.na(file.info(paths)$size) |
    file.info(paths)$size <= 0
  if (any(missing)) {
    stop("missing staged output: ", required[missing][1], call. = FALSE)
  }

  inventory <- utils::read.csv(
    file.path(stage, "evidence_manifest.csv"),
    stringsAsFactors = FALSE
  )
  if (
    !identical(names(inventory), c("path", "bytes", "md5")) ||
      !nrow(inventory) ||
      anyDuplicated(inventory$path) ||
      any(!grepl("^[0-9a-f]{32}$", inventory$md5)) ||
      any(!is.finite(inventory$bytes) | inventory$bytes < 0)
  ) {
    stop("evidence_manifest.csv is invalid", call. = FALSE)
  }
  inventoried_paths <- file.path(stage, inventory$path)
  if (any(!file.exists(inventoried_paths))) {
    stop("evidence inventory names a missing file", call. = FALSE)
  }
  actual_md5 <- unname(tools::md5sum(inventoried_paths))
  actual_bytes <- as.numeric(file.info(inventoried_paths)$size)
  if (
    any(actual_md5 != inventory$md5) || any(actual_bytes != inventory$bytes)
  ) {
    stop("evidence inventory does not match staged files", call. = FALSE)
  }
  required_inventory <- setdiff(
    gsub("\\\\", "/", required),
    "evidence_manifest.csv"
  )
  if (!all(required_inventory %in% inventory$path)) {
    stop(
      "evidence inventory does not cover all required outputs",
      call. = FALSE
    )
  }
  message("validated complete staged benchmark outputs")
} else if (identical(command, "publish")) {
  # Publish a validated result directory without replacing prior runs.

  args <- command_args
  if (length(args) < 3L) {
    stop("need <stage_dir> <result_root> <run_id>", call. = FALSE)
  }
  stage <- normalizePath(args[1], mustWork = TRUE)
  result_root <- args[2]
  run_id <- args[3]

  bench_publish_result(stage, result_root, run_id)
} else {
  stop("unknown benchmark command: ", command, call. = FALSE)
}
