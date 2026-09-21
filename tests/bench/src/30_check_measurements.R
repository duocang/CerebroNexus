# Validate a staged benchmark result set.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("need <result_dir>", call. = FALSE)
}
result_dir <- args[1]
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "lib", "resource_planning.R"))

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

if (
  !identical(names(manifest), c("key", "value")) || anyDuplicated(manifest$key)
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
  "study_id", "run_id", "profile", "generated_at", "git_sha", "git_branch",
  "git_dirty", "package_version", "r_version", "r_platform", "os", "cpu",
  "logical_cores", "benchmark_threads", "scratch_df",
  "storage_description", "memory_mb", "r_vector_limit_mb",
  "package_Matrix", "package_rhdf5", "package_BPCells", "package_HDF5Array",
  "package_CerebroNexus"
)
if (!all(required_manifest %in% names(manifest_values))) {
  stop("run manifest is missing required provenance", call. = FALSE)
}
required_values <- manifest_values[required_manifest]
if (any(is.na(required_values) | !nzchar(trimws(required_values)))) {
  stop(
    "run manifest contains blank required provenance: ",
    paste(names(required_values)[is.na(required_values) | !nzchar(trimws(required_values))], collapse = ", "),
    call. = FALSE
  )
}
if (!identical(manifest_values[["profile"]], profile$name)) {
  stop("run manifest profile does not match validation profile", call. = FALSE)
}
if (!grepl("^[0-9a-f]{40}$", manifest_values[["git_sha"]])) {
  stop("run manifest has an invalid Git SHA", call. = FALSE)
}
if (
  isTRUE(profile$article_eligible) &&
    !identical(manifest_values[["git_dirty"]], "false")
) {
  stop(
    "publication-profile evidence requires a clean Git worktree",
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
if (identical(profile$name, "panel_c2")) {
  required_panel_columns <- c(required_panel_columns, subset_panel_columns)
}
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
              any(!is.finite(rows$subset_n_cells) | rows$subset_n_cells < 1L) ||
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
fingerprints <- list(
  preparation = prepared_fingerprints,
  query_panel = fingerprint_for(query_panel),
  build = fingerprint_for(exports),
  access = fingerprint_for(access)
)
fingerprint_keys <- Reduce(union, lapply(fingerprints, names))
fingerprint_matrix <- vapply(
  fingerprints,
  function(x) unname(x[match(fingerprint_keys, names(x))]),
  character(length(fingerprint_keys))
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

bench_validate_results(schedule, exports, access, crashes, profile)
message(
  sprintf(
    "validated %d exports and %d access processes for profile %s",
    nrow(exports),
    nrow(access),
    profile$name
  )
)
