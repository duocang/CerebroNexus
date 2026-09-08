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

resource_keys <- paste(
  resource_check$source,
  resource_check$n_cells,
  sep = "\r"
)
schedule_keys <- unique(paste(schedule$source, schedule$n_cells, sep = "\r"))
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
required_manifest <- c("run_id", "profile", "git_sha")
if (!all(required_manifest %in% names(manifest_values))) {
  stop("run manifest is missing required provenance", call. = FALSE)
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

plan_keys <- paste(schedule$source, schedule$n_cells, sep = "\r")
preparation_keys <- paste(preparation$source, preparation$n_cells, sep = "\r")
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
if (!all(required_panel_columns %in% names(query_panel))) {
  stop("query panel is missing required columns", call. = FALSE)
}
panel_keys <- paste(query_panel$source, query_panel$n_cells, sep = "\r")
panel_groups <- split(query_panel, panel_keys)
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
          length(unique(rows$reference_block_fingerprint)) != 1L
      },
      logical(1)
    ))
) {
  stop("query panel does not match the fixed study protocol", call. = FALSE)
}
fingerprint_for <- function(data) {
  keys <- paste(data$source, data$n_cells, sep = "\r")
  observed <- split(as.character(data$query_plan_fingerprint), keys)
  if (any(lengths(lapply(observed, unique)) != 1L)) {
    stop("query-plan fingerprint drifted within a tier", call. = FALSE)
  }
  sort(vapply(observed, function(x) unique(x)[1L], character(1)))
}
prepared_fingerprints <- fingerprint_for(preparation)
if (
  !identical(prepared_fingerprints, fingerprint_for(query_panel)) ||
    !identical(prepared_fingerprints, fingerprint_for(exports)) ||
    !identical(prepared_fingerprints, fingerprint_for(access))
) {
  stop(
    "query-plan fingerprint differs across preparation/build/access",
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
