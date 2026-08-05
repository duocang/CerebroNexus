# Validate a staged benchmark result set.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("need <result_dir>", call. = FALSE)
}
result_dir <- args[1]
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tools/bench")
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

if (
  !identical(names(manifest), c("key", "value")) || anyDuplicated(manifest$key)
) {
  stop("run manifest must contain unique key/value rows", call. = FALSE)
}
manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
required_manifest <- c(
  "run_id",
  "profile",
  "git_sha",
  "dependency_environment",
  "dependency_environment_git_blob"
)
if (!all(required_manifest %in% names(manifest_values))) {
  stop(
    "run manifest is missing dependency environment provenance",
    call. = FALSE
  )
}
if (!identical(manifest_values[["profile"]], profile$name)) {
  stop("run manifest profile does not match validation profile", call. = FALSE)
}
bench_require_safe_plan(resource_check, profile = profile)
if (!grepl("^[0-9a-f]{40}$", manifest_values[["git_sha"]])) {
  stop("run manifest has an invalid Git SHA", call. = FALSE)
}
if (
  !identical(manifest_values[["dependency_environment"]], "default.nix") ||
    !grepl(
      "^[0-9a-f]{40}$",
      manifest_values[["dependency_environment_git_blob"]]
    )
) {
  stop(
    "run manifest has invalid dependency environment provenance",
    call. = FALSE
  )
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
    any(source_manifest$run_id != run_id)
) {
  stop("result rows do not share the manifest run id", call. = FALSE)
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
