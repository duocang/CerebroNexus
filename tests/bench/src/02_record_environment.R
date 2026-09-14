# Write benchmark run provenance.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("need <result.csv>", call. = FALSE)
}
result <- args[1]
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
repo <- normalizePath(file.path(here, "..", ".."))

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
tracked_status <- git_value("status", "--porcelain", "--untracked-files=no")
untracked_status <- git_value(
  "ls-files",
  "--others",
  "--exclude-standard",
  "--",
  ".",
  ":(exclude,glob)tests/bench/result/**"
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
  git_sha = git_value("rev-parse", "HEAD"),
  git_branch = git_value("branch", "--show-current"),
  git_dirty = if (nzchar(status)) "true" else "false",
  package_version = description[1, "Version"],
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
manifest <- c(
  manifest,
  stats::setNames(
    vapply(packages, package_version_or_na, character(1)),
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
