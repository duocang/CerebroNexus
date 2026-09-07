## Guided Core stage loader.

.builder_core_stage_source_files <- unlist(lapply(
  sys.frames(),
  function(frame) get0("ofile", envir = frame, inherits = FALSE)
))
.builder_core_stage_source_files <- .builder_core_stage_source_files[
  is.character(.builder_core_stage_source_files) &
    nzchar(.builder_core_stage_source_files)
]
.builder_core_stage_sibling_dir <- if (
  length(.builder_core_stage_source_files)
) {
  file.path(
    dirname(tail(.builder_core_stage_source_files, 1L)),
    "core_stage"
  )
} else {
  ""
}
.builder_core_stage_source_candidates <- unique(c(
  .builder_core_stage_sibling_dir,
  file.path(getwd(), "ui", "core_stage"),
  file.path(getwd(), "inst", "builder", "ui", "core_stage"),
  system.file(
    "builder",
    "ui",
    "core_stage",
    package = "CerebroNexus"
  )
))
.builder_core_stage_source_candidates <-
  .builder_core_stage_source_candidates[
    nzchar(.builder_core_stage_source_candidates)
  ]
.builder_core_stage_source_matches <-
  .builder_core_stage_source_candidates[
    dir.exists(.builder_core_stage_source_candidates)
  ]
if (!length(.builder_core_stage_source_matches)) {
  stop("Builder Core stage source location is unavailable.", call. = FALSE)
}
.builder_core_stage_source_dir <-
  .builder_core_stage_source_matches[[1L]]
for (.builder_core_stage_source_name in c(
  "assay.R",
  "groups.R",
  "projections.R",
  "trajectory.R",
  "specialized.R",
  "stage.R"
)) {
  sys.source(
    file.path(
      .builder_core_stage_source_dir,
      .builder_core_stage_source_name
    ),
    envir = environment()
  )
}
rm(
  .builder_core_stage_sibling_dir,
  .builder_core_stage_source_candidates,
  .builder_core_stage_source_dir,
  .builder_core_stage_source_files,
  .builder_core_stage_source_matches,
  .builder_core_stage_source_name
)
