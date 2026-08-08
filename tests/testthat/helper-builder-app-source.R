builder_app_source_files <- c(
  "app.R",
  file.path("server", "foundation.R"),
  file.path("server", "imports.R"),
  file.path("server", "datasets.R"),
  file.path("server", "enhancements.R"),
  file.path("server", "review.R"),
  file.path("server", "build.R")
)

builder_app_source_paths <- function() {
  vapply(
    builder_app_source_files,
    function(file) builder_profile_inst_path("builder", file),
    character(1),
    USE.NAMES = FALSE
  )
}

builder_app_source_lines <- function() {
  unlist(
    lapply(builder_app_source_paths(), readLines, warn = FALSE),
    use.names = FALSE
  )
}

builder_app_source_text <- function() {
  paste(builder_app_source_lines(), collapse = "\n")
}
