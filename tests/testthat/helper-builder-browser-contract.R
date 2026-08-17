builder_browser_contract_app <- function(
  app_dir,
  contract_version,
  .local_envir = parent.frame()
) {
  stopifnot(contract_version %in% c(0L, 1L))
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  fixture_root <- tempfile("builder-contract-")
  fixture_dir <- file.path(fixture_root, "builder")
  viewer_dir <- file.path(dirname(app_dir), "viewer")
  if (!dir.exists(viewer_dir)) {
    stop("Builder Viewer support is unavailable.", call. = FALSE)
  }
  dir.create(fixture_root, recursive = TRUE)
  fs::dir_copy(app_dir, fixture_dir)
  fs::dir_copy(viewer_dir, file.path(fixture_root, "viewer"))
  withr::defer(
    unlink(fixture_root, recursive = TRUE, force = TRUE),
    envir = .local_envir
  )

  prerequisite <- file.path(fixture_dir, "prerequisite.R")
  writeLines(
    c(
      readLines(prerequisite, warn = FALSE),
      "",
      "## Test fixture: fix the installed Viewer contract in this process.",
      sprintf(
        "builder_installed_app_contract_version <- function(...) %dL",
        as.integer(contract_version)
      )
    ),
    prerequisite
  )
  fixture_dir
}

builder_browser_current_contract_app <- function(
  app_dir,
  .local_envir = parent.frame()
) {
  builder_browser_contract_app(app_dir, 1L, .local_envir)
}

builder_browser_old_contract_app <- function(
  app_dir,
  .local_envir = parent.frame()
) {
  builder_browser_contract_app(app_dir, 0L, .local_envir)
}

builder_expect_clean_browser_logs <- function(app) {
  logs <- app$get_logs()
  failures <- logs[
    as.character(logs$location) == "chromote" &
      tolower(as.character(logs$level)) %in%
        c("error", "warning", "assert", "throw"),
    ,
    drop = FALSE
  ]
  expect_identical(
    nrow(failures),
    0L,
    info = paste(failures$message, collapse = "\n")
  )
}
