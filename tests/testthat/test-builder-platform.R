builder_repo_source("io.R", local = globalenv())

test_that("Windows native pickers run outside the Shiny process", {
  rscript <- "C:/R/bin/Rscript.exe"
  files <- builder_native_picker_spec(
    "dataset_files",
    .system = "Windows",
    .rscript = rscript
  )
  folder <- builder_native_picker_spec(
    "project_directory",
    .system = "Windows",
    .rscript = rscript
  )

  expect_identical(files$command, rscript)
  expect_identical(folder$command, rscript)
  expect_false("select" %in% names(files))
  expect_false("select" %in% names(folder))
  expect_silent(parse(text = builder_windows_picker_script()))
  expect_match(paste(files$args, collapse = "\n"), "choose.files", fixed = TRUE)
  expect_match(paste(folder$args, collapse = "\n"), "choose.dir", fixed = TRUE)
})

test_that("macOS and Linux picker specs keep their native cancellation contracts", {
  macos <- builder_native_picker_spec("project_manifest", .system = "Darwin")
  zenity <- builder_native_picker_spec(
    "table_files",
    .system = "Linux",
    .which = function(command) {
      if (identical(command, "zenity")) "/usr/bin/zenity" else ""
    }
  )
  kdialog <- builder_native_picker_spec(
    "project_directory",
    .system = "Linux",
    .which = function(command) {
      if (identical(command, "kdialog")) "/usr/bin/kdialog" else ""
    }
  )

  expect_identical(macos$command, "osascript")
  expect_identical(macos$cancel_status, integer())
  expect_match(
    paste(macos$args, collapse = "\n"),
    "on error number -128",
    fixed = TRUE
  )
  expect_identical(zenity$command, "/usr/bin/zenity")
  expect_identical(zenity$cancel_status, 1L)
  expect_true(all(c("--multiple", "--separator=\n") %in% zenity$args))
  expect_identical(kdialog$command, "/usr/bin/kdialog")
  expect_identical(kdialog$cancel_status, 1L)
  expect_true("--getexistingdirectory" %in% kdialog$args)
})

builder_platform_picker_process <- function(
  status,
  output = character(),
  errors = character()
) {
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() FALSE
  process$get_exit_status <- function() status
  process$read_all_output_lines <- function() output
  process$read_all_error_lines <- function() errors
  process
}

test_that("native picker failures cannot masquerade as a selection or cancellation", {
  root <- withr::local_tempdir()
  failed_with_output <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(2L, root, "dialog failed"),
    result = NULL,
    cancel_status = 1L
  ))
  failed_without_detail <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(2L),
    result = NULL,
    cancel_status = 1L
  ))
  cancelled <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(1L),
    result = NULL,
    cancel_status = 1L
  ))

  expect_identical(failed_with_output$status, "error")
  expect_match(failed_with_output$error, "dialog failed", fixed = TRUE)
  expect_identical(failed_without_detail$status, "error")
  expect_match(failed_without_detail$error, "status 2", fixed = TRUE)
  expect_identical(cancelled, list(status = "cancelled", path = NULL))
})
