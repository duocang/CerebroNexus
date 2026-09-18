builder_repo_source("io.R", local = globalenv())

test_that("native pickers are reserved for projects and output folders", {
  expect_identical(
    builder_native_picker_kinds(),
    c("output_directory", "project_directory", "project_manifest")
  )
  expect_identical(
    eval(formals(builder_native_picker_spec)$kind),
    c("output_directory", "project_directory", "project_manifest")
  )
  expect_identical(
    eval(formals(builder_start_native_picker)$kind),
    c("output_directory", "project_directory", "project_manifest")
  )
  expect_error(
    builder_native_picker_result("dataset_files", function() character()),
    "should be one of"
  )
})

test_that("Windows native pickers use modern STA shell dialogs", {
  powershell <- "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
  folder <- builder_native_picker_spec(
    "project_directory",
    .system = "Windows",
    .powershell = powershell
  )

  expect_identical(folder$command, powershell)
  expect_false("select" %in% names(folder))
  expect_true("-STA" %in% folder$args)
  expect_identical(folder$encoding, "UTF-8")
  expect_match(
    paste(folder$args, collapse = "\n"),
    "IFileOpenDialog",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_manifest"),
    "OpenFileDialog",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_manifest"),
    "$dialog.AutoUpgradeEnabled = $true",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("output_directory"),
    "[BuilderModernFolderPicker]::Pick",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_directory"),
    "FileOpenOptions.PickFolders",
    fixed = TRUE
  )
  expect_false(grepl(
    "FolderBrowserDialog",
    builder_windows_picker_script("project_directory"),
    fixed = TRUE
  ))
  pwsh <- builder_native_picker_spec(
    "output_directory",
    .system = "Windows",
    .which = function(command) {
      if (identical(command, "pwsh.exe")) {
        "C:/Program Files/PowerShell/7/pwsh.exe"
      } else {
        ""
      }
    }
  )
  expect_identical(
    pwsh$command,
    "C:/Program Files/PowerShell/7/pwsh.exe"
  )
})

test_that("every native picker has one typed selection contract", {
  contracts <- lapply(
    builder_native_picker_kinds(),
    builder_native_picker_contract
  )

  expect_identical(
    vapply(contracts, `[[`, character(1), "kind"),
    builder_native_picker_kinds()
  )
  expect_identical(
    vapply(contracts, `[[`, character(1), "type"),
    c("directory", "directory", "file")
  )
  expect_true(all(vapply(
    contracts,
    function(contract) nzchar(contract$prompt),
    logical(1)
  )))
})

test_that("browser upload accept lists share backend format contracts", {
  expect_identical(
    builder_file_accept(builder_dataset_extensions()),
    ".rds,.qs2,.qs"
  )
  expect_identical(
    builder_file_accept(builder_table_extensions()),
    ".csv,.tsv,.txt,.xls,.xlsx,.xlsm"
  )
  expect_identical(
    builder_file_accept(builder_image_extensions()),
    ".png,.jpg,.jpeg"
  )
})

test_that("all server-filesystem buttons use the native picker boundary", {
  builder_root <- dirname(.builder_io_source_path)
  server_root <- file.path(builder_root, "server")
  server_code <- paste(
    vapply(
      list.files(server_root, pattern = "[.]R$", full.names = TRUE),
      function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )
  calls <- regmatches(
    server_code,
    gregexpr(
      'builder_schedule_native_picker\\(\\s*"[^"]+"',
      server_code,
      perl = TRUE
    )
  )[[1L]]
  kinds <- sub('.*"([^"]+)"$', "\\1", calls)

  expect_identical(
    sort(kinds),
    sort(builder_native_picker_kinds())
  )
})

test_that("browser uploads stay on HTML file transports", {
  builder_root <- dirname(.builder_io_source_path)
  app <- paste(
    readLines(
      file.path(builder_root, "app.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  marker <- paste(
    readLines(
      file.path(builder_root, "ui", "marker_import.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  enhance <- paste(
    readLines(
      file.path(builder_root, "ui", "enhance_stage.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(
      file.path(builder_root, "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    app,
    "builder_file_accept(builder_dataset_extensions())",
    fixed = TRUE
  )
  expect_match(
    marker,
    "builder_file_accept(builder_table_extensions())",
    fixed = TRUE
  )
  expect_gte(
    lengths(regmatches(
      enhance,
      gregexpr("builder_file_accept", enhance, fixed = TRUE)
    )),
    2L
  )
  expect_match(javascript, 'picker.value = "";', fixed = TRUE)
  expect_match(javascript, ".enhance-tissue-file-button", fixed = TRUE)
})

test_that("macOS specs cover every directory and file picker", {
  specs <- lapply(builder_native_picker_kinds(), function(kind) {
    builder_native_picker_spec(kind, .system = "Darwin")
  })

  expect_true(all(vapply(
    specs,
    function(spec) {
      identical(spec$command, "osascript") &&
        identical(spec$cancel_status, integer()) &&
        identical(spec$encoding, "UTF-8")
    },
    logical(1)
  )))
  expect_true(all(vapply(
    specs,
    function(spec) {
      grepl(
        "on error number -128",
        paste(spec$args, collapse = "\n"),
        fixed = TRUE
      )
    },
    logical(1)
  )))
  expect_true(all(vapply(
    specs[1:2],
    function(spec) {
      grepl("choose folder", paste(spec$args, collapse = "\n"), fixed = TRUE)
    },
    logical(1)
  )))
  expect_match(
    paste(specs[[3L]]$args, collapse = "\n"),
    'choose file with prompt "Open a Builder project." of type {"json"}',
    fixed = TRUE
  )
})

test_that("Linux Zenity and KDialog specs cover every picker", {
  which_zenity <- function(command) {
    if (identical(command, "zenity")) "/usr/bin/zenity" else ""
  }
  which_kdialog <- function(command) {
    if (identical(command, "kdialog")) "/usr/bin/kdialog" else ""
  }
  zenity <- lapply(builder_native_picker_kinds(), function(kind) {
    builder_native_picker_spec(
      kind,
      .system = "Linux",
      .which = which_zenity
    )
  })
  kdialog <- lapply(builder_native_picker_kinds(), function(kind) {
    builder_native_picker_spec(
      kind,
      .system = "Linux",
      .which = which_kdialog
    )
  })

  expect_true(all(vapply(
    c(zenity, kdialog),
    function(spec) {
      identical(spec$cancel_status, 1L) &&
        identical(spec$encoding, "UTF-8")
    },
    logical(1)
  )))
  expect_true(all(vapply(
    zenity[1:2],
    function(spec) {
      "--directory" %in% spec$args
    },
    logical(1)
  )))
  expect_true("--file-filter=Builder project | *.json" %in% zenity[[3L]]$args)
  expect_true(all(vapply(
    kdialog[1:2],
    function(spec) {
      "--getexistingdirectory" %in% spec$args
    },
    logical(1)
  )))
  expect_true("--getopenfilename" %in% kdialog[[3L]]$args)
  expect_true("Builder project (*.json)" %in% kdialog[[3L]]$args)
  expect_true(all(vapply(
    c(zenity, kdialog),
    function(spec) {
      "--title" %in% spec$args || any(startsWith(spec$args, "--title="))
    },
    logical(1)
  )))
})

test_that("Linux without a desktop picker fails into the server-path fallback", {
  expect_error(
    builder_native_picker_spec(
      "output_directory",
      .system = "Linux",
      .which = function(command) ""
    ),
    "No system picker is available",
    fixed = TRUE
  )
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

test_that("native picker cancellation is explicit and empty success is an error", {
  marker <- builder_native_picker_cancel_marker()
  cancelled <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(0L, marker),
    result = NULL,
    cancel_status = integer()
  ))
  empty <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(0L),
    result = NULL,
    cancel_status = integer()
  ))

  expect_identical(cancelled, list(status = "cancelled", path = NULL))
  expect_identical(empty$status, "error")
  expect_true(empty$fallback)
  expect_match(empty$error, "without returning a selection", fixed = TRUE)
  expect_match(
    builder_macos_picker_script("output_directory"),
    marker,
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("output_directory"),
    marker,
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_manifest"),
    marker,
    fixed = TRUE
  )
})

test_that("native picker subprocess hides the Windows console host", {
  arguments <- NULL
  process <- builder_platform_picker_process(1L)
  started <- builder_start_native_picker(
    "output_directory",
    .spec = list(command = "picker", args = character()),
    .process_new = function(...) {
      arguments <<- list(...)
      process
    }
  )

  expect_identical(started$process, process)
  expect_true(arguments$windows_hide_window)
})
