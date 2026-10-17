test_that("import callbacks do not serialize the live Builder environment", {
  runtime <- new.env(parent = globalenv())
  for (file in c("project.R", "session.R")) {
    builder_repo_source(file, local = runtime)
  }
  runtime$large_session_value <- raw(2e6)
  callbacks <- lapply(
    list(
      importer = runtime$builder_project_load_retained_source,
      copy = runtime$builder_project_copy_file,
      marker = runtime$builder_project_transport_retained_write
    ),
    runtime$builder_session_worker_callback
  )
  callbacks$collect <- runtime$.builder_session_collect_failed_import
  expect_lt(length(serialize(callbacks, NULL)), 50000L)
  root <- withr::local_tempdir()
  source <- file.path(root, "source.rds")
  saveRDS(list(value = 42L), source)
  result <- callr::r(
    function(callbacks, root, source) {
      builder_seurat_file_adapter <<- function(path) readRDS(path)
      .builder_register_adapter <<- function(adapter, id, progress) adapter
      target <- file.path(root, "retained", "source.rds")
      marker <- file.path(root, "retained.rds")
      value <- callbacks$importer(
        "ds1",
        list(
          source = source,
          retained_path = target,
          transport_retained_path = marker
        ),
        NULL,
        .copy = callbacks$copy,
        .transport_writer = callbacks$marker
      )
      collected <- FALSE
      callbacks$collect(function(full) {
        collected <<- full
      })
      list(value = value$value, marker = readRDS(marker), collected = collected)
    },
    args = list(callbacks = callbacks, root = root, source = source)
  )
  expect_identical(result$value, 42L)
  expect_identical(result$marker$id, "ds1")
  expect_true(file.exists(result$marker$retained_path))
  expect_true(result$collected)
})

test_that("background project writes preserve revision conflicts and retry", {
  runtime <- new.env(parent = globalenv())
  builder_repo_source("prerequisite.R", local = runtime)
  files <- vapply(
    c("core/bundle_path_contract.R", "io.R", "project.R"),
    function(path) normalizePath(builder_test_inst_path("builder", path)),
    character(1)
  )
  for (path in files) {
    runtime$builder_source_utf8(path, runtime)
  }
  root <- withr::local_tempdir()
  manifest <- runtime$builder_project_new_manifest(
    root,
    "Background transaction"
  )
  write <- function(value, revision) {
    process <- runtime$builder_project_write_process(
      value,
      root,
      revision,
      files,
      runtime$builder_source_utf8
    )
    process$wait(30000)
    expect_false(process$is_alive())
    process$get_result()
  }
  first <- write(manifest, 0L)
  expect_equal(first$manifest$project$revision, 1L)
  expect_true(file.exists(first$path))
  expect_error(write(manifest, 0L), "updated by another Builder window")
  second <- write(first$manifest, 1L)
  expect_equal(second$manifest$project$revision, 2L)
  expect_equal(second$manifest$project$name, "Background transaction")
})

test_that("WSL picker paths preserve spaces and reject failed translation", {
  builder_repo_source("io.R", local = environment())
  selected <- c("C:\\Data space\\sample.rds", "D:\\研究\\builder-project.json")
  calls <- 0L
  converted <- builder_wsl_picker_paths(
    selected,
    .run = function(command, args, ...) {
      calls <<- calls + 1L
      # One shell converts every path, each as its own argument.
      expect_identical(command, "sh")
      expect_identical(args[[1]], "-c")
      expect_match(args[[2]], 'wslpath -u "$path"', fixed = TRUE)
      paths <- args[-(1:3)]
      expect_identical(paths, selected)
      list(
        status = 0L,
        stdout = paste0(
          "/mnt/",
          tolower(substr(paths, 1, 1)),
          gsub("\\", "/", substring(paths, 3), fixed = TRUE),
          "\n",
          collapse = ""
        )
      )
    }
  )
  expect_identical(calls, 1L)
  expect_identical(
    converted,
    c("/mnt/c/Data space/sample.rds", "/mnt/d/研究/builder-project.json")
  )
  expect_identical(builder_wsl_picker_paths(character()), character())
  expect_error(
    builder_wsl_picker_paths(selected, .run = function(...) {
      list(status = 1L, stdout = "")
    }),
    "Cannot access"
  )
  expect_error(
    builder_wsl_picker_paths(selected, .run = function(...) {
      list(status = 0L, stdout = "/mnt/c/only-one\n")
    }),
    "Cannot access"
  )
  spec <- builder_native_picker_spec(
    "project_manifest",
    .system = "Linux",
    .wsl = TRUE,
    .which = function(command) paste0("/bin/", command)
  )
  expect_identical(spec$path_style, "windows")
  expect_true("-EncodedCommand" %in% spec$args)
  expect_true(length(base64enc::base64decode(tail(spec$args, 1))) > 0L)
  root <- withr::local_tempdir()
  manifest <- file.path(root, "builder-project.json")
  writeLines("{}", manifest)
  expect_identical(
    builder_server_path_resolve(
      "C:\\Project files\\builder-project.json",
      "file",
      roots = root,
      .wsl = TRUE,
      .translate = function(value) {
        expect_identical(value, "C:\\Project files\\builder-project.json")
        manifest
      }
    ),
    as.character(fs::path_real(manifest))
  )
  dir.create(file.path(root, "restricted"))
  expect_error(
    builder_server_path_resolve(
      "C:\\Project files\\builder-project.json",
      "file",
      roots = file.path(root, "restricted"),
      .wsl = TRUE,
      .translate = function(value) manifest
    ),
    "allowed server folders"
  )
})

test_that("a real attachment worker verifies images and reports changed sources", {
  skip_if_not_installed("png")
  runtime <- new.env(parent = baseenv())
  builder_repo_source("prerequisite.R", local = runtime)
  files <- vapply(
    c("io.R", "project.R", "extras.R"),
    function(path) normalizePath(builder_test_inst_path("builder", path)),
    character(1)
  )
  for (path in files) {
    runtime$builder_source_utf8(path, runtime)
  }
  root <- withr::local_tempdir()
  image <- file.path(root, "source.png")
  png::writePNG(matrix(0.5, 8, 8), image)
  entries <- list(list(
    id = "ds1",
    settings = list(
      images = list(
        section = list(histology = list(source_path = image))
      )
    )
  ))
  jobs <- runtime$builder_project_attachment_jobs(entries, root)
  expect_length(jobs, 1L)
  run <- function(jobs) {
    process <- callr::r_bg(
      function(jobs, root, files, source_utf8) {
        isolated <- new.env(parent = baseenv())
        for (path in files) {
          source_utf8(path, isolated)
        }
        isolated$builder_project_stage_attachment_jobs(jobs, root)
      },
      args = list(jobs, root, files, runtime$builder_source_utf8)
    )
    process$wait(30000)
    process$get_result()
  }
  results <- run(jobs)
  expect_true(runtime$builder_project_attachment_results_match(jobs, results))
  saved <- runtime$builder_project_apply_attachment_results(
    entries,
    results,
    root
  )
  expect_true(file.exists(
    saved[[1]]$settings$images$section$histology$source_path
  ))
  writeBin(charToRaw("changed source"), image)
  expect_error(run(jobs), "changed before saving")
})

test_that("background Save restores display settings and reuses unchanged configuration", {
  runtime <- new.env(parent = globalenv())
  builder_repo_source("prerequisite.R", local = runtime)
  files <- vapply(
    c("core/bundle_path_contract.R", "io.R", "project.R", "extras.R"),
    function(path) normalizePath(builder_test_inst_path("builder", path)),
    character(1)
  )
  for (path in files) {
    runtime$builder_source_utf8(path, runtime)
  }
  root <- withr::local_tempdir()
  source <- file.path(root, "sample.rds")
  saveRDS(list(data = 1:3), source)
  entry <- list(
    id = "ds1",
    revision = 1L,
    path = source,
    filename = "sample.rds",
    settings = list(
      name = "Round trip",
      overview_point_size = 7,
      overview_point_opacity = 0.45,
      viewer_display = list(font_family = "Georgia", text_size = 16)
    ),
    snapshot = list()
  )
  manifest <- runtime$builder_project_new_manifest(root, "Round trip")
  context <- list(
    checked = "ds1",
    artifacts = list(),
    skipped_ids = character(),
    configuration = list(make_app = TRUE),
    last_ui = list(selected_dataset = "ds1")
  )
  run <- function(manifest, entries) {
    process <- runtime$builder_project_write_process(
      manifest,
      root,
      manifest$project$revision,
      files,
      runtime$builder_source_utf8,
      entries = entries,
      context = context
    )
    process$wait(30000)
    process$get_result()
  }
  first <- run(manifest, list(entry))
  restored <- runtime$builder_project_read_dataset_config(
    first$manifest$datasets[[1]],
    root
  )
  expect_identical(restored$settings, entry$settings)
  expect_true(first$manifest$datasets[[1]]$configuration$checked)
  expect_identical(first$manifest$last_ui, context$last_ui)
  second <- run(first$manifest, first$built$entries)
  expect_identical(
    second$manifest$datasets[[1]]$configuration,
    first$manifest$datasets[[1]]$configuration
  )
  expect_equal(second$manifest$project$revision, 2L)
})
