builder_project_test_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "inst", "builder", "prerequisite.R"),
    envir = runtime
  )
  runtime$builder_source_utf8(
    testthat::test_path(
      "..",
      "..",
      "inst",
      "viewer",
      "core",
      "spatial_coordinate_transform.R"
    ),
    envir = runtime
  )
  for (file in c("io.R", "worker.R", "extras.R", "project.R", "build.R")) {
    path <- testthat::test_path("..", "..", "inst", "builder", file)
    runtime$builder_source_utf8(path, envir = runtime)
  }
  runtime
}

## Direct test_file() runs on Windows can start in the C locale. Keep static
## source assertions on the same UTF-8 contract as the Builder runtime.
readLines <- function(
  con,
  n = -1L,
  ok = TRUE,
  warn = TRUE,
  encoding = "UTF-8",
  skipNul = FALSE
) {
  base::readLines(
    con,
    n = n,
    ok = ok,
    warn = warn,
    encoding = encoding,
    skipNul = skipNul
  )
}

builder_project_test_manifest <- function(ids = character()) {
  list(
    schema_version = 1L,
    project = list(id = "project-safe", revision = 0L),
    datasets = lapply(ids, function(id) list(id = id))
  )
}

test_that("project dataset ids are allocated without reusing restored ids", {
  runtime <- builder_project_test_runtime()

  allocated <- runtime$builder_project_allocate_dataset_id(
    sequence = 0L,
    existing_ids = c("ds1", "ds2")
  )
  expect_identical(allocated, list(id = "ds3", sequence = 3L))

  restored <- runtime$builder_project_allocate_dataset_id(
    sequence = 1L,
    existing_ids = "ds1",
    restored_id = "ds7"
  )
  expect_identical(restored, list(id = "ds7", sequence = 7L))
  expect_error(
    runtime$builder_project_allocate_dataset_id(1L, "ds1", "ds1"),
    "already in use"
  )

  replacement_ids <- runtime$builder_project_allocation_ids(
    entries = list(list(id = "ds1"), list(id = "ds2")),
    import_entries = list(list(id = "ds3")),
    replacing_id = "ds1"
  )
  expect_identical(replacement_ids, c("ds2", "ds3"))
})

test_that("uploaded sources are retained separately with their original names", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- file.path(root, "0.qs2")
  second <- file.path(root, "1.qs2")
  writeBin(charToRaw("first"), first)
  writeBin(charToRaw("second"), second)

  retained_first <- runtime$builder_project_retain_session_source(
    first,
    "sample-alpha.qs2",
    root,
    "ds1"
  )
  retained_second <- runtime$builder_project_retain_session_source(
    second,
    "sample-beta.qs2",
    root,
    "ds2"
  )

  expect_identical(basename(retained_first), "sample-alpha.qs2")
  expect_identical(basename(retained_second), "sample-beta.qs2")
  expect_true(file.exists(retained_first))
  expect_true(file.exists(retained_second))
})

test_that("retained upload bytes do not alias the transient source", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "0.qs2")
  writeBin(charToRaw("first"), source)

  retained <- runtime$builder_project_retain_session_source(
    source,
    "sample.qs2",
    root,
    "ds1"
  )
  writeBin(charToRaw("second"), source)

  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("first")
  )
})

test_that("browser uploads are retained by the background loader", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "browser-upload.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("uploaded-bytes"), source)

  loaded <- runtime$builder_project_load_retained_source(
    "ds1",
    list(source = source, retained_path = retained),
    progress = NULL,
    .adapter = function(path) list(path = path),
    .register = function(adapter, id, progress) {
      list(adapter = adapter, id = id)
    }
  )

  expect_identical(
    loaded$retained_path,
    normalizePath(retained, winslash = "/")
  )
  expect_identical(loaded$adapter$path, normalizePath(retained, winslash = "/"))
  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("uploaded-bytes")
  )

  unlink(source)
  retried <- runtime$builder_project_load_retained_source(
    "ds1",
    list(source = source, retained_path = retained),
    progress = NULL,
    .adapter = function(path) list(path = path),
    .register = function(adapter, id, progress) list(adapter = adapter, id = id)
  )
  expect_identical(
    retried$retained_path,
    normalizePath(retained, winslash = "/")
  )
})

test_that("failed background retention removes its partial file", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "browser-upload.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("uploaded-bytes"), source)

  expect_error(
    runtime$builder_project_load_retained_source(
      "ds1",
      list(source = source, retained_path = retained),
      progress = NULL,
      .adapter = identity,
      .register = function(...) stop("register should not run"),
      .copy = function(source, target, ...) {
        writeBin(charToRaw("partial"), target)
        FALSE
      }
    ),
    "could not be retained"
  )

  expect_false(file.exists(retained))
  expect_length(
    list.files(dirname(retained), pattern = "[.]part$", all.files = TRUE),
    0L
  )
})

test_that("example sources use the same retained file contract as uploads", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  bundled <- file.path(root, "complete_viewer_data.rds")
  writeBin(charToRaw("example-bytes"), bundled)
  catalog <- list(
    complete_viewer_data = list(
      id = "complete_viewer_data",
      serialized_path = bundled
    )
  )

  source <- runtime$builder_project_example_source(
    "complete_viewer_data",
    catalog
  )
  retained <- runtime$builder_project_retain_session_source(
    source$path,
    source$filename,
    root,
    "ds1"
  )

  expect_identical(source$origin, "example")
  expect_identical(source$filename, "complete_viewer_data.rds")
  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("example-bytes")
  )
})

test_that("background source jobs commit exact bytes through a part file", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-sources", "ds1", "sample.qs2")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("immutable-source"), source)
  project <- file.path(root, "project")
  dir.create(project)

  job <- runtime$builder_project_source_job(
    list(
      id = "ds1",
      path = source,
      filename = "sample.qs2",
      source_origin = "upload"
    ),
    project
  )
  result <- runtime$builder_project_copy_source_job(job)

  expect_identical(result$status, "ready")
  expect_identical(
    readBin(result$path, "raw", n = 100L),
    charToRaw("immutable-source")
  )
  expect_false(file.exists(job$part))
  expect_identical(result$fingerprint$md5, unname(tools::md5sum(result$path)))
  expect_match(
    runtime$builder_project_relative_path(result$path, project),
    "^sources/ds1/blobs/[0-9a-f]+/sample\\.qs2$"
  )
})

test_that("source jobs reuse the MD5 frozen with a dataset snapshot", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "sample.rds",
        "12",
        "2026-08-22T00:00:00.000000+0000",
        "ABCDEF0123456789ABCDEF0123456789",
        sep = ":"
      )
    )
  )

  expect_identical(
    runtime$builder_project_snapshot_source_md5(entry),
    "abcdef0123456789abcdef0123456789"
  )
})

test_that("legacy retained snapshots save and restore through verified hashing", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-sources", "ds1", "sample.rds")
  project <- file.path(root, "project")
  dir.create(dirname(source), recursive = TRUE)
  dir.create(project)
  writeBin(charToRaw("legacy-retained-source"), source)
  entry <- list(
    id = "ds1",
    path = source,
    filename = "sample.rds",
    source_origin = "upload",
    settings = list(name = "Dataset"),
    snapshot = list(
      source_fingerprint = paste(
        "builder-retained-v1",
        source,
        file.info(source)$size,
        "2026-08-22T00:00:00.000000+0000",
        sep = ":"
      )
    )
  )

  job <- runtime$builder_project_source_job(entry, project)
  expect_null(job$source_md5)
  result <- runtime$builder_project_copy_source_job(job)
  expect_identical(result$status, "ready")
  source_record <- list(
    kind = "managed",
    origin = "upload",
    filename = "sample.rds",
    path = runtime$builder_project_relative_path(result$path, project),
    status = "ready",
    fingerprint = result$fingerprint
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source_record,
    root = project
  )
  status <- runtime$builder_project_dataset_status(record, project)
  restored <- runtime$builder_project_restore_entry(
    record,
    project,
    status = status
  )

  expect_false(grepl(
    "builder-retained-v1",
    paste(capture.output(dput(record)), collapse = "")
  ))
  expect_true(status$source_matches)
  expect_true(status$restorable)
  expect_identical(
    restored$path,
    normalizePath(result$path, winslash = "/", mustWork = TRUE)
  )
})

test_that("only content-addressed managed sources reuse metadata on restore", {
  runtime <- builder_project_test_runtime()

  expect_true(runtime$builder_project_content_addressed_source(
    "sources/ds1/blobs/abcdef0123456789abcdef0123456789/sample.rds",
    "ds1"
  ))
  expect_false(runtime$builder_project_content_addressed_source(
    "sources/ds1/sample.rds",
    "ds1"
  ))
})

test_that("restored examples reuse the matching managed source", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  staged <- file.path(root, "complete_viewer_data.rds")
  writeBin(charToRaw("managed-example"), staged)
  source_md5 <- unname(tools::md5sum(staged))
  relative <- paste(
    "sources",
    "ds1",
    "blobs",
    source_md5,
    "complete_viewer_data.rds",
    sep = "/"
  )
  managed <- file.path(root, relative)
  dir.create(dirname(managed), recursive = TRUE)
  expect_true(file.rename(staged, managed))
  fingerprint <- runtime$builder_project_file_fingerprint(
    managed,
    content = TRUE
  )
  prior <- list(
    source = list(
      kind = "managed",
      origin = "example",
      example = "complete_viewer_data",
      filename = "complete_viewer_data.rds",
      path = relative,
      status = "ready",
      fingerprint = fingerprint
    )
  )
  entry <- list(
    id = "ds1",
    path = NULL,
    filename = "complete_viewer_data.rds",
    example = "complete_viewer_data",
    source_origin = "example",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "complete_viewer_data.rds",
        "15",
        "2026-08-22T00:00:00.000000+0000",
        source_md5,
        sep = ":"
      )
    )
  )

  prepared <- runtime$builder_project_prepare_source(entry, root, prior)

  expect_identical(prepared$source$status, "ready")
  expect_identical(prepared$source$path, relative)
  expect_null(prepared$job)
})

test_that("background source generations never overwrite prior content", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  source <- file.path(root, "session-sources", "ds1", "sample.qs2")
  dir.create(dirname(source), recursive = TRUE)
  dir.create(project)
  writeBin(charToRaw("first-source"), source)
  job <- runtime$builder_project_source_job(
    list(id = "ds1", path = source, filename = "sample.qs2"),
    project
  )
  first <- runtime$builder_project_copy_source_job(job)
  first_bytes <- readBin(first$path, "raw", n = 100L)

  writeBin(charToRaw("second-source"), source)
  second <- runtime$builder_project_copy_source_job(job)

  expect_false(identical(first$path, second$path))
  expect_identical(readBin(first$path, "raw", n = 100L), first_bytes)
  expect_identical(
    readBin(second$path, "raw", n = 100L),
    charToRaw("second-source")
  )
})

test_that("background source jobs report a missing source without a partial target", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  job <- runtime$builder_project_source_job(
    list(
      id = "ds1",
      path = file.path(root, "missing.qs2"),
      filename = "missing.qs2",
      source_origin = "upload"
    ),
    project
  )

  result <- runtime$builder_project_copy_source_job(job)

  expect_identical(result$status, "failed")
  expect_match(result$error, "no longer available", fixed = TRUE)
  expect_false(file.exists(job$part))
  expect_false(file.exists(job$target))
})

test_that("project source preparation records pending work without copying", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(
    root,
    "session-sources",
    "ds1",
    "complete_viewer_data.rds"
  )
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("example"), source)
  project <- file.path(root, "project")
  dir.create(project)
  entry <- list(
    id = "ds1",
    path = source,
    filename = "complete_viewer_data.rds",
    example = "complete_viewer_data",
    source_origin = "example"
  )

  prepared <- runtime$builder_project_prepare_source(entry, project)

  expect_identical(prepared$source$status, "pending")
  expect_identical(prepared$source$origin, "example")
  expect_identical(
    prepared$source$path,
    "sources/ds1/complete_viewer_data.rds"
  )
  expect_false(file.exists(prepared$job$target))
})

test_that("completed background sources are merged into the latest manifest", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  target <- file.path(project, "sources", "ds1", "sample.qs2")
  dir.create(dirname(target), recursive = TRUE)
  writeBin(charToRaw("ready"), target)
  manifest <- builder_project_test_manifest("ds1")
  manifest$datasets[[1L]]$source <- list(
    kind = "managed",
    origin = "upload",
    filename = "sample.qs2",
    path = "sources/ds1/sample.qs2",
    status = "pending"
  )
  result <- list(
    id = "ds1",
    status = "ready",
    path = target,
    fingerprint = runtime$builder_project_file_fingerprint(
      target,
      content = TRUE
    )
  )

  updated <- runtime$builder_project_apply_source_results(
    manifest,
    list(result),
    project
  )

  expect_identical(updated$datasets[[1L]]$source$status, "ready")
  expect_identical(
    updated$datasets[[1L]]$source$fingerprint$md5,
    result$fingerprint$md5
  )
})

test_that("completed source sync repoints live entries and releases owned session copies", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  session_root <- file.path(root, "session")
  retained <- file.path(session_root, "session-sources", "ds1", "sample.qs2")
  target <- file.path(
    project,
    "sources",
    "ds1",
    "blobs",
    "content",
    "sample.qs2"
  )
  dir.create(dirname(retained), recursive = TRUE)
  dir.create(dirname(target), recursive = TRUE)
  dir.create(project, showWarnings = FALSE)
  writeBin(charToRaw("ready"), retained)
  writeBin(charToRaw("ready"), target)
  manifest <- builder_project_test_manifest("ds1")
  manifest$datasets[[1L]]$source <- list(
    kind = "managed",
    origin = "upload",
    filename = "sample.qs2",
    path = runtime$builder_project_relative_path(target, project),
    status = "ready",
    fingerprint = runtime$builder_project_file_fingerprint(
      target,
      content = TRUE
    )
  )
  entries <- list(list(
    id = "ds1",
    path = retained,
    source_origin = "upload",
    snapshot = list(path = file.path(session_root, "snapshot-ds1"))
  ))

  committed <- runtime$builder_project_commit_source_entries(
    entries,
    manifest,
    results = list(list(id = "ds1", status = "ready", path = target)),
    root = project,
    session_root = session_root
  )

  expect_identical(
    committed$entries[[1L]]$path,
    normalizePath(target, winslash = "/")
  )
  expect_identical(committed$entries[[1L]]$source$kind, "managed")
  expect_false(file.exists(retained))
  expect_identical(committed$released, "ds1")
})

test_that("session source cleanup refuses paths outside the owned session root", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  owned <- file.path(root, "owned")
  outside <- file.path(root, "outside.qs2")
  dir.create(file.path(owned, "session-sources", "ds1"), recursive = TRUE)
  retained <- file.path(owned, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("owned"), retained)
  writeBin(charToRaw("outside"), outside)

  expect_true(runtime$builder_project_release_session_source(retained, owned))
  expect_false(file.exists(retained))
  expect_false(runtime$builder_project_release_session_source(outside, owned))
  expect_true(file.exists(outside))
})

test_that("session source release rejects lexical symlinks without deleting targets", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  owned <- file.path(root, "owned")
  outside <- file.path(root, "outside.qs2")
  link <- file.path(owned, "session-sources", "ds1", "linked.qs2")
  dir.create(dirname(link), recursive = TRUE)
  writeBin(charToRaw("outside"), outside)
  skip_if_not(file.symlink(outside, link), "symbolic links are unavailable")

  expect_false(runtime$builder_project_release_session_source(link, owned))
  expect_true(nzchar(Sys.readlink(link)))
  expect_true(file.exists(outside))
})

test_that("managed path checks allow only an explicitly missing create leaf", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  checkpoint_parent <- file.path(root, "checkpoints")

  expect_true(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root
  ))
  expect_false(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root,
    allow_missing_leaf = TRUE
  ))
  expect_true(runtime$.builder_project_path_has_link_within(
    file.path(root, "missing-parent", "checkpoints"),
    root,
    allow_missing_leaf = TRUE
  ))

  expect_true(dir.create(checkpoint_parent))
  expect_false(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root,
    allow_missing_leaf = TRUE
  ))

  unlink(checkpoint_parent, recursive = TRUE)
  skip_if_not(
    file.symlink(file.path(root, "missing-target"), checkpoint_parent),
    "symbolic links are unavailable"
  )
  expect_true(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root,
    allow_missing_leaf = TRUE
  ))
})

test_that("checkpoint cleanup accepts the lexical macOS alias of its project root", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  lexical_root <- withr::local_tempdir()
  normalized_root <- normalizePath(
    lexical_root,
    winslash = "/",
    mustWork = TRUE
  )
  skip_if(
    identical(lexical_root, normalized_root),
    "the temporary root has no lexical filesystem alias"
  )
  checkpoint <- file.path(
    lexical_root,
    "checkpoints",
    "20260820T000000"
  )
  dir.create(checkpoint, recursive = TRUE)
  writeLines("checkpoint", file.path(checkpoint, "dataset.crb"))

  expect_true(runtime$builder_project_cleanup_checkpoint(
    checkpoint,
    normalized_root
  ))
  expect_false(dir.exists(checkpoint))
})

test_that("source progress records stay compact while final results remain complete", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  sources <- file.path(root, paste0("source-", 1:3, ".rds"))
  lapply(seq_along(sources), function(index) {
    writeBin(charToRaw(paste0("source-", index)), sources[[index]])
  })
  jobs <- lapply(seq_along(sources), function(index) {
    runtime$builder_project_source_job(
      list(id = paste0("ds", index), path = sources[[index]]),
      project
    )
  })
  progress_path <- file.path(root, "progress.rds")

  results <- runtime$builder_project_copy_source_jobs(jobs, progress_path)
  progress <- readRDS(progress_path)

  expect_length(results, 3L)
  expect_identical(names(progress), c("completed", "total", "failed", "last"))
  expect_false("results" %in% names(progress))
  expect_identical(progress$completed, 3L)
})

test_that("managed example sources retain their example identity when restored", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "complete_viewer_data.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("example"), source)
  entry <- list(id = "ds1", settings = list(name = "Complete Viewer data"))
  configuration <- runtime$builder_project_write_dataset_config(entry, root)
  record <- list(
    id = "ds1",
    source = list(
      kind = "managed",
      origin = "example",
      example = "complete_viewer_data",
      path = "sources/ds1/complete_viewer_data.rds"
    ),
    configuration = configuration
  )

  restored <- runtime$builder_project_restore_entry(record, root)

  expect_identical(restored$source_origin, "example")
  expect_identical(restored$example, "complete_viewer_data")
  expect_identical(restored$path, normalizePath(source, winslash = "/"))
})

test_that("project payload preserves saved Spatial FOV controls", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  entry <- list(
    id = "ds1",
    settings = list(
      spatial_coordinate_transforms = list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      ),
      spatial_point_appearance = list(
        section_a_1_fov_1 = list(point_opacity = 0.7, point_size = 6)
      )
    )
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    root = root
  )

  restored <- runtime$builder_project_restore_entry(record, root)

  expect_identical(
    restored$settings$spatial_coordinate_transforms[[
      "section_a_1_fov_1"
    ]]$rotation_degrees,
    66.9
  )
  expect_identical(
    restored$settings$spatial_point_appearance[["section_a_1_fov_1"]],
    list(point_opacity = 0.7, point_size = 6)
  )
})

test_that("project spatial assets are externalized per dataset and FOV", {
  skip_if_not_installed("png")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source_path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), source_path)
  inspected <- runtime$builder_read_image(source_path)
  image_record <- function(section) {
    list(
      source = list(
        name = "tissue.png",
        type = "image/png",
        size = inspected$bytes
      ),
      source_path = inspected$source_path,
      source_content_md5 = inspected$source_content_md5,
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section_id = section
    )
  }
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        `section/a` = list(`H&E` = image_record("section/a")),
        `section-a` = list(`H&E` = image_record("section-a"))
      )
    )
  )

  payload_entry <- runtime$builder_project_stage_spatial_assets(entry, root)
  adopted <- runtime$builder_project_adopt_spatial_assets(
    entry,
    payload_entry,
    root
  )
  expect_true(file.exists(
    adopted$settings$images[["section/a"]][["H&E"]]$source_path
  ))
  expect_identical(
    adopted$settings$images[["section/a"]][["H&E"]]$project_asset,
    payload_entry$settings$images[["section/a"]][["H&E"]]$project_asset
  )
  original_fingerprint <- runtime$builder_project_file_fingerprint
  fingerprint_content <- logical()
  runtime$builder_project_file_fingerprint <- function(..., content = FALSE) {
    fingerprint_content <<- c(fingerprint_content, content)
    original_fingerprint(..., content = content)
  }
  restaged <- runtime$builder_project_stage_spatial_assets(adopted, root)
  expect_null(restaged$settings$images[["section/a"]][["H&E"]]$source_uri)
  expect_false(any(fingerprint_content))
  expect_false(grepl(
    "serialize(",
    paste(
      deparse(body(runtime$builder_project_stage_spatial_assets)),
      collapse = "\n"
    ),
    fixed = TRUE
  ))
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    payload_entry = restaged,
    root = root
  )
  runtime$builder_project_file_fingerprint <- original_fingerprint
  payload <- runtime$builder_project_read_dataset_config(record, root)
  first <- payload$settings$images[["section/a"]][["H&E"]]
  second <- payload$settings$images[["section-a"]][["H&E"]]

  expect_null(first$source_uri)
  expect_null(first$uri)
  expect_match(first$project_asset$path, "^spatial-assets/ds1/")
  expect_false(identical(first$project_asset$path, second$project_asset$path))
  expect_true(file.exists(file.path(root, first$project_asset$path)))
  expect_null(record$configuration$payload)

  restored <- runtime$builder_project_restore_entry(record, root)
  restored_source <- restored$settings$images[["section/a"]][[
    "H&E"
  ]]$source_path
  expect_true(file.exists(restored_source))
  expect_null(restored$settings$images[["section/a"]][["H&E"]]$source_uri)
  expect_null(restored$settings$images[["section-a"]][["H&E"]]$uri)
  expect_identical(
    unname(as.character(tools::md5sum(restored_source))),
    inspected$source_content_md5
  )
  expect_identical(
    runtime$builder_project_configuration_digest(restored),
    record$configuration$digest
  )
})

test_that("project restores multi-sheet attachments with one file fingerprint", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "supplement.xlsx")
  writeBin(charToRaw("workbook-bytes"), source)
  sheet <- function(name) {
    list(
      file_name = "supplement.xlsx",
      sheet_name = name,
      display_name = name,
      source_path = source
    )
  }
  entry <- list(
    id = "ds1",
    settings = list(
      tables = list(Clinical = sheet("Clinical"), QC = sheet("QC"))
    )
  )

  staged <- runtime$builder_project_stage_table_assets(entry, root)
  expect_null(staged$settings$tables$Clinical$source_path)
  expect_identical(
    staged$settings$tables$Clinical$project_asset,
    staged$settings$tables$QC$project_asset
  )

  original_fingerprint <- runtime$builder_project_file_fingerprint
  calls <- 0L
  runtime$builder_project_file_fingerprint <- function(...) {
    calls <<- calls + 1L
    original_fingerprint(...)
  }
  restored <- runtime$builder_project_restore_table_assets(staged, root)

  expect_identical(calls, 1L)
  expect_identical(
    restored$settings$tables$Clinical$source_path,
    restored$settings$tables$QC$source_path
  )

  adopted <- runtime$builder_project_adopt_table_assets(entry, staged, root)
  expect_identical(
    runtime$builder_project_configuration_digest(entry),
    runtime$builder_project_configuration_digest(staged)
  )
  expect_identical(
    runtime$builder_project_configuration_digest(entry),
    runtime$builder_project_configuration_digest(adopted)
  )
  renamed <- adopted
  renamed$settings$tables$Clinical$display_name <- "Patients"
  expect_false(identical(
    runtime$builder_project_configuration_digest(entry),
    runtime$builder_project_configuration_digest(renamed)
  ))
  fingerprint_content <- logical()
  runtime$builder_project_file_fingerprint <- function(..., content = FALSE) {
    fingerprint_content <<- c(fingerprint_content, content)
    original_fingerprint(..., content = content)
  }
  runtime$builder_project_stage_table_assets(adopted, root)
  expect_false(any(fingerprint_content))
})

test_that("project table asset jobs deduplicate sheets from one workbook", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "supplement.xlsx")
  writeBin(charToRaw("workbook-bytes"), source)
  table <- function(sheet) {
    list(
      file_name = "supplement.xlsx",
      sheet_name = sheet,
      source_path = source
    )
  }
  entries <- list(list(
    id = "ds1",
    revision = 4L,
    settings = list(
      tables = list(
        Clinical = table("Clinical"),
        QC = table("QC")
      )
    )
  ))

  jobs <- runtime$builder_project_table_asset_jobs(entries, root)

  expect_length(jobs, 1L)
  expect_identical(jobs[[1L]]$entry_id, "ds1")
  expect_identical(jobs[[1L]]$source, normalizePath(source, winslash = "/"))
  expect_identical(jobs[[1L]]$filename, "supplement.xlsx")

  results <- runtime$builder_project_stage_table_asset_jobs(jobs, root)
  expect_true(runtime$builder_project_table_asset_results_match(jobs, results))
  applied <- runtime$builder_project_apply_table_asset_results(
    entries,
    results,
    root
  )
  expect_identical(
    applied[[1L]]$settings$tables$Clinical$source_path,
    applied[[1L]]$settings$tables$QC$source_path
  )
  expect_true(file.exists(
    applied[[1L]]$settings$tables$Clinical$source_path
  ))
  original_fingerprint <- runtime$builder_project_file_fingerprint
  fingerprint_content <- logical()
  runtime$builder_project_file_fingerprint <- function(..., content = FALSE) {
    fingerprint_content <<- c(fingerprint_content, content)
    original_fingerprint(..., content = content)
  }
  runtime$builder_project_stage_table_assets(
    runtime$builder_project_configuration_entry(applied[[1L]]),
    root
  )
  expect_false(any(fingerprint_content))

  writeBin(charToRaw("changed-workbook-bytes"), source)
  expect_false(identical(
    jobs,
    runtime$builder_project_table_asset_jobs(entries, root)
  ))
})

test_that("project table asset save has an asynchronous stale-result guard", {
  source <- paste(
    readLines(
      testthat::test_path(
        "..",
        "..",
        "inst",
        "builder",
        "server",
        "project.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "builder_project_table_asset_generation", fixed = TRUE)
  expect_match(source, "callr::r_bg(", fixed = TRUE)
  expect_match(source, "builder_project_table_asset_jobs", fixed = TRUE)
  expect_match(source, "builder_project_table_save_signature", fixed = TRUE)
  expect_match(source, "process$get_result()", fixed = TRUE)
})

test_that("project table save signature changes with manifest or configuration", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- list(
    root = root,
    manifest = list(project = list(id = "project-safe", revision = 2L))
  )
  entries <- list(list(
    id = "ds1",
    revision = 3L,
    settings = list(name = "Dataset A", tables = list())
  ))
  signature <- runtime$builder_project_table_save_signature(project, entries)

  changed_project <- project
  changed_project$manifest$project$revision <- 3L
  expect_false(identical(
    signature,
    runtime$builder_project_table_save_signature(changed_project, entries)
  ))
  changed_entries <- entries
  changed_entries[[1L]]$settings$name <- "Dataset B"
  expect_false(identical(
    signature,
    runtime$builder_project_table_save_signature(project, changed_entries)
  ))
})

test_that("missing project spatial assets identify the affected FOV and image", {
  skip_if_not_installed("png")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source_path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), source_path)
  inspected <- runtime$builder_read_image(source_path)
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        fov_1 = list(
          DAPI = list(
            source = list(name = "DAPI.png", type = "image/png"),
            source_path = inspected$source_path,
            source_content_md5 = inspected$source_content_md5,
            base_bounds = list(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
          )
        )
      )
    )
  )
  payload_entry <- runtime$builder_project_stage_spatial_assets(entry, root)
  record <- runtime$builder_project_dataset_record(
    entry,
    list(kind = "missing", path = NULL),
    payload_entry = payload_entry,
    root = root
  )
  asset <- payload_entry$settings$images$fov_1$DAPI$project_asset$path
  unlink(file.path(root, asset))

  expect_error(
    runtime$builder_project_restore_entry(record, root),
    "ds1.*fov_1.*DAPI"
  )
  status <- runtime$builder_project_dataset_status(record, root)
  expect_false(status$spatial_assets_ready)
  expect_false(status$restorable)
  expect_identical(status$label, "Needs check · spatial image missing")
})

test_that("spatial asset status validates descriptors without hydrating image payloads", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  asset <- file.path(root, "spatial-assets", "ds1", "fov", "image.png")
  dir.create(dirname(asset), recursive = TRUE)
  writeBin(charToRaw("asset"), asset)
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        fov = list(
          image = list(
            project_asset = list(
              path = runtime$builder_project_relative_path(asset, root),
              mime = "image/png",
              fingerprint = runtime$builder_project_file_fingerprint(
                asset,
                content = TRUE
              )
            )
          )
        )
      )
    )
  )
  record <- list(
    id = "ds1",
    configuration = runtime$builder_project_write_dataset_config(entry, root)
  )
  runtime$builder_project_restore_spatial_assets <- function(...) {
    stop("status must not hydrate")
  }

  status <- runtime$builder_project_spatial_assets_status(record, root)

  expect_true(status$ready)
  expect_null(status$error)
})

test_that("status signatures sort resolved paths as character values", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  relative_paths <- c(
    "datasets/ds1/config.json",
    "sources/ds1/source.rds",
    "artifacts/ds1/ds1.crb",
    "artifacts/ds1/metadata.json"
  )
  for (relative_path in relative_paths) {
    path <- file.path(root, relative_path)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(relative_path), path)
  }
  record <- list(
    id = "ds1",
    configuration = list(path = relative_paths[[1L]]),
    source = list(kind = "managed", path = relative_paths[[2L]]),
    artifact = list(
      path = relative_paths[[3L]],
      members = list(
        list(path = relative_paths[[4L]]),
        list(path = relative_paths[[3L]])
      )
    )
  )

  signature <- runtime$builder_project_status_stat_signature(
    record,
    root
  )

  expect_identical(
    names(signature),
    sort(
      unique(vapply(
        file.path(root, relative_paths),
        normalizePath,
        character(1),
        winslash = "/",
        mustWork = TRUE
      )),
      method = "radix"
    )
  )
  expect_true(all(vapply(signature, is.list, logical(1))))
})

test_that("restore status snapshots are reused without weakening default validation", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "source.rds")
  artifact_path <- file.path(root, "artifacts", "ds1", "bundle", "ds1.crb")
  dir.create(dirname(source), recursive = TRUE)
  dir.create(dirname(artifact_path), recursive = TRUE)
  writeBin(charToRaw("source"), source)
  writeBin(charToRaw("artifact"), artifact_path)
  entry <- list(id = "ds1", settings = list(name = "Dataset"))
  source_fingerprint <- runtime$builder_project_file_fingerprint(
    source,
    content = TRUE
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = runtime$builder_project_relative_path(source, root),
      fingerprint = source_fingerprint
    ),
    artifact = list(
      status = "ready",
      path = runtime$builder_project_relative_path(artifact_path, root),
      fingerprint = runtime$builder_project_file_fingerprint(
        artifact_path,
        content = TRUE
      ),
      members = list(),
      built_from_revision = 0L,
      built_from_source_fingerprint = paste(
        "builder-snapshot-v2",
        "source.rds",
        source_fingerprint$bytes,
        source_fingerprint$modified_at,
        source_fingerprint$md5,
        sep = ":"
      ),
      built_from_configuration = runtime$builder_project_configuration_digest(
        entry
      )
    ),
    checked = TRUE,
    root = root
  )
  manifest <- list(datasets = list(record))
  calls <- 0L
  original <- runtime$builder_project_artifact_available
  runtime$builder_project_artifact_available <- function(...) {
    calls <<- calls + 1L
    original(...)
  }
  config_reads <- 0L
  original_read <- runtime$builder_project_read_dataset_config
  runtime$builder_project_read_dataset_config <- function(...) {
    config_reads <<- config_reads + 1L
    original_read(...)
  }

  snapshot <- runtime$builder_project_status_snapshot(manifest, root)
  expect_true(runtime$builder_project_status_snapshot_fresh(
    snapshot[["ds1"]],
    record,
    root
  ))
  restored <- runtime$builder_project_restore_entry(
    record,
    root,
    hydrate_spatial_assets = FALSE,
    status = snapshot[["ds1"]]
  )
  restored <- runtime$builder_project_artifact_entry(
    restored,
    record$artifact,
    root,
    status = snapshot[["ds1"]],
    record = record
  )
  mark <- runtime$builder_project_restored_check_identity(
    record,
    restored,
    snapshot[["ds1"]],
    root
  )

  expect_identical(calls, 1L)
  expect_identical(config_reads, 1L)
  expect_true(snapshot[["ds1"]]$artifact_ready)
  expect_match(mark, "^artifact:")
  writeBin(charToRaw("artifact-changed"), artifact_path)
  expect_error(
    runtime$builder_project_artifact_entry(
      entry,
      record$artifact,
      root,
      status = snapshot[["ds1"]],
      record = record
    ),
    "no longer available"
  )
  substituted <- record$artifact
  substituted$path <- "artifacts/ds1/bundle/substituted.crb"
  expect_error(
    runtime$builder_project_artifact_entry(
      entry,
      substituted,
      root,
      status = snapshot[["ds1"]],
      record = record
    ),
    "no longer available"
  )
  changed_record <- record
  changed_record$source$path <- "sources/ds1/missing.rds"
  changed_record$runtime_restore_status <- snapshot[["ds1"]]
  changed_status <- runtime$builder_project_dataset_status(changed_record, root)
  expect_false(changed_status$source_ready)
  writeBin(charToRaw("source-changed"), source)
  expect_false(runtime$builder_project_status_snapshot_fresh(
    snapshot[["ds1"]],
    record,
    root
  ))
  runtime$builder_project_dataset_status(record, root)
  expect_gte(calls, 4L)
})

test_that("checkpoint entries keep spatial images external", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    settings = list(
      spatial_image_storage = "external",
      images = list(
        fov_1 = list(image = list(source_path = "C:/external/image.png"))
      )
    )
  )

  checkpoint <- runtime$builder_project_checkpoint_entries(list(entry))

  expect_identical(
    checkpoint[[1L]]$settings$spatial_image_storage,
    "external"
  )
  expect_identical(entry$settings$spatial_image_storage, "external")
  expect_identical(checkpoint[[1L]]$settings$images, entry$settings$images)
})

test_that("checkpoint CRBs are reusable when their private closure is bundled", {
  runtime <- builder_project_test_runtime()
  item <- list(
    filename = "dataset.crb",
    sidecars = "dataset.h5",
    spatial_image_storage = "external",
    private_assets = c("dataset.crb", "dataset.h5")
  )

  expect_true(runtime$builder_project_plan_artifact_reusable(item))

  item$private_assets <- c(item$private_assets, "images/section.png")
  expect_false(runtime$builder_project_plan_artifact_reusable(item))
})

test_that("restored project settings refresh the active spatial dataset", {
  spatial_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "spatial_alignment_server.R"
  )
  project_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  spatial <- paste(readLines(spatial_path, warn = FALSE), collapse = "\n")
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(
    spatial,
    "restore_project_settings = restore_project_settings",
    fixed = TRUE
  )
  expect_match(
    project,
    "alignment_server$restore_project_settings(restored_ids)",
    fixed = TRUE
  )
})

test_that("managed examples keep their origin across the async import boundary", {
  imports_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "imports.R"
  )
  project_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  imports <- paste(readLines(imports_path, warn = FALSE), collapse = "\n")
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(imports, "source_origin = NULL", fixed = TRUE)
  expect_match(imports, "example_id = NULL", fixed = TRUE)
  expect_match(project, "source_origin = source$origin", fixed = TRUE)
  expect_match(project, "example_id = source$example", fixed = TRUE)
})

test_that("the project status only declares safe close after source sync", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "ui",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "Project fully saved · Safe to close", fixed = TRUE)
  expect_match(source, "Saving source files · ", fixed = TRUE)
  expect_match(source, "Stopping previous source copy", fixed = TRUE)
})

test_that("source synchronization warns on close without locking the workspace", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "clean",
    source_syncing = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$warn_before_unload)
  expect_true(capabilities$select_dataset)
  expect_true(capabilities$add_dataset)
  expect_true(capabilities$edit_dataset)
  expect_true(capabilities$save_project)
  expect_false(capabilities$page_inert)
})

test_that("project server uses a dedicated callr source copy process", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "request_builder_project_source_sync", fixed = TRUE)
  expect_match(source, "callr::r_bg", fixed = TRUE)
  expect_match(source, "later::later", fixed = TRUE)
  expect_match(source, "shiny::withReactiveDomain(session, {", fixed = TRUE)
  expect_match(source, "builder_project_apply_source_results", fixed = TRUE)
})

test_that("source sync keeps queued jobs until its worker starts", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  start <- regexpr(
    "builder_project_start_source_sync <- function()",
    source,
    fixed = TRUE
  )[[1L]]
  finish <- regexpr(
    "builder_project_poll_source_sync <- function()",
    source,
    fixed = TRUE
  )[[1L]]
  block <- substr(source, start, finish - 1L)

  spawn <- regexpr("callr::r_bg(", block, fixed = TRUE)[[1L]]
  dequeue <- regexpr(
    "builder_project_source_queue(queued)",
    block,
    fixed = TRUE
  )[[1L]]
  expect_gt(spawn, 0L)
  expect_gt(dequeue, spawn)
  expect_false(grepl(
    "builder_project_source_queue(list())",
    block,
    fixed = TRUE
  ))
  expect_match(block, 'inherits(process, "condition")', fixed = TRUE)
})

test_that("source sync cancellation owns and terminates its process", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "builder_project_stop_source_sync_process <- function(kill = TRUE)",
    fixed = TRUE
  )
  expect_match(source, "process$kill_tree()", fixed = TRUE)
  expect_match(
    source,
    "invalidate_builder_project_source_sync(kill = TRUE)",
    fixed = TRUE
  )
  expect_match(source, "source_sync_started <-", fixed = TRUE)
})

test_that("source sync retains ownership when process termination is unconfirmed", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  first <- grep(
    "builder_project_stop_source_sync_process <- function(kill = TRUE)",
    lines,
    fixed = TRUE
  )[[1L]]
  last <- grep(
    "invalidate_builder_project_source_sync <- function(kill = TRUE)",
    lines,
    fixed = TRUE
  )[[1L]]
  runtime <- new.env(parent = baseenv())
  runtime$isolate <- function(value) value
  holder <- function(initial) {
    value <- initial
    function(next_value) {
      if (!missing(next_value)) {
        value <<- next_value
      }
      value
    }
  }
  alive <- TRUE
  fake_process <- list(
    is_alive = function() alive,
    kill_tree = function() invisible(FALSE),
    kill = function() invisible(FALSE),
    wait = function(timeout) invisible(FALSE)
  )
  progress <- withr::local_tempfile(fileext = ".rds")
  writeBin(charToRaw("progress"), progress)
  runtime$builder_project_source_process <- holder(fake_process)
  runtime$builder_project_source_run <- holder(list(owner = "project-a"))
  runtime$builder_project_source_progress <- holder(progress)
  runtime$.builder_project_text <- function(value) {
    is.character(value) && length(value) == 1L && nzchar(value)
  }
  eval(parse(text = paste(lines[first:(last - 1L)], collapse = "\n")), runtime)

  expect_false(runtime$builder_project_stop_source_sync_process(kill = TRUE))
  expect_identical(runtime$builder_project_source_process(), fake_process)
  expect_identical(
    runtime$builder_project_source_run(),
    list(owner = "project-a")
  )
  expect_identical(runtime$builder_project_source_progress(), progress)
  expect_true(file.exists(progress))

  alive <- FALSE
  expect_true(runtime$builder_project_stop_source_sync_process(kill = FALSE))
  expect_null(runtime$builder_project_source_process())
  expect_null(runtime$builder_project_source_run())
  expect_null(runtime$builder_project_source_progress())
  expect_false(file.exists(progress))
})

test_that("source sync invalidation remains cancelling after a failed stop", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  first <- grep(
    "invalidate_builder_project_source_sync <- function(kill = TRUE)",
    lines,
    fixed = TRUE
  )[[1L]]
  last <- grep(
    "builder_project_start_source_sync <- function()",
    lines,
    fixed = TRUE
  )[[1L]]
  runtime <- new.env(parent = baseenv())
  runtime$`%||%` <- function(left, right) if (is.null(left)) right else left
  runtime$isolate <- function(value) value
  holder <- function(initial) {
    value <- initial
    function(next_value) {
      if (!missing(next_value)) {
        value <<- next_value
      }
      value
    }
  }
  runtime$builder_project_source_generation <- holder(3)
  runtime$builder_project_source_queue <- holder(list(old = list(id = "old")))
  runtime$builder_project_source_run <- holder(list(owner = "project-a"))
  runtime$builder_project_source_sync <- holder(list(
    status = "syncing",
    completed = 1L,
    total = 2L,
    failed = 0L
  ))
  runtime$builder_project_stop_source_sync_process <- function(kill) FALSE
  scheduled <- 0L
  runtime$builder_project_schedule_source_sync_poll <- function(delay) {
    scheduled <<- scheduled + 1L
    invisible(TRUE)
  }
  eval(parse(text = paste(lines[first:(last - 1L)], collapse = "\n")), runtime)

  expect_false(runtime$invalidate_builder_project_source_sync(kill = TRUE))
  expect_identical(runtime$builder_project_source_generation(), 4)
  expect_length(runtime$builder_project_source_queue(), 0L)
  expect_identical(runtime$builder_project_source_sync()$status, "cancelling")
  expect_true(runtime$builder_project_source_run()$cancelling)
  expect_identical(runtime$builder_project_source_run()$owner, "project-a")
  expect_identical(scheduled, 1L)
})

test_that("a finished source process remains owned until its result is collected", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    paste(
      "  process <- isolate(builder_project_source_process())",
      "  if (!is.null(process)) {",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    "!is.null(process) && isTRUE(process$is_alive())",
    source,
    fixed = TRUE
  ))
})

test_that("project manifests reject unsafe and duplicate dataset ids", {
  runtime <- builder_project_test_runtime()
  path <- withr::local_tempfile(fileext = ".json")

  jsonlite::write_json(
    builder_project_test_manifest(c("../outside")),
    path,
    auto_unbox = TRUE
  )
  expect_error(runtime$builder_project_read(path), "unsafe or duplicate")

  jsonlite::write_json(
    builder_project_test_manifest(c("ds1", "ds1")),
    path,
    auto_unbox = TRUE
  )
  expect_error(runtime$builder_project_read(path), "unsafe or duplicate")
})

test_that("reusable artifacts must retain their saved content fingerprint", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("original"), artifact)
  fingerprint <- list(md5 = unname(tools::md5sum(artifact)))

  expect_true(runtime$.builder_build_fingerprint_matches(artifact, fingerprint))
  writeBin(charToRaw("replacement"), artifact)
  expect_false(runtime$.builder_build_fingerprint_matches(
    artifact,
    fingerprint
  ))
})

test_that("reusable artifacts are staged without replacing existing files", {
  runtime <- builder_project_test_runtime()
  source <- withr::local_tempfile(fileext = ".crb")
  target <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("artifact"), source)
  unlink(target)

  expect_true(runtime$.builder_build_copy_file(source, target))
  expect_identical(readBin(target, "raw", n = 100L), charToRaw("artifact"))

  writeBin(charToRaw("existing"), target)
  expect_false(runtime$.builder_build_copy_file(source, target))
  expect_identical(readBin(target, "raw", n = 100L), charToRaw("existing"))
})

test_that("reusable artifact copies are verified after staging", {
  runtime <- builder_project_test_runtime()
  source <- withr::local_tempfile(fileext = ".crb")
  target <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("original"), source)
  unlink(target)
  fingerprint <- list(md5 = unname(tools::md5sum(source)))
  mutating_copy <- function(source, target) {
    copied <- file.copy(source, target)
    writeBin(charToRaw("mutated!"), target)
    copied
  }

  expect_false(runtime$.builder_build_copy_verified(
    source,
    target,
    fingerprint,
    .copy = mutating_copy
  ))
  expect_false(file.exists(target))
})

test_that("reusable artifact staging uses portable clone fallbacks", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "source.crb")
  target <- file.path(root, "darwin.crb")
  writeBin(charToRaw("artifact"), source)
  commands <- list()
  fallbacks <- 0L
  command <- function(command, args, stdout, stderr) {
    commands[[length(commands) + 1L]] <<- c(command, args)
    file.copy(source, target)
    0L
  }
  fallback <- function(...) {
    fallbacks <<- fallbacks + 1L
    TRUE
  }

  expect_true(runtime$.builder_build_copy_file(
    source,
    target,
    .sysname = "Darwin",
    .command = command,
    .fallback = fallback
  ))
  expect_true("-c" %in% commands[[1L]])

  target <- file.path(root, "linux.crb")
  expect_true(runtime$.builder_build_copy_file(
    source,
    target,
    .sysname = "Linux",
    .command = command,
    .fallback = fallback
  ))
  expect_true("--reflink=auto" %in% commands[[2L]])

  target <- file.path(root, "windows.crb")
  expect_true(runtime$.builder_build_copy_file(
    source,
    target,
    .sysname = "Windows",
    .command = command,
    .fallback = fallback
  ))
  expect_identical(length(commands), 2L)
  expect_identical(fallbacks, 1L)
})

test_that("persisted artifact fingerprints contain only plain JSON values", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("artifact"), artifact)

  fingerprint <- runtime$builder_project_file_fingerprint(
    artifact,
    content = TRUE
  )

  expect_identical(class(fingerprint$md5), "character")
  expect_silent(jsonlite::toJSON(fingerprint, auto_unbox = TRUE))
})

test_that("configuration digests are plain JSON strings", {
  runtime <- builder_project_test_runtime()

  digest <- runtime$builder_project_configuration_digest(list())

  expect_identical(class(digest), "character")
  expect_silent(jsonlite::toJSON(list(digest = digest), auto_unbox = TRUE))
})

test_that("build execution rejects a reusable CRB changed after checkpoint", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("original"), artifact)
  fingerprint <- list(md5 = unname(tools::md5sum(artifact)))
  writeBin(charToRaw("replacement"), artifact)
  stage <- withr::local_tempdir()
  plan <- structure(
    list(
      items = list(list(
        id = "ds1",
        name = "Dataset 1",
        filename = "dataset-1.crb",
        reused_artifact = list(
          path = artifact,
          fingerprint = fingerprint,
          members = list()
        )
      )),
      make_app = FALSE,
      app_auth = list(enabled = FALSE)
    ),
    class = c("builder_build_plan", "list")
  )
  unused <- function(...) stop("changed artifacts must fail before hooks run")
  hooks <- list(
    open_snapshot = unused,
    prepare = unused,
    run_analyses = unused,
    export = unused,
    attach_extras = unused,
    verify = unused
  )

  result <- runtime$builder_execute_plan(plan, stage, list(), hooks = hooks)

  expect_identical(result$state, "failure")
  expect_match(result$error, "unavailable or has changed", fixed = TRUE)
  expect_false(file.exists(file.path(stage, "dataset-1.crb")))
})

test_that("saved plan items are reused only when artifact references agree", {
  runtime <- builder_project_test_runtime()
  saved <- list(
    id = "ds1",
    name = "Dataset 1",
    filename = "dataset-1.crb",
    sidecars = "dataset-1.h5",
    viewer_bundle_assets = character(),
    private_assets = c("dataset-1.crb", "dataset-1.h5"),
    viewer_bundle_asset_claims = list(),
    private_asset_claims = list()
  )

  expect_true(runtime$builder_project_reused_plan_matches(saved, saved))
  changed <- saved
  changed$filename <- "renamed.crb"
  expect_false(runtime$builder_project_reused_plan_matches(saved, changed))
})

test_that("failed checkpoint enqueue cannot remain marked running", {
  runtime <- builder_project_test_runtime()
  manifest <- builder_project_test_manifest("ds1")
  manifest$pending_build <- list(id = "checkpoint-1", status = "running")

  updated <- runtime$builder_project_finish_pending_build(
    manifest,
    status = "failed",
    error = "The build could not be queued.",
    finished_at = "2026-08-17T12:00:00Z"
  )

  expect_identical(updated$pending_build$status, "failed")
  expect_identical(
    updated$pending_build$error,
    "The build could not be queued."
  )
  expect_identical(updated$pending_build$finished_at, "2026-08-17T12:00:00Z")
})

test_that("restore dialog keeps recovery choices compact", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "ui",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "Skip · Not loaded", fixed = TRUE)
  expect_match(source, "Use saved CRB · Fast", fixed = TRUE)
  expect_match(source, "Load source · Editable", fixed = TRUE)
  expect_match(source, "if (status$restorable) {", fixed = TRUE)
  expect_false(grepl(
    "Keep in the project, but skip for now",
    source,
    fixed = TRUE
  ))
})

test_that("ready CRB workbench keeps the Review continuation actions", {
  project_ui <- paste(
    readLines(
      testthat::test_path(
        "..",
        "..",
        "inst",
        "builder",
        "ui",
        "project.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  project_server <- paste(
    readLines(
      testthat::test_path(
        "..",
        "..",
        "inst",
        "builder",
        "server",
        "review.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    project_ui,
    'uiOutput("configure_actions")',
    fixed = TRUE
  )
  expect_match(
    project_ui,
    '"continue_to_review",',
    fixed = TRUE
  )
  expect_match(
    project_ui,
    '"Continue to Review",',
    fixed = TRUE
  )
  expect_match(
    project_server,
    "builder_project_artifact_actions_ui(",
    fixed = TRUE
  )
})

test_that("restore choices render descriptive labels and prefer checked CRB reuse", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$selectInput <- shiny::selectInput
  ui_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "ui",
    "project.R"
  )
  runtime$builder_source_utf8(ui_path, envir = runtime)
  root <- withr::local_tempdir()
  source_path <- file.path(root, "source.rds")
  artifact_path <- file.path(root, "artifact.crb")
  writeBin(charToRaw("source"), source_path)
  writeBin(charToRaw("artifact"), artifact_path)
  entry <- list(id = "ds1", settings = list(name = "Dataset A"))
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = runtime$builder_project_relative_path(source_path, root)
    ),
    checked = TRUE,
    artifact = list(
      status = "ready",
      reusable = TRUE,
      path = runtime$builder_project_relative_path(artifact_path, root),
      fingerprint = runtime$builder_project_file_fingerprint(
        artifact_path,
        content = TRUE
      ),
      members = list(),
      built_from_revision = 0L,
      built_from_configuration = runtime$builder_project_configuration_digest(
        entry
      )
    ),
    root = root
  )

  html <- htmltools::renderTags(
    runtime$builder_project_restore_row_ui(record, root)
  )$html

  expect_match(html, "Use saved CRB · Fast", fixed = TRUE)
  expect_match(html, "Load source · Editable", fixed = TRUE)
  expect_match(html, '<option value="reuse" selected>', fixed = TRUE)
  expect_match(html, "Checked · CRB ready", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      html,
      gregexpr("Checked · CRB ready", html, fixed = TRUE)
    )),
    1L
  )

  record$configuration$checked <- FALSE
  record$artifact$status <- "stale"
  html <- htmltools::renderTags(
    runtime$builder_project_restore_row_ui(record, root)
  )$html
  expect_match(html, "Needs check · load source", fixed = TRUE)
})

test_that("project lifecycle capabilities lock only conflicting operations", {
  runtime <- builder_project_test_runtime()

  importing <- runtime$builder_activity_state(
    client_imports = 1L,
    project_phase = "dirty",
    has_project = TRUE,
    has_datasets = TRUE
  )
  import_capabilities <- runtime$builder_activity_capabilities(importing)
  expect_true(import_capabilities$add_dataset)
  expect_true(import_capabilities$edit_dataset)
  expect_false(import_capabilities$check_dataset)
  expect_false(import_capabilities$save_project)
  expect_false(import_capabilities$build)

  saving <- runtime$builder_activity_state(
    project_phase = "saving",
    has_project = TRUE,
    has_datasets = TRUE
  )
  save_capabilities <- runtime$builder_activity_capabilities(saving)
  expect_true(save_capabilities$page_inert)
  expect_true(save_capabilities$warn_before_unload)
  expect_false(save_capabilities$mutate_datasets)
  expect_false(save_capabilities$save_project)
})

test_that("project autosave waits for both import queues to drain", {
  runtime <- builder_project_test_runtime()
  activity <- function(client_imports = 0L, server_imports = FALSE) {
    runtime$builder_activity_state(
      client_imports = client_imports,
      server_imports = server_imports,
      project_phase = "dirty",
      has_project = TRUE,
      has_datasets = TRUE
    )
  }

  expect_false(
    runtime$builder_activity_capabilities(activity(client_imports = 1L))[[
      "save_project"
    ]]
  )
  expect_false(
    runtime$builder_activity_capabilities(activity(server_imports = TRUE))[[
      "save_project"
    ]]
  )
  expect_true(runtime$builder_activity_capabilities(activity())$save_project)
})

test_that("terminal retry rows do not block autosave after another import succeeds", {
  runtime <- builder_project_test_runtime()
  runtime$builder_source_utf8(
    testthat::test_path("..", "..", "inst", "builder", "loading.R"),
    envir = runtime
  )
  queue <- runtime$builder_import_queue()
  entry <- runtime$builder_import_entry(
    "ds2",
    "Failed dataset",
    list(kind = "file", path = "failed.qs2")
  )
  queue <- runtime$builder_import_add(queue, entry)
  queue <- runtime$builder_import_transition(queue, "ds2", "reading", 1L)
  activity <- function(imports) {
    ## The successful import is already attached; only the retry row remains.
    runtime$builder_activity_state(
      server_imports = !is.null(runtime$builder_import_focus_id(imports)),
      project_phase = "dirty",
      has_project = TRUE,
      has_datasets = TRUE
    )
  }

  expect_false(
    runtime$builder_activity_capabilities(activity(queue))$save_project
  )

  queue <- runtime$builder_import_transition(
    queue,
    "ds2",
    "error",
    1L,
    error = "Could not read the dataset."
  )

  expect_length(queue$entries, 1L)
  expect_identical(queue$entries$ds2$load_state, "error")
  expect_true(
    runtime$builder_activity_capabilities(activity(queue))$save_project
  )

  project_server <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    project_server,
    "server_imports = !is.null(import_focus_id())",
    fixed = TRUE
  )
})

test_that("project autosave checks save capability before requesting a save", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  observer_start <- regexpr(
    "observe({\n  project <- builder_project()",
    source,
    fixed = TRUE
  )
  observer_end <- regexpr(
    "show_builder_project_folder_error <- function",
    source,
    fixed = TRUE
  )
  expect_gt(observer_start[[1L]], 0L)
  expect_gt(observer_end[[1L]], observer_start[[1L]])
  autosave <- substr(
    source,
    observer_start[[1L]],
    observer_end[[1L]] - 1L
  )
  guard <- regexpr(
    paste(
      "if (!isTRUE(builder_capabilities()$save_project)) {",
      "    return()",
      "  }",
      sep = "\n"
    ),
    autosave,
    fixed = TRUE
  )[[1L]]
  request <- regexpr(
    "request_builder_project_save(",
    autosave,
    fixed = TRUE
  )[[1L]]

  expect_gt(guard, 0L)
  expect_gt(request, guard)
})

test_that("project creation is available only with data", {
  runtime <- builder_project_test_runtime()
  empty <- runtime$builder_activity_capabilities(
    runtime$builder_activity_state()
  )
  populated <- runtime$builder_activity_capabilities(
    runtime$builder_activity_state(has_datasets = TRUE)
  )
  importing <- runtime$builder_activity_capabilities(
    runtime$builder_activity_state(client_imports = 1L, has_datasets = TRUE)
  )

  expect_false(empty$create_project)
  expect_true(populated$create_project)
  expect_true(importing$create_project)
})

test_that("a project folder can be chosen while another dataset is importing", {
  runtime <- builder_project_test_runtime()
  importing <- runtime$builder_activity_state(
    client_imports = 1L,
    server_imports = TRUE,
    project_phase = "none",
    has_project = FALSE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(importing)

  expect_true(capabilities$create_project)
  expect_true(capabilities$add_dataset)
  expect_false(capabilities$check_dataset)
  expect_false(capabilities$save_project)
  expect_false(capabilities$build)
})

test_that("project folders distinguish empty, existing, and unrelated content", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()

  expect_identical(runtime$builder_project_folder_state(root)$kind, "empty")

  writeLines("keep", file.path(root, ".keep"))
  unrelated <- runtime$builder_project_folder_state(root)
  expect_identical(unrelated$kind, "nonempty")
  expect_identical(unrelated$managed_conflicts, character())

  dir.create(file.path(root, "Sources"))
  reserved <- runtime$builder_project_folder_state(root)
  expect_identical(reserved$kind, "nonempty")
  expect_identical(reserved$managed_conflicts, "Sources")

  writeLines("{}", runtime$builder_project_manifest_path(root))
  expect_identical(runtime$builder_project_folder_state(root)$kind, "project")
})

test_that("non-empty project folders require explicit confirmation", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$builder_source_utf8(
    testthat::test_path("..", "..", "inst", "builder", "ui", "project.R"),
    envir = runtime
  )

  html <- htmltools::renderTags(
    runtime$builder_project_nonempty_folder_dialog("/tmp/existing-files")
  )$html

  expect_match(html, "Folder already contains files", fixed = TRUE)
  expect_match(html, "Existing files will be kept", fixed = TRUE)
  expect_match(
    html,
    'id="choose_another_builder_project_folder"',
    fixed = TRUE
  )
  expect_match(html, 'id="confirm_builder_project_folder"', fixed = TRUE)
  expect_match(html, 'id="cancel_builder_project_folder"', fixed = TRUE)
})

test_that("existing Builder projects require update confirmation", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$builder_source_utf8(
    testthat::test_path("..", "..", "inst", "builder", "ui", "project.R"),
    envir = runtime
  )

  html <- htmltools::renderTags(
    runtime$builder_project_existing_folder_dialog("/tmp/project")
  )$html

  expect_match(html, "Update existing project?", fixed = TRUE)
  expect_match(
    html,
    "Other files in the folder will not be changed",
    fixed = TRUE
  )
  expect_match(
    html,
    'id="confirm_existing_builder_project_folder"',
    fixed = TRUE
  )
  expect_match(
    html,
    'id="choose_another_builder_project_folder"',
    fixed = TRUE
  )
})

test_that("the top bar omits the format capability summary", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "app.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl(
    'span(class = "formats", textOutput("format_line", inline = TRUE))',
    source,
    fixed = TRUE
  ))
})

test_that("spatial drafts are committed by workflow actions instead of blocking them", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "dirty",
    spatial_dirty = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$check_dataset)
  expect_true(capabilities$navigate_workflow)
  expect_true(capabilities$prepare_crbs)
  expect_true(capabilities$build)
  expect_true(capabilities$warn_before_unload)
})

test_that("project saving can commit spatial drafts through the internal mutation path", {
  foundation_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "foundation.R"
  )
  imports_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "imports.R"
  )
  enhancements_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "enhancements.R"
  )
  project_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  foundation <- paste(readLines(foundation_path, warn = FALSE), collapse = "\n")
  imports <- paste(readLines(imports_path, warn = FALSE), collapse = "\n")
  enhancements <- paste(
    readLines(enhancements_path, warn = FALSE),
    collapse = "\n"
  )
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(
    foundation,
    "replace_entry <- function(updated, internal = FALSE)",
    fixed = TRUE
  )
  expect_match(
    imports,
    "commit_enhance_images <- function(entry, images, internal = FALSE)",
    fixed = TRUE
  )
  expect_match(enhancements, "internal = TRUE", fixed = TRUE)
  expect_match(
    project,
    "materialize_coordinate_drafts(\n      notify = FALSE",
    fixed = TRUE
  )
})

test_that("project location selection, opening, and restore make the page inert", {
  runtime <- builder_project_test_runtime()

  for (phase in c("choosing", "opening", "restoring")) {
    activity <- runtime$builder_activity_state(project_phase = phase)
    capabilities <- runtime$builder_activity_capabilities(activity)
    expect_true(capabilities$page_inert)
    expect_true(capabilities$warn_before_unload)
    expect_false(capabilities$open_project)
  }
})

test_that("Open remains available for switching an idle workspace", {
  runtime <- builder_project_test_runtime()

  for (phase in c("none", "clean", "dirty", "save_failed", "conflict")) {
    activity <- runtime$builder_activity_state(
      project_phase = phase,
      has_project = !identical(phase, "none"),
      has_datasets = TRUE
    )
    expect_true(
      runtime$builder_activity_capabilities(activity)$open_project,
      info = phase
    )
  }
})

test_that("Open confirms before replacing unsaved workspace state", {
  runtime <- builder_project_test_runtime()

  expect_false(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state()
  ))
  expect_false(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state(
      project_phase = "clean",
      has_project = TRUE,
      has_datasets = TRUE
    )
  ))
  expect_true(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state(has_datasets = TRUE)
  ))
  expect_true(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state(
      project_phase = "clean",
      spatial_dirty = TRUE,
      has_project = TRUE,
      has_datasets = TRUE
    )
  ))
  for (phase in c("dirty", "save_failed", "conflict")) {
    expect_true(
      runtime$builder_activity_requires_open_confirmation(
        runtime$builder_activity_state(
          project_phase = phase,
          has_project = TRUE,
          has_datasets = TRUE
        )
      ),
      info = phase
    )
  }
})

test_that("pending project settings survive until source imports are dispatched", {
  runtime <- builder_project_test_runtime()
  pending <- list(ds1 = list(id = "ds1"))

  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      operation = "opening"
    ),
    character()
  )
  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      import_ids = "ds1",
      operation = "restoring"
    ),
    character()
  )
  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      operation = "restoring"
    ),
    "ds1"
  )
})

test_that("project open work resolves without timer polling", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    'builder_project_operation_phase("opening")',
    fixed = TRUE
  )
  expect_match(source, "builder_async_then(", fixed = TRUE)
  expect_false(grepl(
    "builder_project_schedule_open_poll",
    source,
    fixed = TRUE
  ))
  expect_match(source, "builder_project_restore_progress", fixed = TRUE)
  expect_match(source, 'mode = "restoring"', fixed = TRUE)
})

test_that("project restore stays in the opening overlay", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    'builderActivityState.phase === "opening" || builderActivityState.phase === "restoring"',
    fixed = TRUE
  )
})

test_that("project open snapshots attach validated runtime status", {
  runtime <- builder_project_test_runtime()
  project_path <- file.path("project-root", "builder-project.json")
  runtime$builder_project_read <- function(path) {
    list(datasets = list(list(id = "ds1"), list(id = "ds2")))
  }
  runtime$builder_project_status_snapshot <- function(manifest, root) {
    expect_identical(root, dirname(project_path))
    list(
      ds1 = list(label = "first"),
      ds2 = list(label = "second")
    )
  }

  opened <- runtime$builder_project_open_snapshot(project_path)

  expect_identical(opened$path, project_path)
  expect_identical(opened$root, dirname(project_path))
  expect_identical(
    opened$manifest$datasets[[1L]]$runtime_restore_status$label,
    "first"
  )
  expect_identical(
    opened$manifest$datasets[[2L]]$runtime_restore_status$label,
    "second"
  )
})

test_that("ready CRB restore selection is prepared transactionally in manifest order", {
  runtime <- builder_project_test_runtime()
  runtime$builder_project_dataset_status <- function(record, root) {
    record$runtime_restore_status
  }
  runtime$builder_project_restore_entry <- function(
    record,
    root,
    hydrate_spatial_assets = TRUE,
    status = NULL
  ) {
    list(id = record$id, load_state = "reload_required")
  }
  runtime$builder_project_artifact_entry <- function(
    entry,
    artifact,
    root,
    status = NULL,
    record = NULL
  ) {
    entry$load_state <- "artifact_ready"
    entry$project_artifact <- artifact
    entry
  }
  runtime$builder_project_restored_check_identity <- function(
    record,
    entry,
    status,
    root
  ) {
    paste0("mark-", entry$id)
  }
  manifest <- list(
    datasets = list(
      list(
        id = "ds2",
        artifact = list(path = "artifacts/ds2.crb"),
        runtime_restore_status = list(artifact_ready = TRUE, checked = TRUE)
      ),
      list(
        id = "ds1",
        artifact = list(path = "artifacts/ds1.crb"),
        runtime_restore_status = list(artifact_ready = TRUE, checked = TRUE)
      )
    )
  )

  prepared <- runtime$builder_project_prepare_open_selection(
    manifest,
    "project-root",
    list(ds2 = "reuse", ds1 = "reuse")
  )

  expect_identical(
    vapply(prepared$reusable_entries, `[[`, character(1), "id"),
    c("ds2", "ds1")
  )
  expect_identical(names(prepared$artifacts), c("ds2", "ds1"))
  expect_identical(
    prepared$marks,
    c(ds2 = "mark-ds2", ds1 = "mark-ds1")
  )
  expect_identical(prepared$pending_entries, list())
  expect_identical(prepared$skipped_ids, character())

  skipped <- runtime$builder_project_prepare_open_selection(
    manifest,
    "project-root",
    list(ds2 = "skip", ds1 = "skip")
  )
  expect_identical(skipped$reusable_entries, list())
  expect_identical(skipped$skipped_ids, c("ds2", "ds1"))
})

test_that("ready CRB confirmation validates the complete state before committing", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  start <- regexpr(
    "complete_builder_project_open <- function(actions = NULL) {",
    source,
    fixed = TRUE
  )[[1L]]
  finish <- regexpr(
    "\nobserveEvent(input$confirm_builder_project_open, {",
    source,
    fixed = TRUE
  )[[1L]]
  observer <- substr(source, start, finish - 1L)

  prepare_position <- regexpr(
    "prepared <- tryCatch(",
    observer,
    fixed = TRUE
  )[[1L]]
  state_position <- regexpr(
    "next_store <- tryCatch(",
    observer,
    fixed = TRUE
  )[[1L]]
  commit_position <- regexpr("store(next_store)", observer, fixed = TRUE)[[1L]]

  expect_gt(prepare_position, 0L)
  expect_gt(state_position, prepare_position)
  expect_gt(commit_position, state_position)
  expect_false(grepl("order(vapply(", observer, fixed = TRUE))
  expect_match(
    observer,
    "The reusable CRBs could not be attached to this session",
    fixed = TRUE
  )
})

test_that("a revision conflict permits reopening but rejects mutation", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "conflict",
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$open_project)
  expect_false(capabilities$edit_dataset)
  expect_false(capabilities$save_project)
  expect_match(
    runtime$builder_activity_reason(activity, "save_project"),
    "Reopen the project",
    fixed = TRUE
  )
})

test_that("an existing project can persist removal of its final dataset", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "dirty",
    has_project = TRUE,
    has_datasets = FALSE
  )

  expect_true(runtime$builder_activity_capabilities(activity)$save_project)
})

test_that("project dirty state follows configuration checked state and order", {
  runtime <- builder_project_test_runtime()
  first <- list(
    id = "ds1",
    settings = list(name = "First"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  second <- list(
    id = "ds2",
    settings = list(name = "Second"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  manifest <- list(
    datasets = list(
      list(
        id = "ds1",
        order = 1L,
        configuration = list(
          digest = runtime$builder_project_configuration_digest(first),
          checked = FALSE
        )
      ),
      list(
        id = "ds2",
        order = 2L,
        configuration = list(
          digest = runtime$builder_project_configuration_digest(second),
          checked = TRUE
        )
      )
    )
  )

  expect_false(runtime$builder_project_live_dirty(
    list(first, second),
    "ds2",
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(first, second),
    character(),
    manifest
  ))
  changed <- first
  changed$settings$name <- "Renamed"
  expect_true(runtime$builder_project_live_dirty(
    list(changed, second),
    "ds2",
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(second, first),
    "ds2",
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(first),
    character(),
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(),
    character(),
    manifest
  ))
  expect_false(runtime$builder_project_live_dirty(
    list(),
    character(),
    list(datasets = list())
  ))
  expect_false(runtime$builder_project_live_dirty(
    list(),
    character(),
    manifest,
    ignored_ids = c("ds1", "ds2")
  ))
  inactive_manifest <- manifest
  inactive_manifest$datasets <- lapply(
    inactive_manifest$datasets,
    function(record) {
      record$release <- list(included = FALSE)
      record
    }
  )
  expect_false(runtime$builder_project_live_dirty(
    list(),
    character(),
    inactive_manifest
  ))
})

test_that("a changed project revision marks dirty without hashing settings", {
  runtime <- builder_project_test_runtime()
  runtime$builder_project_cached_configuration_digest <- function(...) {
    stop("changed revisions must not hash configuration", call. = FALSE)
  }
  entry <- list(
    id = "ds1",
    revision = 4L,
    settings = list(name = "Renamed")
  )
  manifest <- list(
    datasets = list(list(
      id = "ds1",
      order = 1L,
      configuration = list(
        revision = 3L,
        digest = "saved-digest",
        checked = FALSE
      )
    ))
  )

  expect_true(runtime$builder_project_live_dirty(
    list(entry),
    character(),
    manifest
  ))
})

test_that("checked identities survive only while removal remains undoable", {
  runtime <- builder_project_test_runtime()
  marks <- c(ds1 = "mark-one", ds2 = "mark-two", stale = "mark-stale")
  removed <- list(id = "ds2", entry = list(id = "ds2"))

  expect_identical(
    runtime$builder_project_retain_check_marks(
      marks,
      live_ids = "ds1",
      last_removed = removed,
      can_undo_remove = TRUE
    ),
    c(ds1 = "mark-one", ds2 = "mark-two")
  )
  expect_identical(
    runtime$builder_project_retain_check_marks(
      marks,
      live_ids = "ds1",
      last_removed = removed,
      can_undo_remove = FALSE
    ),
    c(ds1 = "mark-one")
  )
})

test_that("checked project records bind confirmation to the restored entry", {
  runtime <- builder_project_test_runtime()
  restored <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/new-snapshot",
      owner_token = "new-owner",
      object_md5 = "new-md5"
    ),
    settings = list(
      name = "Spatial dataset",
      spatial_coordinate_transforms = list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      )
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  mark <- runtime$builder_project_check_identity(restored)
  expect_identical(mark, runtime$builder_project_check_identity(restored))

  changed <- restored
  changed$settings$spatial_coordinate_transforms[[
    "section_a_1_fov_1"
  ]]$rotation_degrees <- 70
  expect_false(identical(
    mark,
    runtime$builder_project_check_identity(changed)
  ))
})

test_that("runtime snapshot replacement does not invalidate checked configuration", {
  runtime <- builder_project_test_runtime()
  first <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/first",
      owner_token = "first-owner",
      object_md5 = "first-md5"
    ),
    settings = list(name = "Dataset", default_group = "cell_type")
  )
  replacement <- first
  replacement$snapshot <- list(
    path = "/session/replacement",
    owner_token = "replacement-owner",
    object_md5 = "replacement-md5"
  )

  expect_identical(
    runtime$builder_project_check_identity(first),
    runtime$builder_project_check_identity(replacement)
  )

  replacement$settings$default_group <- "cluster"
  expect_false(identical(
    runtime$builder_project_check_identity(first),
    runtime$builder_project_check_identity(replacement)
  ))
})

test_that("live checked identity uses the monotonic dataset revision", {
  runtime <- builder_project_test_runtime()
  runtime$builder_project_cached_configuration_digest <- function(...) {
    stop("live identity must not digest configuration", call. = FALSE)
  }
  entry <- list(
    id = "ds1",
    revision = 7L,
    settings = list(name = "Dataset", included_projections = "umap")
  )

  expect_identical(
    runtime$builder_project_check_identity(entry),
    "configuration:ds1:revision:7"
  )
  changed <- entry
  changed$settings$included_projections <- c("umap", "pca")
  changed$revision <- 8L
  expect_identical(
    runtime$builder_project_check_identity(changed),
    "configuration:ds1:revision:8"
  )
})

test_that("pending coordinate drafts participate in checked identity", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/ds1",
      owner_token = "owner-ds1",
      object_md5 = strrep("a", 32L)
    ),
    settings = list(
      name = "Spatial dataset",
      images = list(),
      spatial_coordinate_transforms = list()
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  confirmed <- runtime$builder_project_effective_check_identity(entry, list())
  snapshot_identity <- runtime$.builder_worker_identity(entry$snapshot)
  draft <- function(rotation) {
    list(
      ds1 = list(
        `fov-a` = list(
          dataset = "ds1",
          section = "fov-a",
          snapshot_identity = snapshot_identity,
          spec = list(
            schema_version = 1L,
            rotation_degrees = rotation,
            scale = 1
          )
        )
      )
    )
  }

  expect_false(identical(
    confirmed,
    runtime$builder_project_effective_check_identity(entry, draft(37.5))
  ))
  expect_identical(
    runtime$builder_project_effective_check_identity(entry, draft(0)),
    confirmed
  )
  marks <- stats::setNames(confirmed, entry$id)
  expect_identical(
    runtime$builder_project_checked_ids(list(entry), marks, draft(37.5)),
    character()
  )
  expect_identical(
    runtime$builder_project_checked_ids(list(entry), marks, draft(0)),
    entry$id
  )
})

test_that("reusable CRB preparation skips current artifacts", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset.crb")
  writeLines("ready", artifact_path)
  entry <- list(
    id = "ds1",
    revision = 4L,
    snapshot = list(source_fingerprint = "source-a"),
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 4L,
    built_from_source_fingerprint = "source-a",
    built_from_configuration = runtime$builder_project_configuration_digest(
      entry
    )
  )

  expect_length(
    runtime$builder_project_entries_requiring_crb(
      list(entry),
      list(ds1 = artifact),
      root
    ),
    0L
  )
  entry$settings$name <- "Changed"
  expect_identical(
    vapply(
      runtime$builder_project_entries_requiring_crb(
        list(entry),
        list(ds1 = artifact),
        root
      ),
      `[[`,
      character(1),
      "id"
    ),
    "ds1"
  )
})

test_that("current project CRBs replace only temporary Build entries", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset.crb")
  writeLines("ready", artifact_path)
  entry <- list(
    id = "ds1",
    revision = 4L,
    load_state = "loaded",
    snapshot = list(source_fingerprint = "source-a"),
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 4L,
    built_from_source_fingerprint = "source-a",
    built_from_configuration = runtime$builder_project_configuration_digest(
      entry
    )
  )

  build_entries <- runtime$builder_project_entries_for_build(
    list(entry),
    list(ds1 = artifact),
    root
  )

  expect_identical(entry$load_state, "loaded")
  expect_identical(build_entries[[1L]]$load_state, "artifact_ready")
  expect_identical(
    build_entries[[1L]]$project_artifact$resolved_path,
    normalizePath(artifact_path, winslash = "/", mustWork = TRUE)
  )

  changed <- entry
  changed$settings$name <- "Changed"
  expect_identical(
    runtime$builder_project_entries_for_build(
      list(changed),
      list(ds1 = artifact),
      root
    ),
    list(changed)
  )

  changed <- entry
  changed$revision <- 5L
  changed$snapshot$source_fingerprint <- "source-b"
  expect_identical(
    runtime$builder_project_entries_for_build(
      list(changed),
      list(ds1 = artifact),
      root
    ),
    list(changed)
  )

  unlink(artifact_path)
  expect_identical(
    runtime$builder_project_entries_for_build(
      list(entry),
      list(ds1 = artifact),
      root
    ),
    list(entry)
  )
})

test_that("re-registered artifacts retain their source identity", {
  runtime <- builder_project_test_runtime()
  previous <- list(built_from_source_fingerprint = "source-a")
  reused <- list(reused_artifact = list(path = "dataset.crb"))

  expect_identical(
    runtime$builder_project_artifact_source_fingerprint(
      list(snapshot = NULL),
      reused,
      previous
    ),
    "source-a"
  )
  expect_identical(
    runtime$builder_project_artifact_source_fingerprint(
      list(snapshot = list(source_fingerprint = "source-b")),
      reused,
      previous
    ),
    "source-b"
  )
  expect_null(runtime$builder_project_artifact_source_fingerprint(
    list(snapshot = NULL),
    list(),
    previous
  ))
})

test_that("reopened projects reject artifacts from an older source", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source_path <- file.path(root, "sources", "ds1", "source.rds")
  artifact_path <- file.path(root, "artifacts", "ds1", "dataset.crb")
  dir.create(dirname(source_path), recursive = TRUE)
  dir.create(dirname(artifact_path), recursive = TRUE)
  writeBin(charToRaw("new-source"), source_path)
  writeBin(charToRaw("old-artifact"), artifact_path)
  source_fingerprint <- runtime$builder_project_file_fingerprint(
    source_path,
    content = TRUE
  )
  entry <- list(
    id = "ds1",
    revision = 5L,
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = runtime$builder_project_relative_path(artifact_path, root),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 4L,
    built_from_source_fingerprint = "source-a",
    built_from_configuration = runtime$builder_project_configuration_digest(
      entry
    )
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = runtime$builder_project_relative_path(source_path, root),
      fingerprint = source_fingerprint
    ),
    artifact = artifact,
    checked = TRUE,
    root = root
  )

  expect_false(
    runtime$builder_project_dataset_status(record, root)$artifact_ready
  )
  reopened <- runtime$builder_project_prepare_open_selection(
    list(datasets = list(record)),
    root,
    list(ds1 = "reuse")
  )
  expect_length(reopened$reusable_entries, 0L)
  expect_identical(reopened$skipped_ids, "ds1")

  record$artifact$built_from_revision <- 5L
  record$artifact$built_from_source_fingerprint <- paste(
    "builder-snapshot-v2",
    "source.rds",
    source_fingerprint$bytes,
    source_fingerprint$modified_at,
    source_fingerprint$md5,
    sep = ":"
  )
  expect_true(
    runtime$builder_project_dataset_status(record, root)$artifact_ready
  )
})

test_that("a ready project CRB remains separate from the checked flag", {
  runtime <- builder_project_test_runtime()
  record <- list(
    configuration = list(
      checked = FALSE,
      digest = "same-configuration"
    ),
    artifact = list(
      status = "ready",
      built_from_configuration = "same-configuration"
    )
  )

  expect_false(runtime$builder_project_record_configuration_confirmed(record))

  record$configuration$checked <- TRUE
  record$configuration$checked_digest <- "same-configuration"
  expect_true(runtime$builder_project_record_configuration_confirmed(record))

  record$configuration$digest <- "newer-configuration"
  expect_false(runtime$builder_project_record_configuration_confirmed(record))
})

test_that("browser lifecycle code includes queue sync and close protection", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "builder_client_import_state", fixed = TRUE)
  expect_match(source, "builder_activity_state", fixed = TRUE)
  expect_match(source, "beforeunload", fixed = TRUE)
  expect_match(source, "builder-operation-overlay", fixed = TRUE)
  expect_match(
    source,
    'complete_dataset_check: activityCapability("check_dataset")',
    fixed = TRUE
  )
  expect_match(
    source,
    'continue_to_review: activityCapability("navigate_workflow")',
    fixed = TRUE
  )
})

test_that("connection flush sends a message captured in reactive context", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "activity_message <- builder_activity_message(",
    fixed = TRUE
  )
  expect_match(
    source,
    "build_overlay = isolate(builder_build_overlay())",
    fixed = TRUE
  )
  expect_match(
    source,
    paste0(
      'function\\(\\)\\s*\\{\\s*session\\$sendCustomMessage\\(\\s*',
      '"builder_activity_state",\\s*activity_message'
    ),
    perl = TRUE
  )
})

test_that("CRB planning requests always acknowledge or reach a terminal state", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "builder_project_build_crb_request_id <- reactiveVal(NULL)",
    fixed = TRUE
  )
  expect_match(
    source,
    "builder_project_send_crb_progress <- function(",
    fixed = TRUE
  )
  expect_match(
    source,
    "builder_project_fail_crb_request <- function(",
    fixed = TRUE
  )
  expect_match(
    source,
    'payload$request_id <- request_id',
    fixed = TRUE
  )
  expect_identical(
    lengths(regmatches(
      source,
      gregexpr('"builder_project_crb_progress"', source, fixed = TRUE)
    )),
    1L
  )

  prepare_start <- regexpr(
    "prepare_builder_project_crbs <- function(request_id)",
    source,
    fixed = TRUE
  )[[1L]]
  prepare_end <- regexpr(
    "observeEvent(input$prepare_builder_project_crbs",
    source,
    fixed = TRUE
  )[[1L]]
  expect_gt(prepare_start, 0L)
  expect_gt(prepare_end, prepare_start)
  prepare <- substr(source, prepare_start, prepare_end - 1L)
  expect_match(prepare, "tryCatch(", fixed = TRUE)
  expect_match(prepare, "error = function(error)", fixed = TRUE)
  expect_match(
    prepare,
    "builder_project_cleanup_checkpoint(output, project$root)",
    fixed = TRUE
  )
  expect_gte(
    lengths(regmatches(
      prepare,
      gregexpr("builder_project_fail_crb_request(", prepare, fixed = TRUE)
    )),
    5L
  )

  observer_end <- regexpr(
    "\nobserve({\n  value <- result()",
    source,
    fixed = TRUE
  )[[1L]]
  expect_gt(observer_end, prepare_end)
  observer <- substr(source, prepare_end, observer_end - 1L)
  planning_position <- regexpr(
    'builder_project_send_crb_progress(\n    "planning"',
    observer,
    fixed = TRUE
  )[[1L]]
  capability_position <- regexpr(
    'builder_operation_allowed("prepare_crbs")',
    observer,
    fixed = TRUE
  )[[1L]]
  expect_gt(planning_position, 0L)
  expect_gt(capability_position, planning_position)
  expect_match(
    observer,
    "save_started <- request_builder_project_save(",
    fixed = TRUE
  )
  expect_match(observer, "if (isTRUE(ok)) {", fixed = TRUE)
  expect_match(observer, "} else {", fixed = TRUE)
  expect_match(observer, "if (!isTRUE(save_started)) {", fixed = TRUE)
  expect_match(observer, "error = function(error)", fixed = TRUE)

  expect_match(
    source,
    "builder_project_capture_build_plan <- function(\n  plan,\n  crb_request_id = NULL",
    fixed = TRUE
  )
  expect_match(
    source,
    "builder_project_build_plan(captured_plan)\n  builder_project_build_crb_request_id(request_id)",
    fixed = TRUE
  )
  capture_start <- regexpr(
    "builder_project_capture_build_plan <- function(",
    source,
    fixed = TRUE
  )[[1L]]
  capture_end <- regexpr(
    "builder_project_source_runtime_file <- function()",
    source,
    fixed = TRUE
  )[[1L]]
  capture <- substr(source, capture_start, capture_end - 1L)
  expect_false(grepl("serialize(", capture, fixed = TRUE))
  expect_match(capture, "builder_project_build_plan(plan)", fixed = TRUE)
  expect_match(source, "captured_plan <- plan", fixed = TRUE)
  expect_match(
    source,
    "save_error <- NULL\n    saved <- tryCatch(",
    fixed = TRUE
  )
  expect_match(
    source,
    "failed_project$manifest <- builder_project_finish_pending_build(",
    fixed = TRUE
  )
  expect_match(source, "terminal_saved <- isTRUE(tryCatch(", fixed = TRUE)
  expect_match(
    source,
    "crb_request_id <- isolate(builder_project_build_crb_request_id())",
    fixed = TRUE
  )
  expect_match(
    source,
    "builder_project_build_crb_request_id(NULL)",
    fixed = TRUE
  )
  expect_match(
    source,
    "promote = builder_has_text(crb_request_id)",
    fixed = TRUE
  )
})

test_that("deferred project saves keep the Shiny session domain", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "builder_project_schedule_source_sync_poll <- function(delay = 0.2)",
    fixed = TRUE
  )
  expect_match(
    source,
    "shiny::withReactiveDomain(session, {",
    fixed = TRUE
  )
})

test_that("activity locks do not rewrite unchanged text during DOM enhancement", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "title.textContent !== builderActivityState.busy_title",
    fixed = TRUE
  )
  expect_match(
    source,
    "message.textContent !== builderActivityState.busy_message",
    fixed = TRUE
  )
})

test_that("activity locks keep the loading workspace visible and cancellable", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl(
    "workspace.inert = workspaceLocked",
    source,
    fixed = TRUE
  ))
  expect_match(
    source,
    "#builder-workspace input, #builder-workspace select, ",
    fixed = TRUE
  )
  expect_match(
    source,
    ".builder-retry-import, .builder-remove-import",
    fixed = TRUE
  )
})

test_that("dynamic content enhancement is coalesced per animation frame", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "function matchingDynamicElements(roots, selector)",
    fixed = TRUE
  )
  expect_match(
    source,
    "function matchingDynamicContainers(roots, selector)",
    fixed = TRUE
  )
  expect_match(source, "root.closest(selector)", fixed = TRUE)
  expect_match(
    source,
    "function scheduleDynamicContentEnhancement(root)",
    fixed = TRUE
  )
  expect_match(source, "setupDatasetDropzones(roots);", fixed = TRUE)
  expect_match(source, "registerStages(roots);", fixed = TRUE)
  expect_match(source, "setupCreatableSelects(roots);", fixed = TRUE)
  expect_match(source, "updateDatasetLoadTimes(roots);", fixed = TRUE)
  expect_match(
    source,
    "Array.from(mutation.addedNodes || []).forEach",
    fixed = TRUE
  )
  expect_match(
    source,
    "new MutationObserver(handleDynamicContentMutations)",
    fixed = TRUE
  )
  expect_false(grepl("characterData: true", source, fixed = TRUE))
  expect_match(
    source,
    'sidebar.addEventListener("scroll", scheduleSpatialAlignmentScrollbars',
    fixed = TRUE
  )
  expect_match(
    source,
    'window.addEventListener("scroll", scheduleSpatialAlignmentScrollbars',
    fixed = TRUE
  )
  expect_match(
    source,
    "if (spatialScrollbarFrame !== null) return;",
    fixed = TRUE
  )
})

test_that("spatial canvas is cleared before switching datasets", {
  server_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "spatial_alignment_server.R"
  )
  client_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder-spatial-canvas.js"
  )
  server_source <- paste(readLines(server_path, warn = FALSE), collapse = "\n")
  client_source <- paste(readLines(client_path, warn = FALSE), collapse = "\n")

  expect_match(
    server_source,
    'send_canvas_clear <- function() {',
    fixed = TRUE
  )
  expect_match(server_source, '"builder_spatial_canvas_clear"', fixed = TRUE)
  expect_match(
    client_source,
    'Shiny.addCustomMessageHandler("builder_spatial_canvas_clear", function (message) {',
    fixed = TRUE
  )
  expect_match(client_source, "generation < state.generation", fixed = TRUE)
  expect_match(client_source, "clear();", fixed = TRUE)
})

test_that("dataset config generations survive manifest backups and stale writers", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  manifest <- runtime$builder_project_new_manifest(root, "Generation safety")
  entry <- function(name) {
    list(
      id = "ds1",
      revision = 1L,
      settings = list(name = name),
      acknowledgements = character(),
      spatial_drafts = list()
    )
  }
  record <- function(name) {
    runtime$builder_project_dataset_record(
      entry(name),
      source = list(kind = "missing", path = NULL),
      root = root
    )
  }

  manifest$datasets <- list(record("first"))
  first <- runtime$builder_project_write(manifest, root)
  stale <- first$manifest

  winner <- first$manifest
  winner$datasets <- list(record("winner"))
  winner <- runtime$builder_project_write(
    winner,
    root,
    expected_revision = first$manifest$project$revision
  )

  stale$datasets <- list(record("stale"))
  expect_error(
    runtime$builder_project_write(
      stale,
      root,
      expected_revision = first$manifest$project$revision
    ),
    "updated by another Builder window",
    fixed = TRUE
  )

  expect_identical(
    runtime$builder_project_read_dataset_config(
      winner$manifest$datasets[[1L]],
      root
    )$settings$name,
    "winner"
  )
  backup <- runtime$builder_project_read(paste0(winner$path, ".bak"))
  expect_identical(
    runtime$builder_project_read_dataset_config(
      backup$datasets[[1L]],
      root
    )$settings$name,
    "first"
  )
  expect_false(identical(
    backup$datasets[[1L]]$configuration$path,
    winner$manifest$datasets[[1L]]$configuration$path
  ))
})

test_that("manifest publish failure restores the canonical manifest", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- runtime$builder_project_write(
    runtime$builder_project_new_manifest(root, "Restore safety"),
    root
  )
  target <- first$path
  backup <- paste0(target, ".bak")
  move <- function(from, to) {
    if (
      startsWith(basename(from), "builder-project-") &&
        identical(basename(to), "builder-project.json")
    ) {
      return(FALSE)
    }
    file.rename(from, to)
  }

  expect_error(
    runtime$builder_project_write(
      first$manifest,
      root,
      expected_revision = first$manifest$project$revision,
      .move = move
    ),
    "could not be committed",
    fixed = TRUE
  )
  expect_true(file.exists(target))
  expect_false(file.exists(backup))
  expect_identical(
    runtime$builder_project_read(target)$project$revision,
    first$manifest$project$revision
  )
})

test_that("manifest restore failure reports the retained backup", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- runtime$builder_project_write(
    runtime$builder_project_new_manifest(root, "Recovery path"),
    root
  )
  target <- first$path
  backup <- paste0(target, ".bak")
  move <- function(from, to) {
    if (
      identical(basename(from), "builder-project.json") &&
        identical(basename(to), "builder-project.json.bak")
    ) {
      return(file.rename(from, to))
    }
    FALSE
  }

  expect_error(
    runtime$builder_project_write(
      first$manifest,
      root,
      expected_revision = first$manifest$project$revision,
      .move = move
    ),
    paste(
      "Recover it from:",
      normalizePath(backup, winslash = "/", mustWork = FALSE)
    ),
    fixed = TRUE
  )
  expect_false(file.exists(target))
  expect_true(file.exists(backup))
  expect_identical(
    runtime$builder_project_read(backup)$project$revision,
    first$manifest$project$revision
  )
})

test_that("manifest revision commits are serialized by an owned project lock", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()

  first <- runtime$builder_project_acquire_manifest_lock(root)
  expect_error(
    runtime$builder_project_acquire_manifest_lock(root),
    "Another Builder window",
    fixed = TRUE
  )
  expect_true(dir.exists(first$path))
  expect_true(runtime$builder_project_release_manifest_lock(first))

  second <- runtime$builder_project_acquire_manifest_lock(root)
  expect_true(runtime$builder_project_release_manifest_lock(second))

  implementation <- paste(
    deparse(body(runtime$builder_project_write)),
    collapse = "\n"
  )
  acquire <- regexpr(
    "builder_project_acquire_manifest_lock(root)",
    implementation,
    fixed = TRUE
  )[[1L]]
  read_disk <- regexpr(
    "builder_project_read(target)",
    implementation,
    fixed = TRUE
  )[[1L]]
  expect_gt(acquire, 0L)
  expect_gt(read_disk, acquire)
})

test_that("project input is bounded and Builder has no inline image decoder", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  oversized <- file.path(root, "oversized.json")
  writeBin(
    rep(as.raw(0x20), runtime$.builder_project_manifest_max_bytes + 1L),
    oversized
  )

  expect_error(
    runtime$builder_project_read(oversized),
    "size limit",
    fixed = TRUE
  )
  implementation <- paste(
    readLines(testthat::test_path("..", "..", "inst", "builder", "project.R")),
    collapse = "\n"
  )
  expect_false(grepl("base64decode", implementation, fixed = TRUE))
  expect_false(grepl(
    "builder_project_decode_image_uri",
    implementation,
    fixed = TRUE
  ))
})

test_that("restored source identity requires the recorded content fingerprint", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "source.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("AAAA"), source)
  recorded_time <- file.info(source)$mtime[[1L]]
  record <- runtime$builder_project_dataset_record(
    list(id = "ds1", settings = list(name = "Dataset")),
    source = list(
      kind = "managed",
      path = "sources/ds1/source.rds",
      status = "ready",
      fingerprint = runtime$builder_project_file_fingerprint(
        source,
        content = TRUE
      )
    ),
    checked = TRUE,
    root = root
  )

  writeBin(charToRaw("BBBB"), source)
  Sys.setFileTime(source, recorded_time)
  status <- runtime$builder_project_dataset_status(record, root)

  expect_false(status$source_matches)
  expect_false(status$checked)
})

test_that("content-addressed sources detect same-metadata tampering", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  staged <- file.path(root, "source.rds")
  writeBin(charToRaw("AAAA"), staged)
  source_md5 <- unname(tools::md5sum(staged))
  relative <- paste(
    "sources",
    "ds1",
    "blobs",
    source_md5,
    "source.rds",
    sep = "/"
  )
  source <- file.path(root, relative)
  dir.create(dirname(source), recursive = TRUE)
  expect_true(file.rename(staged, source))
  recorded_time <- file.info(source)$mtime[[1L]]
  record <- runtime$builder_project_dataset_record(
    list(id = "ds1", settings = list(name = "Dataset")),
    source = list(
      kind = "managed",
      path = relative,
      status = "ready",
      fingerprint = runtime$builder_project_file_fingerprint(
        source,
        content = TRUE
      )
    ),
    checked = TRUE,
    root = root
  )

  writeBin(charToRaw("BBBB"), source)
  Sys.setFileTime(source, recorded_time)
  status <- runtime$builder_project_dataset_status(record, root)

  expect_false(status$source_matches)
  expect_false(status$checked)
})

test_that("artifact availability validates the primary file and every member", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  primary <- file.path(root, "artifacts", "ds1", "bundle", "ds1.crb")
  member <- file.path(dirname(primary), "ds1.h5")
  dir.create(dirname(primary), recursive = TRUE)
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("member"), member)
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = "artifacts/ds1/bundle/ds1.crb",
    fingerprint = runtime$builder_project_file_fingerprint(
      primary,
      content = TRUE
    ),
    members = list(list(
      target = "ds1.h5",
      path = "artifacts/ds1/bundle/ds1.h5",
      fingerprint = runtime$builder_project_file_fingerprint(
        member,
        content = TRUE
      )
    ))
  )

  expect_true(runtime$builder_project_artifact_available(artifact, root))
  primary_time <- file.info(primary)$mtime[[1L]]
  writeBin(charToRaw("altered"), primary)
  Sys.setFileTime(primary, primary_time)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
  writeBin(charToRaw("primary"), primary)
  Sys.setFileTime(primary, primary_time)
  expect_true(runtime$builder_project_artifact_available(artifact, root))
  member_time <- file.info(member)$mtime[[1L]]
  writeBin(charToRaw("tamper"), member)
  Sys.setFileTime(member, member_time)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
  writeBin(charToRaw("member"), member)
  Sys.setFileTime(member, member_time)
  expect_true(runtime$builder_project_artifact_available(artifact, root))
  writeBin(charToRaw("changed"), member)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
  unlink(member)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
})

test_that("managed writes reject symlink ancestors before creating external files", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  outside <- file.path(root, "outside")
  dir.create(project)
  dir.create(outside)
  if (!isTRUE(file.symlink(outside, file.path(project, "datasets")))) {
    skip("symlinks are unavailable")
  }

  expect_error(
    runtime$builder_project_write_dataset_config(
      list(id = "ds1", settings = list(name = "Dataset")),
      project
    ),
    "symbolic link",
    fixed = TRUE
  )
  expect_length(list.files(outside, recursive = TRUE, all.files = TRUE), 0L)
})

test_that("artifact bundles are immutable generations", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build_dir <- file.path(root, "build")
  project <- file.path(root, "project")
  dir.create(build_dir)
  dir.create(project)
  primary <- file.path(build_dir, "ds1.crb")
  member <- file.path(build_dir, "ds1.h5")
  writeBin(charToRaw("first-primary"), primary)
  writeBin(charToRaw("first-member"), member)

  first <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = project
  )
  expect_true(file.exists(primary))
  expect_true(file.exists(member))
  first_bytes <- readBin(file.path(project, first$path), "raw", n = 100L)

  writeBin(charToRaw("second-primary"), primary)
  writeBin(charToRaw("second-member"), member)
  second <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = project
  )

  expect_false(identical(first$path, second$path))
  expect_identical(
    readBin(file.path(project, first$path), "raw", n = 100L),
    first_bytes
  )
  expect_identical(length(second$members), 1L)
})

test_that("artifact bundles retain BPCells directory contents", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build_dir <- file.path(root, "build")
  project <- file.path(root, "project")
  sidecar <- file.path(build_dir, "ds1.bpcells")
  dir.create(file.path(sidecar, "matrix"), recursive = TRUE)
  dir.create(project)
  primary <- file.path(build_dir, "ds1.crb")
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("index"), file.path(sidecar, ".index"))
  writeBin(charToRaw("data"), file.path(sidecar, "matrix", "data"))

  bundle <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.bpcells",
    dataset_id = "ds1",
    root = project,
    promote = TRUE
  )

  expect_false(file.exists(primary))
  expect_false(file.exists(file.path(sidecar, ".index")))
  expect_false(file.exists(file.path(sidecar, "matrix", "data")))
  expect_setequal(
    vapply(bundle$members, `[[`, character(1), "target"),
    c("ds1.bpcells/.index", "ds1.bpcells/matrix/data")
  )
  expect_true(runtime$builder_project_artifact_available(
    c(list(status = "ready"), bundle),
    project
  ))
})

test_that("BPCells artifact bundles reject symbolic links", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build_dir <- file.path(root, "build")
  project <- file.path(root, "project")
  sidecar <- file.path(build_dir, "ds1.bpcells")
  dir.create(sidecar, recursive = TRUE)
  dir.create(project)
  primary <- file.path(build_dir, "ds1.crb")
  data <- file.path(sidecar, "data")
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("data"), data)
  skip_if_not(file.symlink(data, file.path(sidecar, "linked")))

  expect_error(
    runtime$builder_project_store_artifact_bundle(
      primary,
      sidecars = "ds1.bpcells",
      dataset_id = "ds1",
      root = project
    ),
    "escaped the build folder",
    fixed = TRUE
  )
})

test_that("checkpoint artifacts promote to immutable shared generations", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  checkpoint <- file.path(root, "checkpoints", "build-1")
  dir.create(checkpoint, recursive = TRUE)
  primary <- file.path(checkpoint, "ds1.crb")
  member <- file.path(checkpoint, "ds1.h5")
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("member"), member)

  bundle <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = root,
    promote = TRUE
  )

  expect_false(file.exists(primary))
  expect_false(file.exists(member))
  expect_true(runtime$builder_project_artifact_available(
    c(list(status = "ready"), bundle),
    root
  ))

  second_checkpoint <- file.path(root, "checkpoints", "build-2")
  dir.create(second_checkpoint, recursive = TRUE)
  second_primary <- file.path(second_checkpoint, "ds1.crb")
  second_member <- file.path(second_checkpoint, "ds1.h5")
  writeBin(charToRaw("primary"), second_primary)
  writeBin(charToRaw("member"), second_member)
  adopted <- runtime$builder_project_store_artifact_bundle(
    second_primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = root,
    promote = TRUE
  )

  expect_identical(adopted$path, bundle$path)
  expect_true(file.exists(file.path(root, bundle$path)))
  expect_true(file.exists(second_primary))
  expect_true(file.exists(second_member))

  implementation <- paste(
    deparse(body(runtime$builder_project_store_artifact_bundle)),
    collapse = "\n"
  )
  rename_failure <- regexpr(
    "if (!file.rename(staging, generation_dir))",
    implementation,
    fixed = TRUE
  )[[1L]]
  concurrent_adoption <- regexpr(
    "if (dir.exists(generation_dir))",
    implementation,
    fixed = TRUE
  )[[1L]]
  expect_gt(rename_failure, 0L)
  expect_gt(concurrent_adoption, rename_failure)
  expect_match(
    implementation,
    "return(read_existing_generation())",
    fixed = TRUE
  )
})

test_that("configuration identity cache invalidates only on entry revision", {
  runtime <- builder_project_test_runtime()
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  digest <- function(entry) {
    calls <<- calls + 1L
    paste0("digest-", entry$revision)
  }
  entry <- list(id = "ds1", revision = 1L, settings = list(name = "Dataset"))

  first <- runtime$builder_project_cached_configuration_digest(
    entry,
    cache,
    digest
  )
  second <- runtime$builder_project_cached_configuration_digest(
    entry,
    cache,
    digest
  )
  entry$revision <- 2L
  third <- runtime$builder_project_cached_configuration_digest(
    entry,
    cache,
    digest
  )

  expect_identical(first, second)
  expect_false(identical(second, third))
  expect_identical(calls, 2L)
})

test_that("configuration identity reuses a spatial source content digest", {
  runtime <- builder_project_test_runtime()
  content_md5 <- strrep("a", 32L)
  entry <- list(
    settings = list(
      images = list(
        section = list(
          image = list(
            source_path = "C:/private/image.png",
            source_content_md5 = content_md5
          )
        )
      )
    )
  )
  expect_match(
    runtime$builder_project_configuration_digest(entry),
    "^[[:xdigit:]]{32}$"
  )
})

test_that("configuration identity cache bounds variants", {
  runtime <- builder_project_test_runtime()
  cache <- new.env(parent = emptyenv())
  entry <- list(id = "ds1", revision = 1L, settings = list())
  for (index in 1:20) {
    runtime$builder_project_cached_configuration_digest(
      entry,
      cache,
      digest = function(entry) paste0("digest-", index),
      variant = paste0("draft-", index)
    )
  }
  expect_lte(length(ls(cache)), 8L)
})

test_that("terminal checkpoint cleanup requires a committed failed manifest", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  checkpoint <- file.path(root, "checkpoints", "build-1")
  dir.create(checkpoint, recursive = TRUE)
  writeBin(charToRaw("checkpoint"), file.path(checkpoint, "dataset.crb"))

  expect_false(runtime$builder_project_cleanup_terminal_checkpoint(
    saved = FALSE,
    status = "failed",
    path = checkpoint,
    root = root
  ))
  expect_true(dir.exists(checkpoint))
  expect_true(runtime$builder_project_cleanup_terminal_checkpoint(
    saved = TRUE,
    status = "failed",
    path = checkpoint,
    root = root
  ))
  expect_false(dir.exists(checkpoint))
})

test_that("checkpoint cleanup refuses lexical symlinks without deleting targets", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  checkpoint_root <- file.path(root, "checkpoints")
  target <- file.path(checkpoint_root, "retained")
  link <- file.path(checkpoint_root, "build-link")
  dir.create(target, recursive = TRUE)
  writeBin(charToRaw("checkpoint"), file.path(target, "dataset.crb"))
  skip_if_not(file.symlink(target, link), "symbolic links are unavailable")

  expect_false(runtime$builder_project_cleanup_checkpoint(link, root))
  expect_true(nzchar(Sys.readlink(link)))
  expect_true(file.exists(file.path(target, "dataset.crb")))
})

test_that("project open clears all cross-project runtime caches", {
  source <- paste(
    readLines(testthat::test_path(
      "..",
      "..",
      "inst",
      "builder",
      "server",
      "project.R"
    )),
    collapse = "\n"
  )

  expect_match(source, "projection_previews(list())", fixed = TRUE)
  expect_match(source, "trajectory_previews(list())", fixed = TRUE)
  expect_match(source, "spatial_previews(list())", fixed = TRUE)
  expect_match(
    source,
    "builder_project_configuration_cache_clear(",
    fixed = TRUE
  )
  expect_match(
    source,
    "builder_configuration_identity_cache",
    fixed = TRUE
  )
  expect_match(source, "invalidate_builder_project_source_sync()", fixed = TRUE)
})

test_that("source sync context is bound to project identity and revision", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project_a <- list(
    root = root,
    path = file.path(root, "builder-project.json"),
    manifest = list(project = list(id = "project-a", revision = 3L))
  )
  context <- runtime$builder_project_source_context(
    project_a,
    generation = 4L,
    active_ids = "ds1"
  )

  expect_true(runtime$builder_project_source_context_matches(
    context,
    project_a,
    generation = 4L
  ))
  project_b <- project_a
  project_b$manifest$project$id <- "project-b"
  expect_false(runtime$builder_project_source_context_matches(
    context,
    project_b,
    generation = 4L
  ))
  project_a$manifest$project$revision <- 4L
  expect_false(runtime$builder_project_source_context_matches(
    context,
    project_a,
    generation = 4L
  ))
  expect_false(runtime$builder_project_source_context_matches(
    context,
    project_b,
    generation = 5L
  ))

  server <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(server, "queued[[job$id]] <- job", fixed = TRUE)
  expect_false(grepl("if (!job$id %in% active)", server, fixed = TRUE))
})

test_that("checkpoint preparation requires a durable project save before enqueue", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  checkpoint_start <- regexpr(
    "enqueue_builder_project_checkpoint <- function(",
    source,
    fixed = TRUE
  )[[1L]]
  checkpoint_end <- regexpr(
    "prepare_builder_project_crbs <- function(",
    source,
    fixed = TRUE
  )[[1L]]
  checkpoint <- substr(source, checkpoint_start, checkpoint_end - 1L)

  expect_match(checkpoint, "saved <- save_builder_project_state(", fixed = TRUE)
  expect_match(checkpoint, "if (!isTRUE(saved))", fixed = TRUE)
  expect_match(checkpoint, "builder_project_checkpoint(FALSE)", fixed = TRUE)
  expect_match(
    source,
    "builder_project_cleanup_checkpoint(plan$out_dir, previous_project$root)",
    fixed = TRUE
  )
  expect_match(
    source,
    "allow_missing_leaf = TRUE",
    fixed = TRUE
  )
})
