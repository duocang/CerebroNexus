ir_cache_functions <- function() {
  scope <- new.env(parent = globalenv())
  scope$IR_ENCODING_VERSION <- 1L
  for (expr in parse(viewer_test_path("immune_repertoire", "data.R"))) {
    if (
      is.call(expr) &&
        as.character(expr[[1]])[1] %in% c("<-", "=") &&
        is.call(expr[[3]]) &&
        identical(expr[[3]][[1]], as.name("function"))
    ) {
      eval(expr, scope)
    }
  }
  scope
}

test_that("an encoding made in memory is kept for the next session", {
  skip_if_not_installed("qs2")
  withr::local_envvar(CEREBRO_IR_CACHE = withr::local_tempdir())
  scope <- ir_cache_functions()
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    list(version = 1L, sample = 1:3)
  }
  key <- list(path = "a.crb", size = 1, mtime = 2, repertoire = "")
  first <- scope$ir_process_memo(
    "encoded",
    key,
    compute,
    new.env(),
    disk = TRUE
  )
  expect_identical(calls, 1L)
  ## A new process (an empty memo) reads the file instead of encoding again.
  second <- scope$ir_process_memo(
    "encoded",
    key,
    compute,
    new.env(),
    disk = TRUE
  )
  expect_identical(calls, 1L)
  expect_identical(second, first)
  ## Another data set, or a changed file, is another entry.
  other <- modifyList(key, list(size = 9))
  scope$ir_process_memo("encoded", other, compute, new.env(), disk = TRUE)
  expect_identical(calls, 2L)
  ## Without `disk` nothing is written or read.
  scope$ir_process_memo("plain", key, compute, new.env())
  expect_identical(calls, 3L)
  expect_length(list.files(Sys.getenv("CEREBRO_IR_CACHE")), 2L)
})

test_that("a damaged or outdated cache file is encoded again", {
  skip_if_not_installed("qs2")
  withr::local_envvar(CEREBRO_IR_CACHE = withr::local_tempdir())
  scope <- ir_cache_functions()
  key <- list(path = "b.crb")
  file <- scope$ir_disk_cache_file("encoded", key)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines("not a qs2 file", file)
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    list(version = 1L)
  }
  scope$ir_process_memo("encoded", key, compute, new.env(), disk = TRUE)
  expect_identical(calls, 1L)
  ## Written again, readable now; an older encoding version is not used.
  expect_identical(scope$ir_disk_cache_read(file)$version, 1L)
  qs2::qs_save(list(version = 0L), file)
  expect_null(scope$ir_disk_cache_read(file))
  ## A cache directory that cannot be written is not an error.
  withr::local_envvar(CEREBRO_IR_CACHE = file.path(file, "inside-a-file"))
  expect_no_error(
    scope$ir_process_memo("encoded", key, compute, new.env(), disk = TRUE)
  )
})

test_that("encoding in memory says why and how to avoid it", {
  scope <- ir_cache_functions()
  expect_message(
    scope$ir_notify_slow_encoding(list(manifest = list())),
    "built before the fast Immune repertoire format"
  )
  expect_message(
    scope$ir_notify_slow_encoding(NULL),
    "has no Viewer Pack"
  )
  expect_message(
    scope$ir_notify_slow_encoding(NULL),
    "buildViewerPack(<crb file>, overwrite = TRUE)",
    fixed = TRUE
  )
})
