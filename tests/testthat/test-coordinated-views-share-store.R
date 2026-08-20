# test-coordinated-views-share-store.R — persistent linked-view share records.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE),
  normalizePath(system.file(package = "CerebroNexus"), mustWork = FALSE)
)
share_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "viewer"
))][1]
share_file <- if (is.na(share_inst)) {
  ""
} else {
  file.path(
    share_inst,
    "viewer/coordinated_views/share_store.R"
  )
}
share_env <- new.env(parent = baseenv())
if (nzchar(share_file) && file.exists(share_file)) {
  sys.source(share_file, envir = share_env)
}

test_that("a share record is opaque, revocable, and expires after ninety days", {
  expect_true(exists(
    "cv_share_store_open",
    envir = share_env,
    inherits = FALSE
  ))
  store <- share_env$cv_share_store_open(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(store$con))
  created_at <- as.POSIXct("2026-08-20 12:00:00", tz = "UTC")
  created <- share_env$cv_share_store_create(
    store,
    '{"schema":"test"}',
    "fingerprint-a",
    now = created_at
  )
  expect_match(created$token, "^[A-Za-z0-9_-]{43}$")
  expect_match(created$receipt, "^[A-Za-z0-9_-]{43}$")
  expect_equal(
    share_env$cv_share_store_fetch(
      store,
      created$token,
      "fingerprint-a",
      now = created_at
    )$json,
    '{"schema":"test"}'
  )
  expect_error(
    share_env$cv_share_store_fetch(
      store,
      created$token,
      "fingerprint-b",
      now = created_at
    ),
    class = "cv_share_error"
  )
  expect_error(
    share_env$cv_share_store_revoke(
      store,
      created$token,
      "wrong-receipt",
      now = created_at
    ),
    class = "cv_share_error"
  )
  share_env$cv_share_store_revoke(
    store,
    created$token,
    created$receipt,
    now = created_at
  )
  expect_error(
    share_env$cv_share_store_fetch(
      store,
      created$token,
      "fingerprint-a",
      now = created_at
    ),
    class = "cv_share_error"
  )
  expired <- share_env$cv_share_store_create(
    store,
    '{"schema":"test"}',
    "fingerprint-a",
    now = created_at
  )
  expect_error(
    share_env$cv_share_store_fetch(
      store,
      expired$token,
      "fingerprint-a",
      now = created_at + 90 * 24 * 60 * 60 + 1
    ),
    class = "cv_share_error"
  )
})

test_that("a browser may supply cryptographically generated share credentials", {
  store <- share_env$cv_share_store_open(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(store$con))
  token <- paste(rep("A", 43L), collapse = "")
  receipt <- paste(rep("B", 43L), collapse = "")
  created <- share_env$cv_share_store_create(
    store,
    '{"schema":"test"}',
    "fingerprint-a",
    token = token,
    receipt = receipt
  )

  expect_identical(created$token, token)
  expect_identical(created$receipt, receipt)
  expect_error(
    share_env$cv_share_store_create(
      store,
      '{"schema":"test"}',
      "fingerprint-a",
      token = token,
      receipt = receipt
    ),
    class = "cv_share_error"
  )
})
