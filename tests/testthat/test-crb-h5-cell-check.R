test_that("a daemon confirms an H5 file's cell names once per process", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("mirai")
  crb_cache <- new.env()
  sys.source(viewer_test_path("crb_cache.R"), crb_cache)
  sidecar <- withr::local_tempfile(fileext = ".h5")
  cells <- c("AAAC-1", "AAAG-1", "AAAT-1")
  rhdf5::h5createFile(sidecar)
  rhdf5::h5createGroup(sidecar, "expression")
  rhdf5::h5write(cells, sidecar, "expression/genes")
  rhdf5::h5closeAll()
  expected <- rlang::hash(enc2utf8(cells))

  crb_cache$crb_h5_cell_check_start(sidecar, expected)
  expect_true(crb_cache$crb_h5_cells_verified(sidecar, expected))
  expect_false(crb_cache$crb_h5_cells_pending(sidecar, expected))

  ## Other names fail the check; the session then reads and refuses them.
  crb_cache$crb_h5_cell_check_start(sidecar, "another-hash")
  expect_false(crb_cache$crb_h5_cells_verified(sidecar, "another-hash"))
  ## No check started: nothing is confirmed until the session records one.
  expect_false(crb_cache$crb_h5_cells_verified(sidecar, "unchecked"))
  crb_cache$crb_h5_cells_confirm(sidecar, "unchecked")
  expect_true(crb_cache$crb_h5_cells_verified(sidecar, "unchecked"))
})
