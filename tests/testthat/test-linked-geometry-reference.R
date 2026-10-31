test_that('Linked references only validated canonical two-dimensional trajectory frames', {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path('core', 'viewer_pack.R'), scope)
  sys.source(viewer_test_path('coordinated_views', 'bundle.R'), scope)
  scope$viewerPackTrajectoryFrame <- function(pack, method, name) pack$frame
  primary <- list(
    n = 3L,
    canonical_order_id = 'order',
    projections = list(umap = list(ndim = 2L))
  )
  pack <- list(
    cell_count = 3L,
    manifest = list(cell_order_fingerprint = 'order'),
    frame = list(
      geometry_kind = 'canonical_projection',
      projection_name = 'umap',
      cells = 2L
    )
  )
  trajectory <- list(
    meta = data.frame(
      DR_1 = c(1, NA, 3),
      DR_2 = c(4, NA, 6),
      row.names = c('a', 'b', 'c')
    )
  )
  object <- structure(
    list(
      getMethodsForTrajectories = function() 'm',
      getNamesOfTrajectories = function(method) 't',
      getTrajectory = function(method, name) trajectory
    ),
    cerebro_viewer_pack = pack
  )
  build <- function(primary) {
    scope$cv_build_trajectories(object, c('a', 'b', 'c'), primary = primary)[[
      1
    ]]
  }
  result <- build(primary)
  expect_null(result$x)
  expect_null(result$y)
  expect_identical(result$geometry_ref$projection, 'umap')
  expect_equal(as.integer(result$geometry_ref$exclude), 1L)
  ## Without a primary, on a 3D projection or another cell order the
  ## coordinates are sent.
  expect_equal(unclass(build(NULL)$x), c(1, NA, 3))
  primary$projections$umap$ndim <- 3L
  expect_equal(unclass(build(primary)$y), c(4, NA, 6))
  primary$projections$umap$ndim <- 2L
  primary$canonical_order_id <- 'other'
  expect_null(build(primary)$geometry_ref)
  expect_equal(unclass(build(primary)$x), c(1, NA, 3))
})
