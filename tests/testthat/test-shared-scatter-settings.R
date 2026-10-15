test_that("projection defaults are canonical and spatial defaults independent", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), env)
  options <- list(point_size = c(a = 3, b = 8), point_opacity = c(a = .4, b = .9),
    percentage_cells_to_show = c(a = 70, b = 100), expression_point_size = c(a = 10, b = 12),
    spatial_percentage_cells_to_show = c(a = 50, b = 80))
  for (id in c("a", "b")) for (page in c("projection", "expression", "linked", "trajectory"))
    expect_identical(env$viewerScatterDefaults(options, id, page), env$viewerScatterDefaults(options, id))
  expect_identical(env$viewerScatterDefaults(options, "a", "spatial"),
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 50))
  expect_equal(env$viewerScatterDefaults(list(point_size = c(a = 2)), "b")$point_size, 5)
})

test_that("function coordinate settings validate dataset, FOV and numeric bounds", {
  catalogs <- list(a = list(section = list()), b = list(other = list()))
  points <- list(a = list(section = list(point_size = 4, point_opacity = .6)))
  rois <- list(a = list(section = list(region = list(rotation_degrees = 30,
    point_size = 2, point_opacity = .5))))
  expect_equal(.normalizeAppSpatialPointSettings(points, catalogs), points)
  expect_equal(.normalizeAppSpatialPointSettings(rois, catalogs, roi = TRUE), rois)
  expect_error(.normalizeAppSpatialPointSettings(list(unknown = points$a), catalogs), "unknown dataset")
  expect_error(.normalizeAppSpatialPointSettings(list(b = points$a), catalogs), "unknown FOV")
  points$a$section$point_opacity <- 2
  expect_error(.normalizeAppSpatialPointSettings(points, catalogs), "point_opacity")
})
