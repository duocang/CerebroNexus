# test-trajectory.R — Tests for trajectory module

# The full T+B demo now carries the monocle2 B-cell trajectory (the former
# standalone trajectory-only demo was consolidated into it).
trajectory_crb <- system.file(
  "extdata/examples/demo_full_tcr_bcr.crb",
  package = "CerebroNexus"
)

## The other trajectory module files are sourced or parsed by the page tests.
test_that("trajectory method selector parses without errors", {
  expect_no_error(parse(
    file = viewer_test_path("trajectory", "select_method_and_name.R")
  ))
})

test_that("trajectory UI defines correct tabName", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("trajectory", "UI.R"), scope)
  page <- htmltools::tagQuery(scope$tab_trajectory)
  expect_equal(page$filter("#shiny-tab-trajectory")$length(), 1L)
})

test_that("full T+B demo trajectory data is accessible and complete", {
  crb <- readCerebro(trajectory_crb)
  methods <- crb$getMethodsForTrajectories()
  expect_true("monocle2" %in% methods)
  names <- crb$getNamesOfTrajectories(methods[1])
  expect_true(is.character(names))
  expect_true(length(names) > 0)
  traj <- crb$getTrajectory(methods[1], names[1])
  expect_true(is.list(traj))
  expect_true(all(c("meta", "edges") %in% names(traj)))
  expect_true(is.data.frame(traj$meta))
  expect_true(is.data.frame(traj$edges))
  expect_true(nrow(traj$meta) > 0)
  expect_true("pseudotime" %in% colnames(traj$meta))
  expect_true("state" %in% colnames(traj$meta))
  expect_true("B_cell_maturation" %in% crb$getNamesOfTrajectories("monocle2"))
})

test_that("getTrajectory bug is fixed in class definition", {
  crb <- readCerebro(trajectory_crb)
  expect_error(
    crb$getTrajectory("missing", "missing"),
    "not available|not found"
  )
  expect_error(
    crb$getTrajectory("monocle2", "missing"),
    "not available|not found"
  )
  expect_gt(nrow(crb$getTrajectory("monocle2", "B_cell_maturation")$meta), 0L)
})

test_that("trajectory helper utilities are defined in the app scope", {
  scope <- viewer_runtime_env()
  metadata <- data.frame(
    cell_barcode = "c1",
    group = "A",
    percent.mt = 2,
    pct_ribo = 3,
    percent.hb = 4
  )
  scope$getMetaData <- function() metadata
  scope$getGroups <- function() "group"
  scope$Cerebro.options <- list()
  expect_identical(scope$getVariableToCompareChoices(), names(metadata)[-1])
  scope$Cerebro.options$variable_to_compare <- TRUE
  expect_identical(scope$getVariableToCompareChoices(), "group")
  for (pair in list(
    c("Mito", "percent.mt"),
    c("Ribo", "pct_ribo"),
    c("Ery", "percent.hb")
  )) {
    expect_identical(scope[[paste0("get", pair[1], "Column")]](), pair[2])
    expect_true(scope[[paste0("has", pair[1], "Column")]]())
  }
  metadata <- metadata["cell_barcode"]
  for (kind in c("Mito", "Ribo", "Ery")) {
    expect_null(scope[[paste0("get", kind, "Column")]]())
    expect_false(scope[[paste0("has", kind, "Column")]]())
  }
})
