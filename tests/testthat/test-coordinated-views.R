# test-coordinated-views.R — Tests for the Linked views bundle builders.
#
# cv_build_bundle() and its cv_build_* / cv_* helpers (coordinated_views/bundle.R)
# turn a loaded Cerebro object into ONE client bundle. They are pure — no
# input/output/reactive scope — so we source the file into an isolated env and
# drive it directly. Two past regressions are guarded here explicitly:
#   * a single-level group / single clonotype serialising as a JSON scalar
#     instead of an array (the I()/AsIs contract in cv_group/cv_space/cv_clone),
#     which made the client throw mid-update and keep the previous data set; and
#   * a standard `spatial` slot and a `trekker` slot collapsing into one space
#     instead of two — the omnibus demo carries BOTH and is the fixture for it.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
local_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "shiny/v1.4"
))][1]
if (!is.na(local_inst)) {
  bundle_file <- file.path(
    local_inst,
    "viewer/coordinated_views/bundle.R"
  )
  omnibus_crb <- file.path(local_inst, "extdata/v1.4/demo_omnibus.crb")
} else {
  bundle_file <- system.file(
    "viewer/coordinated_views/bundle.R",
    package = "CerebroNexus"
  )
  omnibus_crb <- system.file(
    "extdata/v1.4/demo_omnibus.crb",
    package = "CerebroNexus"
  )
}

## Source the pure builders into an isolated env (no app scope). The app-only
## helpers reactive_colors() / cerebro_group_colors() are absent here; bundle.R
## guards each, so cv_build_bundle() simply falls back to its own palette.
cv_env <- new.env()
have_bundle <- nzchar(bundle_file) && file.exists(bundle_file)
if (have_bundle) {
  sys.source(bundle_file, envir = cv_env)
}

test_that("bundle.R parses and defines the builder API", {
  skip_if_not(have_bundle, "coordinated_views/bundle.R not found")
  expect_no_error(parse(file = bundle_file))
  api <- c(
    "cv_group",
    "cv_space",
    "cv_clone",
    "cv_build_groups",
    "cv_build_projections",
    "cv_build_spatial",
    "cv_build_trekker",
    "cv_build_clone",
    "cv_build_bundle"
  )
  for (fn in api) {
    expect_true(is.function(get0(fn, envir = cv_env)), info = fn)
  }
})

test_that("cv_group/cv_space/cv_clone force JSON arrays even at length 1", {
  skip_if_not(have_bundle)
  skip_if_not_installed("jsonlite")
  arr <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  }

  ## A single-level group: values/levels/colors must all still be JSON arrays.
  g <- cv_env$cv_group(0L, "OnlyLevel", "#123456")
  expect_s3_class(g$levels, "AsIs")
  expect_s3_class(g$colors, "AsIs")
  expect_match(arr(g$levels), "^\\[")
  expect_match(arr(g$colors), "^\\[")
  expect_match(arr(g$values), "^\\[")

  ## A single-cell space: x/y are arrays, id/label stay scalar.
  s <- cv_env$cv_space("umap", "x (expression)", 1, 2)
  expect_match(arr(s$x), "^\\[")
  expect_match(arr(s$y), "^\\[")

  ## A single-clonotype bundle: id/label/size are arrays; the true scalars
  ## (n_clones / n_receptor) stay bare so the client reads them as numbers.
  cl <- cv_env$cv_clone(0L, "CASSLGX", 1L, 1L, 1L)
  expect_match(arr(cl$size), "^\\[")
  expect_match(arr(cl$label), "^\\[")
  expect_false(startsWith(arr(cl$n_clones), "["))
})

test_that("cv_clone_per_cell aligns clone identity to cells, NA when unmatched", {
  skip_if_not(have_bundle)
  ir <- list(data.frame(
    barcode = c("c1", "c2", "c3"),
    CTstrict = c("A", "A", "B"),
    CTaa = c("CASSL", "CASSL", "CASSF"),
    stringsAsFactors = FALSE
  ))
  cells <- c("c3", "c1", "cX") # cX carries no receptor
  out <- cv_env$cv_clone_per_cell(ir, cells)
  expect_equal(out$ctstrict, c("B", "A", NA))
  expect_equal(out$ctaa, c("CASSF", "CASSL", NA))
  ## No IR at all -> NULL (the immune axis is simply skipped).
  expect_null(cv_env$cv_clone_per_cell(NULL, cells))
})

test_that("cv_build_bundle assembles every modality from the omnibus demo", {
  skip_if_not(have_bundle)
  skip_if_not(
    nzchar(omnibus_crb) && file.exists(omnibus_crb),
    "omnibus demo .crb not available"
  )
  skip_if_not(
    requireNamespace("CerebroNexus", quietly = TRUE),
    "CerebroNexus not loaded (R6 class needed to read the .crb)"
  )
  crb <- tryCatch(readRDS(omnibus_crb), error = function(e) NULL)
  skip_if(is.null(crb), "omnibus demo .crb not loadable")

  b <- cv_env$cv_build_bundle(crb)
  expect_type(b, "list")
  expect_gt(b$n, 0)
  expect_length(b$cells, b$n)

  ## spaces: umap + spatial + trekker + clone, each a DISTINCT id. The
  ## spatial<->trekker coexistence is the regression this fixture exists for.
  ids <- vapply(b$spaces, function(s) s$id, "")
  expect_true(all(c("umap", "spatial", "trekker", "clone") %in% ids))
  expect_equal(anyDuplicated(ids), 0L)

  ## every space is aligned to the SAME cell vector (keyed on cell index).
  for (s in b$spaces) {
    expect_length(s$x, b$n)
    expect_length(s$y, b$n)
  }

  ## multi-sample spatial: the three donor sections travel in $samples so the
  ## client can switch between them.
  sp <- b$spaces[[which(ids == "spatial")]]
  expect_false(is.null(sp$samples))
  expect_gte(length(sp$samples), 2)

  ## groups present and aligned; the immune axis adds clone_expansion.
  expect_gte(length(b$groups), 1)
  expect_true("clone_expansion" %in% names(b$groups))
  for (g in b$groups) {
    expect_length(g$values, b$n)
  }

  ## side-bundles for the Trekker QC modal and the clone readout.
  expect_false(is.null(b$trekker))
  expect_false(is.null(b$clone))
  expect_true(b$default_projection %in% names(b$projections))
  expect_true(b$default_group %in% names(b$groups))
})
