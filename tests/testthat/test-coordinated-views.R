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

test_that("cv_build_fields turns every numeric meta column into a colouring", {
  skip_if_not(have_bundle)
  md <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    cluster = c("a", "b", "a"),
    nUMI = c(100, 200, 300),
    percent.mt = c(1.5, NA, 3.5),
    constant = c(7, 7, 7),
    stringsAsFactors = FALSE
  )
  f <- cv_env$cv_build_fields(md)
  ## QC-looking columns are exactly what users colour by — they must be offered.
  expect_true("meta:nUMI" %in% names(f))
  expect_true("meta:percent.mt" %in% names(f))
  ## no colouring can be built from a constant column or a non-numeric one
  expect_false("meta:constant" %in% names(f))
  expect_false("meta:cluster" %in% names(f))
  expect_false("meta:cell_barcode" %in% names(f))
  ## the quantised vector is aligned to the rows, spans the full scale, and
  ## carries the true range so the client can show real values, not 0-255.
  fu <- f[["meta:nUMI"]]
  expect_length(fu$v, nrow(md))
  expect_equal(as.integer(fu$v), c(0L, fu$scale %/% 2L, fu$scale))
  expect_equal(fu$min, 100)
  expect_equal(fu$max, 300)
  ## NA stays NA so the client can draw it as "no value" rather than as zero.
  expect_true(is.na(f[["meta:percent.mt"]]$v[2]))
})

test_that("cv_build_extra_groups covers categorical columns that are not groups", {
  skip_if_not(have_bundle)
  md <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    cluster = c("a", "b", "a"),
    dextramer_allele = c("A*02:01", "A*02:01", "B*07:02"),
    stringsAsFactors = FALSE
  )
  eg <- cv_env$cv_build_extra_groups(md, "cluster", function(g, lev) {
    cv_env$cv_colors_for(lev)
  })
  ## registered groups stay out (they are already offered, and they own the
  ## group filters); unregistered categorical columns come in.
  expect_false("cluster" %in% names(eg$groups))
  expect_true("dextramer_allele" %in% names(eg$groups))
  expect_equal(
    as.character(eg$groups$dextramer_allele$levels),
    c("A*02:01", "B*07:02")
  )
  expect_equal(as.integer(eg$groups$dextramer_allele$values), c(0L, 0L, 1L))
  expect_length(eg$skipped, 0)

  ## A column with as many levels as cells is an identifier, not a grouping —
  ## not colourable, but REPORTED rather than dropped, so the picker can say why
  ## a column the Projection tab offers is missing here.
  md$barcode_copy <- md$cell_barcode
  eg2 <- cv_env$cv_build_extra_groups(md, "cluster", function(g, lev) {
    cv_env$cv_colors_for(lev)
  })
  expect_false("barcode_copy" %in% names(eg2$groups))
  expect_true("barcode_copy" %in% names(eg2$skipped))
  expect_equal(eg2$skipped$barcode_copy, 3L)
})

test_that("cv_build_projections records dimensionality instead of dropping it", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  crb <- list(
    availableProjections = function() c("umap", "umap_3D"),
    getProjection = function(n) {
      m <- if (n == "umap_3D") {
        matrix(1:6, nrow = 2, dimnames = list(cells, c("x", "y", "z")))
      } else {
        matrix(1:4, nrow = 2, dimnames = list(cells, c("x", "y")))
      }
      m
    }
  )
  pj <- cv_env$cv_build_projections(crb, cells)
  expect_equal(pj$umap$ndim, 2L)
  expect_equal(pj$umap_3D$ndim, 3L)
  ## The third dimension travels so the client can rotate the embedding rather
  ## than show a flattened shadow of it. A 2-D projection must NOT carry a z —
  ## the client uses its presence to decide which panels can be turned.
  expect_null(pj$umap$z)
  expect_length(pj$umap_3D$z, length(cells))
  expect_equal(as.numeric(pj$umap_3D$z), c(5, 6))
  ## and it is array-wrapped, like every other per-cell vector in the bundle
  expect_s3_class(pj$umap_3D$z, "AsIs")
})

test_that("cv_build_bundle still works when no grouping variable is registered", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2", "c3")
  md <- data.frame(
    cell_barcode = cells,
    nUMI = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() md,
    getGroups = function() character(0),
    availableProjections = function() "umap",
    getProjection = function(n) {
      matrix(1:6, nrow = 3, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL
  )
  b <- cv_env$cv_build_bundle(crb)
  ## Projection colours cells by ANY meta column, so an object with no
  ## registered group is perfectly usable there — Linked views must not go blank.
  expect_type(b, "list")
  expect_equal(b$n, 3L)
  expect_true("meta:nUMI" %in% names(b$fields))
  ## with nothing categorical to fall back on, the default colouring is the
  ## first continuous field, expressed as the client's field mode string.
  expect_equal(b$default_group, paste0(cv_env$cv_field_mode, "meta:nUMI"))
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

  ## Colour-by parity with the Projection tab, which offers EVERY meta column.
  ## Whatever the meta data holds must be reachable through exactly one of the
  ## three lists — a column that is in none of them cannot be coloured by, which
  ## is the gap that kept Linked views from replacing the Projection tab.
  md <- crb$getMetaData()
  ## cat_skipped counts as accounted-for: those columns cannot be coloured by,
  ## but the picker lists them (disabled, with the reason) rather than dropping
  ## them, so they are still reachable as an answer.
  offered <- c(
    names(b$groups),
    names(b$cat_extra),
    names(b$cat_skipped),
    sub(
      "^meta:",
      "",
      names(b$fields)
    )
  )
  for (cn in setdiff(colnames(md), "cell_barcode")) {
    v <- md[[cn]]
    ## constant columns have no colouring to offer; everything else must
    if (length(unique(v[!is.na(v)])) <= 1) {
      next
    }
    expect_true(cn %in% offered, info = cn)
  }
  ## the QC columns specifically — they were previously filtered out
  expect_true("meta:nCount_RNA" %in% names(b$fields))
  ## fields carry a true range and a quantisation scale, not raw 0-255 codes
  for (f in b$fields) {
    expect_length(f$v, b$n)
    expect_true(is.numeric(f$scale) && f$scale > 0)
    expect_true(f$max > f$min)
  }
  ## Trekker's physical fields join the SAME list (no second lookup on $trekker)
  expect_null(b$trekker$fields)
  expect_true(any(!grepl("^meta:", names(b$fields))))
  ## every projection reports its dimensionality
  for (p in b$projections) {
    expect_true(is.numeric(p$ndim) && p$ndim >= 2)
  }
  ## The omnibus is the only demo carrying reductions that are not flat, and it
  ## carries BOTH shapes the client treats differently: exactly three components
  ## (a RunUMAP(n.components = 3) result) and more than three (a PCA, of which
  ## only the first three are rendered). Without them the rotate path had no
  ## demo to run on at all.
  expect_equal(b$projections$umap_3d$ndim, 3L)
  expect_length(b$projections$umap_3d$z, b$n)
  expect_equal(b$projections$pca$ndim, 5L)
  expect_length(b$projections$pca$z, b$n)
  ## and the flat ones must NOT carry a z — its presence is what the client uses
  ## to decide a panel can be turned
  expect_null(b$projections$umap$z)
  expect_null(b$projections$tsne$z)
})
