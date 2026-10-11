# Separate Spatial protocol v1: deterministic canonical 1000 x 1000 grid.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
root <- normalizePath(args[[1L]])
input <- normalizePath(args[[2L]])
output <- normalizePath(args[[3L]], mustWork = FALSE)
stopifnot(identical(dirname(input), dirname(output)), !file.exists(output))
pkgload::load_all(root, quiet = TRUE)
object <- readCerebro(input)
cells <- as.character(object$getMetaData()$cell_barcode)
stopifnot(length(cells) == 1000000L)
coords <- expand.grid(x = 0:999, y = 0:999)
rownames(coords) <- cells
image_file <- paste0(output, "-alignment.png")
checker <- outer(0:999, 0:999, function(x, y) ((x %/% 100L + y %/% 100L) %% 2L) * 0.2 + 0.65)
png::writePNG(checker, image_file)
object$addSpatialData("canonical_grid", list(
  coordinates = coords,
  expression = matrix(numeric(), nrow = 0L, ncol = length(cells),
    dimnames = list(character(), cells)),
  histology_images = list(alignment = list(
    histology_image = paste0("data:image/png;base64,", base64enc::base64encode(image_file)),
    histology_image_bounds = c(xmin = 0, xmax = 999, ymin = 0, ymax = 999)
  ))
))
saveCerebro(object, output)
identity <- list(protocol = "spatial-canonical-grid-v1", cells = length(cells),
  fingerprint = object$cell_fingerprint, grid = "x varies first; x,y=0:999; canonical cell order",
  source_sha256 = digest::digest(file = input, algo = "sha256"),
  dataset_sha256 = digest::digest(file = output, algo = "sha256"),
  alignment_sha256 = digest::digest(file = image_file, algo = "sha256"),
  generator_sha256 = digest::digest(file = file.path(root,
    "tests/bench/viewer-final-protocol/prepare-fixed-spatial.R"), algo = "sha256"))
jsonlite::write_json(identity, paste0(output, ".identity.json"), auto_unbox = TRUE, pretty = TRUE)
print(identity)
