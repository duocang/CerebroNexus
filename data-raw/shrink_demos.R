# Cuts the shipped example data sets to the demo gene panel in place, without
# rebuilding them from their (large) sources: cells, embeddings, QC and every
# analysis table are kept, only the expression rows (and the spatial expression)
# are subset by demo_gene_panel(). The cap is 500 genes, 100 for a file that is
# still over 3 MB, and the per-file caps in `caps` below. A file that already has no more genes than its cap is
# left untouched, so the script can be run again. Run from the package root:
#   Rscript data-raw/shrink_demos.R
devtools::load_all(".", quiet = TRUE)
source("data-raw/demo_gene_panel.R")

caps <- c(
  demo_trekker = 250L,
  demo_spatial_merfish = 200L,
  demo_spatial_xenium = 200L,
  demo_spatial_slideseq = 300L,
  example = 300L
)

for (file in list.files("inst/extdata/examples", "[.]crb$", full.names = TRUE)) {
  object <- readCerebro(file)
  before <- nrow(object$expression)
  name <- tools::file_path_sans_ext(basename(file))
  cap <- if (name %in% names(caps)) {
    caps[[name]]
  } else if (file.size(file) > 3e6) {
    100L
  } else {
    500L
  }
  if (before <= cap) {
    message(basename(file), ": ", before, " genes, unchanged")
    next
  }
  save_demo_crb(demo_gene_panel(object, cap), file)
  message(basename(file), ": ", before, " -> ", nrow(readCerebro(file)$expression), " genes, ", file.size(file), " bytes")
}
