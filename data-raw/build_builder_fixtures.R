#!/usr/bin/env Rscript
## Build the single synthetic Seurat input used by the Builder gallery.

arguments <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", arguments, value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run this script with Rscript from the repository root.", call. = FALSE)
}
script <- normalizePath(sub("^--file=", "", script_argument), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
if (!identical(normalizePath(getwd(), mustWork = TRUE), repo)) {
  stop(
    "Run this script from the CerebroNexus repository root: ",
    repo,
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
})
sys.source(file.path("inst", "builder", "io.R"), envir = environment())

output <- file.path("inst", "builder", "fixtures")
builder_write_permanent_fixtures(output)
cat(
  "Wrote All content Seurat fixture and five histology sidecars to ",
  output,
  "\n"
)
