# Write a deterministic checksum inventory for a completed evidence package.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("usage: 49_write_evidence_manifest.R <stage_dir>", call. = FALSE)
}
stage <- normalizePath(args[[1]], mustWork = TRUE)
output <- file.path(stage, "evidence_manifest.csv")
files <- list.files(
  stage,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)
files <- files[
  !dir.exists(files) &
    normalizePath(files) != normalizePath(output, mustWork = FALSE)
]
relative <- substring(files, nchar(stage) + 2L)
order <- order(relative, method = "radix")
files <- files[order]
relative <- relative[order]
if (!length(files)) {
  stop("cannot inventory an empty evidence package", call. = FALSE)
}
info <- file.info(files)
manifest <- data.frame(
  path = gsub("\\\\", "/", relative),
  bytes = as.numeric(info$size),
  md5 = unname(tools::md5sum(files)),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, output, row.names = FALSE, na = "")
message("wrote evidence inventory for ", nrow(manifest), " files")
