## Build an HLA analysis archive in a daemon and return its bytes.

hla_build_export_archive <- function(tables) {
  staging <- tempfile("hla_export_")
  archive <- tempfile(fileext = ".zip")
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(c(staging, archive), recursive = TRUE), add = TRUE)
  for (name in names(tables)) {
    utils::write.csv(
      tables[[name]],
      file.path(staging, paste0(name, ".csv")),
      row.names = FALSE
    )
  }
  utils::zip(
    zipfile = archive,
    files = list.files(staging, full.names = TRUE),
    flags = "-j -q"
  )
  size <- file.info(archive)$size
  readBin(archive, what = "raw", n = size)
}
