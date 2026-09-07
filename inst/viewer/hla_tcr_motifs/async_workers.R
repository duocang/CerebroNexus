## Build an HLA analysis archive in a daemon and return its bytes.

hla_build_export_archive <- function(tables) {
  staging <- tempfile("hla_export_")
  archive <- tempfile(fileext = ".zip")
  zip_command <- Sys.getenv("R_ZIPCMD", unset = "")
  if (!nzchar(zip_command)) {
    zip_command <- Sys.which("zip")
  }
  if (!nzchar(zip_command)) {
    stop("The zip utility is required to export HLA analysis archives.")
  }
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
    flags = "-j -q",
    zip = zip_command
  )
  size <- file.info(archive)$size
  readBin(archive, what = "raw", n = size)
}
