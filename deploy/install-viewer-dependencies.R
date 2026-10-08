options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
description <- read.dcf("/tmp/cerebro-build/DESCRIPTION")
imports <- trimws(gsub("[[:space:]]*\\([^)]*\\)", "", strsplit(description[1, "Imports"], ",")[[1]]))
required <- unique(c(imports, "BPCells", "shinymanager", "HDF5Array", "rhdf5"))
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
declarations <- trimws(strsplit(paste(description[1, c("Imports", "Suggests")], collapse = ","), ",")[[1]])
minimum <- declarations[grepl("\\(>=", declarations)]
minimum_names <- trimws(sub("[[:space:]]*\\(.*", "", minimum))
minimum_versions <- sub(".*\\(>=[[:space:]]*([^)]*)\\).*", "\\1", minimum)
minimum_versions <- setNames(trimws(minimum_versions), minimum_names)
minimum_versions <- minimum_versions[names(minimum_versions) %in% required]
too_old <- names(minimum_versions)[vapply(names(minimum_versions), function(p) {
  !p %in% missing && utils::packageVersion(p) < package_version(minimum_versions[[p]])
}, logical(1))]
missing <- union(missing, too_old)
bioc <- setdiff(missing, "BPCells")
if (length(bioc)) BiocManager::install(bioc, ask = FALSE, update = FALSE, Ncpus = 2)
if ("BPCells" %in% missing) {
  remotes::install_github("bnprks/BPCells/r", upgrade = "never")
}
# Validate in a fresh process: dependency installation can replace namespaces
# that the initial availability checks have already loaded in this process.
check <- tempfile(fileext = ".R")
writeLines(c(
  paste0("required <- ", paste(deparse(required), collapse = "\n")),
  paste0("minimum <- ", paste(deparse(minimum_versions), collapse = "\n")),
  "for (p in required) { message('Checking ', p); loadNamespace(p) }",
  "for (p in names(minimum)) stopifnot(utils::packageVersion(p) >= package_version(minimum[[p]]))",
  'print(vapply(c("qs2", "BPCells", "shinymanager"), function(p) as.character(packageVersion(p)), character(1)))'
), check)
status <- system2(file.path(R.home("bin"), "Rscript"), check)
unlink(check)
if (status != 0L) stop("Runtime dependency validation failed in a fresh R process")
