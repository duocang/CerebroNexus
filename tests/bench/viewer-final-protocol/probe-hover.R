args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("usage: Rscript probe-hover.R ROOT ONE_M_CRB")
root <- normalizePath(args[[1L]])
crb <- normalizePath(args[[2L]])
pkgload::load_all(root, quiet = TRUE)
object <- readCerebro(crb)
metadata <- object$getMetaData()
groups <- object$getGroups()
scope <- new.env(parent = globalenv())
scope$getGroups <- function() groups
expressions <- parse(file.path(root, "inst/viewer/utility_functions.R"))
definition <- Filter(function(x) is.call(x) && identical(x[[1L]], quote(`<-`)) &&
  identical(as.character(x[[2L]]), "buildHoverInfoForProjections"), expressions)
stopifnot(length(definition) == 1L)
eval(definition[[1L]], envir = scope)

format_unique <- function(values) {
  distinct <- unique(values)
  formatC(distinct, format = "f", big.mark = ",", digits = 0)[
    match(values, distinct)]
}
candidate <- function(table) {
  ids <- table[["cell_barcode"]]
  umi <- format_unique(table[["nUMI"]])
  genes <- format_unique(table[["nGene"]])
  out <- glue::glue("<b>Cell</b>: {ids}<br>",
    "<b>Transcripts</b>: {umi}<br>",
    "<b>Expressed genes</b>: {genes}")
  for (group in groups) {
    out <- glue::glue("{out}<br>", "<b>{group}</b>: {table[[group]]}")
  }
  out
}
cat("rows", nrow(metadata), "groups", paste(groups, collapse = ","),
    "nUMI distinct", length(unique(metadata$nUMI)),
    "nGene distinct", length(unique(metadata$nGene)), "\n")
start <- proc.time()[["elapsed"]]
before <- scope$buildHoverInfoForProjections(metadata)
before_s <- proc.time()[["elapsed"]] - start
start <- proc.time()[["elapsed"]]
after <- candidate(metadata)
after_s <- proc.time()[["elapsed"]] - start
cat("baseline", before_s, "candidate", after_s,
    "identical", identical(before, after),
    "bytes", as.numeric(object.size(after)), "\n")
if (!identical(before, after)) {
  mismatch <- which(as.character(before) != as.character(after))
  print(head(mismatch))
  stop("Hover values differ")
}
