#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("usage: profile_linked_transport.R ROOT CRB", call. = FALSE)
}

root <- normalizePath(args[[1L]], mustWork = TRUE)
crb_path <- normalizePath(args[[2L]], mustWork = TRUE)
suppressPackageStartupMessages(devtools::load_all(root, quiet = TRUE))

env <- new.env(parent = globalenv())
for (path in c(
  "inst/viewer/utility_functions.R",
  "inst/viewer/clone_contract.R",
  "inst/viewer/core/viewer_pack.R",
  "inst/viewer/coordinated_views/bundle.R"
)) {
  sys.source(file.path(root, path), envir = env)
}

data_set <- readCerebro(crb_path)
pack <- env$viewerPackOpen(crb_path, data_set)
if (!is.null(pack)) {
  attr(data_set, "cerebro_viewer_pack") <- pack
}
env$available_crb_files <- list(
  selected = crb_path,
  files = crb_path,
  names = tools::file_path_sans_ext(basename(crb_path))
)

timed <- function(expr) {
  started <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(value = value, ms = (proc.time()[["elapsed"]] - started) * 1000)
}
packed_size <- function(value) length(env$cv_wire_pack_message(value))

primary <- timed(env$cv_build_bundle(data_set, primary_only = TRUE))
full <- timed(env$cv_build_bundle(data_set))
supplement <- env$cv_bundle_supplement(primary$value, full$value)
messages <- list(
  primary = env$cv_wire_pack_bundle(primary$value, include_cells = FALSE),
  supplement = env$cv_wire_pack_message(supplement),
  cells = env$cv_wire_pack_cells(full$value$dataset_id, full$value$cells)
)

rows <- data.frame(
  section = names(messages),
  bytes = vapply(messages, length, numeric(1)),
  stringsAsFactors = FALSE
)
components <- setdiff(
  names(supplement),
  c(
    "dataset_id",
    "dataset_fingerprint",
    "progressive_token"
  )
)
component_rows <- data.frame(
  section = paste0("supplement.", components),
  bytes = vapply(
    components,
    function(name) packed_size(supplement[name]),
    numeric(1)
  ),
  stringsAsFactors = FALSE
)
rows <- rbind(rows, component_rows)
if (is.list(supplement$clone)) {
  clone_names <- names(supplement$clone)
  rows <- rbind(
    rows,
    data.frame(
      section = paste0("supplement.clone.", clone_names),
      bytes = vapply(
        clone_names,
        function(name) packed_size(supplement$clone[name]),
        numeric(1)
      ),
      stringsAsFactors = FALSE
    )
  )
}
rows$mib <- rows$bytes / 1024^2

cat(sprintf("cells\t%d\n", full$value$n))
cat(sprintf("primary_build_ms\t%.0f\n", primary$ms))
cat(sprintf("full_build_ms\t%.0f\n", full$ms))
cat(sprintf(
  "first_open_total_bytes\t%.0f\n",
  sum(vapply(messages, length, numeric(1)))
))
write.table(rows, row.names = FALSE, sep = "\t", quote = FALSE)
