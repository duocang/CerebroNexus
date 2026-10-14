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
supplement <- timed(env$cv_build_progressive_supplement(
  data_set,
  primary$value
))
primary_wire <- timed(env$cv_wire_pack_bundle(
  primary$value,
  include_cells = FALSE
))
supplement_wire <- timed(env$cv_wire_pack_message(supplement$value))
messages <- list(
  primary = primary_wire$value,
  supplement = supplement_wire$value
)
shared_primary <- primary$value
shared_name <- shared_primary$default_projection
shared_primary$shared_projection <- shared_name
shared_primary$projections[[shared_name]]$x <- NULL
shared_primary$projections[[shared_name]]$y <- NULL
shared_primary$projections[[shared_name]]$z <- NULL
shared_messages <- list(
  primary = env$cv_wire_pack_bundle(shared_primary, include_cells = FALSE),
  supplement = messages$supplement
)

rows <- data.frame(
  section = names(messages),
  bytes = vapply(messages, length, numeric(1)),
  stringsAsFactors = FALSE
)
components <- setdiff(
  names(supplement$value),
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
    function(name) packed_size(supplement$value[name]),
    numeric(1)
  ),
  stringsAsFactors = FALSE
)
rows <- rbind(rows, component_rows)
if (is.list(supplement$value$clone)) {
  clone_names <- names(supplement$value$clone)
  rows <- rbind(
    rows,
    data.frame(
      section = paste0("supplement.clone.", clone_names),
      bytes = vapply(
        clone_names,
        function(name) packed_size(supplement$value$clone[name]),
        numeric(1)
      ),
      stringsAsFactors = FALSE
    )
  )
}
rows$mib <- rows$bytes / 1024^2

cat(sprintf("cells\t%d\n", primary$value$n))
cat(sprintf("primary_build_ms\t%.0f\n", primary$ms))
cat(sprintf("supplement_build_ms\t%.0f\n", supplement$ms))
cat(sprintf("primary_serialize_ms\t%.0f\n", primary_wire$ms))
cat(sprintf("supplement_serialize_ms\t%.0f\n", supplement_wire$ms))
cat(sprintf(
  "first_open_total_bytes\t%.0f\n",
  sum(vapply(messages, length, numeric(1)))
))
cat(sprintf(
  "incremental_bytes\t%.0f\n",
  length(messages$supplement)
))
cat(sprintf(
  "shared_incremental_bytes\t%.0f\n",
  sum(vapply(shared_messages, length, numeric(1)))
))
write.table(rows, row.names = FALSE, sep = "\t", quote = FALSE)
