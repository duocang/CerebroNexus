#!/usr/bin/env Rscript

root <- normalizePath(if (file.exists("DESCRIPTION")) "." else "..")
devtools::load_all(root, quiet = TRUE)
source(file.path(root, "inst/viewer/async_runtime.R"))
source(file.path(root, "inst/viewer/clone_contract.R"))
source(file.path(root, "inst/viewer/hla_tcr_motifs/core/hla_typing.R"))
source(file.path(root, "inst/viewer/hla_tcr_motifs/core/hla_motif_core.R"))
source(file.path(root, "inst/viewer/spatial/func_spatial_helpers.R"))
source(file.path(root, "inst/viewer/coordinated_views/bundle.R"))

measure_pair <- function(name, sync, worker, args, times = 3L) {
  sync_elapsed <- sync_lag <- async_submit <- async_lag <- async_total <- numeric(
    times
  )
  sync_value <- async_value <- NULL
  for (i in seq_len(times)) {
    lag <- NA_real_
    started <- proc.time()[["elapsed"]]
    later::later(
      function() lag <<- proc.time()[["elapsed"]] - started,
      delay = 0
    )
    sync_value <- sync()
    sync_elapsed[[i]] <- proc.time()[["elapsed"]] - started
    while (is.na(lag)) {
      later::run_now(0.1)
    }
    sync_lag[[i]] <- lag

    lag <- NA_real_
    started <- proc.time()[["elapsed"]]
    later::later(
      function() lag <<- proc.time()[["elapsed"]] - started,
      delay = 0
    )
    task <- cerebro_async_submit(worker, args)
    async_submit[[i]] <- proc.time()[["elapsed"]] - started
    while (is.na(lag)) {
      later::run_now(0.1)
    }
    async_lag[[i]] <- lag
    async_value <- task[]
    async_total[[i]] <- proc.time()[["elapsed"]] - started
  }
  list(
    values = list(sync = sync_value, async = async_value),
    result = data.frame(
      workload = name,
      mode = c("master synchronous", "mirai asynchronous"),
      submit_ms = 1000 * c(median(sync_elapsed), median(async_submit)),
      event_loop_lag_ms = 1000 * c(median(sync_lag), median(async_lag)),
      total_ms = 1000 * c(median(sync_elapsed), median(async_total)),
      payload_mb = c(0, as.numeric(object.size(args)) / 1024^2)
    )
  )
}

profile <- paste0("cerebro-benchmark-", Sys.getpid())
invisible(cerebro_async_init(
  cerebro_async_config(list(workers = 1L, compute = profile))
))
on.exit(cerebro_async_shutdown(), add = TRUE)
invisible(cerebro_async_submit(function() Sys.getpid())[])

demo_path <- file.path(root, "inst/extdata/examples/demo_hla_tcr_dextramer.crb")
demo <- readRDS(demo_path)
segments <- hla_parse_ir_segments(demo$getImmuneRepertoire(), "TRB")
hla_args <- list(
  root = file.path(root, "inst/viewer/hla_tcr_motifs/core"),
  files = c("hla_typing.R", "hla_motif_core.R"),
  function_name = "hla_build_motif_graph_raw",
  args = list(
    seg = segments,
    by_v = FALSE,
    meta_cols = character(),
    context_col = "mhc_context"
  ),
  function_args = c(context_summary = "hla_context_summary")
)
hla <- measure_pair(
  "HLA motif graph (12,000 receptors)",
  function() {
    hla_build_motif_graph_raw(
      segments,
      context_col = "mhc_context"
    )
  },
  cerebro_async_source_call,
  hla_args
)
stopifnot(
  igraph::vcount(hla$values$sync) == igraph::vcount(hla$values$async),
  igraph::ecount(hla$values$sync) == igraph::ecount(hla$values$async)
)

set.seed(20260907)
n <- 1200L
spatial_args <- list(
  root = file.path(root, "inst/viewer/spatial"),
  files = "func_spatial_helpers.R",
  function_name = "morans_i",
  args = list(
    x = runif(n),
    y = runif(n),
    values = rnorm(n),
    k = 6L
  )
)
moran <- measure_pair(
  "Moran's I (1,200 cells)",
  function() do.call(morans_i, spatial_args$args),
  cerebro_async_source_call,
  spatial_args
)
stopifnot(isTRUE(all.equal(moran$values$sync, moran$values$async)))

crb_cold <- median(replicate(3L, system.time(readRDS(demo_path))[["elapsed"]]))
cache <- new.env(parent = emptyenv())
cache[[demo_path]] <- demo
crb_cached <- system.time(
  for (i in seq_len(10000L)) {
    cache[[demo_path]]
  }
)[["elapsed"]] /
  10000

bundle_cold <- median(replicate(
  3L,
  system.time(cv_build_bundle(demo))[["elapsed"]]
))
bundle <- cv_build_bundle(demo)
cache[["bundle"]] <- bundle
bundle_cached <- system.time(
  for (i in seq_len(10000L)) {
    cache[["bundle"]]
  }
)[["elapsed"]] /
  10000

cache_results <- data.frame(
  workload = rep(
    c("CRB load, later session", "Linked Views bundle, later session"),
    each = 2L
  ),
  mode = rep(c("master per-session", "process cache hit"), times = 2L),
  submit_ms = 1000 * c(crb_cold, crb_cached, bundle_cold, bundle_cached),
  event_loop_lag_ms = 1000 *
    c(crb_cold, crb_cached, bundle_cold, bundle_cached),
  total_ms = 1000 * c(crb_cold, crb_cached, bundle_cold, bundle_cached),
  payload_mb = 0
)

results <- rbind(hla$result, moran$result, cache_results)
results[] <- lapply(results, function(column) {
  if (is.numeric(column)) round(column, 3) else column
})
print(results, row.names = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  utils::write.csv(results, args[[1L]], row.names = FALSE)
}
