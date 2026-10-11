args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true")
if (length(args) != 4L) {
  stop("usage: Rscript check-lifecycle.R ROOT CRB_A CRB_B OUTPUT_TSV")
}
root <- normalizePath(args[[1L]])
paths <- normalizePath(args[2:3])
output <- normalizePath(args[[4L]], mustWork = FALSE)
pkgload::load_all(root, quiet = TRUE)
counts <- vapply(paths, function(path) nrow(readCerebro(path)$getMetaData()), integer(1))
names(counts) <- c("A", "B")

app_dir <- tempfile("viewer-lifecycle-")
dir.create(app_dir)
on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
quote_r <- function(value) encodeString(value, quote = '"')
writeLines(c(
  sprintf("devtools::load_all(%s, quiet=TRUE)", quote_r(root)),
  sprintf("launchCerebro(mode='closed', crb_file_to_load=c(A=%s,B=%s))",
    quote_r(paths[[1L]]), quote_r(paths[[2L]]))
), file.path(app_dir, "app.R"))
shinytest2::local_app_support(app_dir)
app <- shinytest2::AppDriver$new(app_dir, name = "viewer_lifecycle_v2",
  height = 950, width = 1619)
on.exit(try(app$stop(), silent = TRUE), add = TRUE)
app$wait_for_js("!!document.getElementById('crb_file_selector')?.selectize && !!window.CerebroDatasetContext",
  timeout = 120000)
app$run_js(paste0(
  "window.__lifecycleAck=null;",
  "Shiny.addCustomMessageHandler('cerebro_benchmark_dataset_ready',",
  "function(x){window.__lifecycleAck=x;});"
))

rss_mib <- function(pid) {
  handle <- tryCatch(ps::ps_handle(pid), error = function(error) NULL)
  if (is.null(handle)) return(NA_real_)
  children <- tryCatch(ps::ps_children(handle, recursive = TRUE),
    error = function(error) list())
  sum(vapply(c(list(handle), children), function(x) {
    tryCatch(ps::ps_memory_info(x)[["rss"]],
      error = function(error) 0)
  }, numeric(1))) / 2^20
}
r_pid <- app$.__enclos_env__$private$shiny_process$get_pid()
browser <- app$get_chromote_session()$parent
on.exit(try(browser$close(), silent = TRUE), add = TRUE)
browser_pid <- browser$get_browser()$.__enclos_env__$private$process$get_pid()

rows <- list()
load_selected <- function(label, step) {
  token <- paste0("lifecycle-", step, "-", label)
  app$run_js(sprintf(paste0(
    "window.__lifecycleAck=null;",
    "Shiny.setInputValue('cerebro_benchmark_dataset_load_request',%s,",
    "{priority:'event'});"
  ), quote_r(token)))
  app$wait_for_js(sprintf(paste0(
    "window.__lifecycleAck?.request===%s&&",
    "window.__lifecycleAck?.cell_count===%s&&",
    "window.CerebroDatasetContext.phase()==='ready'"
  ), quote_r(token), counts[[label]]), timeout = 120000)
  state <- app$get_js(paste0(
    "(() => {const C=window.CerebroDatasetContext;",
    "return {key:C.current()?.dataset_key,",
    "generation:C.current()?.generation,",
    "phase:C.phase(),selected:document.getElementById('crb_file_selector').value,",
    "fingerprint:window.__lifecycleAck?.dataset_fingerprint};})()"
  ))
  stopifnot(identical(state$key, label), identical(state$selected, label),
    identical(state$phase, "ready"),
    grepl("^md5-cell-set-v1:", state$fingerprint))
  rows[[length(rows) + 1L]] <<- data.frame(
    step = step, selection = label, generation = state$generation,
    cell_count = counts[[label]], fingerprint = state$fingerprint,
    r_after_mib = rss_mib(r_pid), browser_after_mib = rss_mib(browser_pid)
  )
  invisible(state)
}

initial <- load_selected("A", 0L)
for (step in seq_len(6L)) {
  label <- if (step %% 2L) "B" else "A"
  app$run_js(sprintf(
    "document.getElementById('crb_file_selector').selectize.setValue(%s);",
    quote_r(label)))
  state <- load_selected(label, step)
  stopifnot(state$generation > initial$generation)
  initial <- state
}
app$run_js("window.CerebroDatasetContext.beginConnection();")
app$wait_for_js("window.CerebroDatasetContext.phase()==='ready'",
  timeout = 120000)
reconnected <- app$get_js("window.CerebroDatasetContext.current().generation")
stopifnot(identical(reconnected, initial$generation))
out <- do.call(rbind, rows)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.table(out, output, sep = "\t", row.names = FALSE, quote = FALSE)
print(out)
cat("simulated reconnect sync: PASS\n")
