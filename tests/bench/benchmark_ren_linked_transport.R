#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L || !args[[3L]] %in% c("cold", "projection")) {
  stop("usage: benchmark_ren_linked_transport.R ROOT CRB cold|projection")
}

root <- normalizePath(args[[1L]], mustWork = TRUE)
crb <- normalizePath(args[[2L]], mustWork = TRUE)
scenario <- args[[3L]]
Sys.setenv(NOT_CRAN = "true")
quote_r <- function(value) encodeString(value, quote = '"')

app_dir <- tempfile("ren-linked-transport-")
dir.create(app_dir)
on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
writeLines(
  c(
    sprintf("devtools::load_all(%s, quiet = TRUE)", quote_r(root)),
    "launchCerebro(",
    "  mode = \"closed\",",
    sprintf("  crb_file_to_load = c(\"Ren\" = %s),", quote_r(crb)),
    "  percentage_cells_to_show = 100,",
    "  projections_show_hover_info = TRUE",
    ")"
  ),
  file.path(app_dir, "app.R")
)

suppressWarnings(shinytest2::local_app_support(app_dir))
app <- shinytest2::AppDriver$new(
  app_dir,
  name = paste0("ren-linked-", scenario),
  height = 950,
  width = 1619,
  load_timeout = 900000,
  timeout = 900000,
  check_names = FALSE
)
on.exit(app$stop(), add = TRUE)
app$wait_for_value(output = "load_data_number_of_cells", timeout = 900000)

click_tab <- function(tab) {
  app$run_js(sprintf(
    "document.querySelector('a[href=\"#shiny-tab-%s\"]').click();",
    tab
  ))
}
wait_projection <- function() {
  app$wait_for_js(
    paste0(
      "(() => {const h=document.getElementById('overview_projection_cell_view_host');",
      "const g=h?.querySelector('canvas.cv-gpu-layer');",
      "return g?.style.display==='block'&&Number(g.dataset.pointCount)===1462702&&",
      "g._cerebroPointRenderer?.isReady();})()"
    ),
    timeout = 180000
  )
}
wait_linked <- function(primary = FALSE) {
  app$wait_for_js(
    sprintf(
      "window.cerebroLinkedViewsState?.%s?.()===true",
      if (primary) "primaryReady" else "ready"
    ),
    timeout = 180000
  )
}
heap <- function() {
  app$get_js(
    "performance.memory ? performance.memory.usedJSHeapSize : null"
  )
}

if (identical(scenario, "projection")) {
  click_tab("overview")
  wait_projection()
  app$wait_for_js(
    "Object.keys(window.CerebroSharedDatasetState?.projections||{}).length>0",
    timeout = 30000
  )
}
heap_before <- heap()
started <- proc.time()[["elapsed"]]
click_tab("coordinated_views")
wait_linked(primary = TRUE)
primary_ms <- (proc.time()[["elapsed"]] - started) * 1000
wait_linked()
complete_ms <- (proc.time()[["elapsed"]] - started) * 1000
app$get_js(paste0(
  "Promise.all(Array.from(document.querySelectorAll(",
  "'#shiny-tab-coordinated_views canvas.cv-gpu-layer'))",
  ".map(c=>c._cerebroPointRenderer?.idle?.()||Promise.resolve())).then(()=>true)"
))
summary <- app$get_js("window.cerebroLinkedViewsState.summary()")

idle_started <- proc.time()[["elapsed"]]
idle_ok <- tryCatch(
  {
    app$wait_for_idle(timeout = 60000)
    TRUE
  },
  error = function(error) FALSE
)
idle_ms <- (proc.time()[["elapsed"]] - idle_started) * 1000

back_started <- proc.time()[["elapsed"]]
click_tab("overview")
wait_projection()
linked_to_projection_ms <- (proc.time()[["elapsed"]] - back_started) * 1000
repeat_started <- proc.time()[["elapsed"]]
click_tab("coordinated_views")
wait_linked()
repeat_linked_ms <- (proc.time()[["elapsed"]] - repeat_started) * 1000
heap_after <- heap()

result <- list(
  scenario = scenario,
  primary_ms = primary_ms,
  complete_ms = complete_ms,
  auxiliary_idle_ms = idle_ms,
  auxiliary_idle_reached = idle_ok,
  linked_to_projection_ms = linked_to_projection_ms,
  repeat_linked_ms = repeat_linked_ms,
  heap_before = heap_before,
  heap_after = heap_after,
  transport = summary$transport
)
cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n")
