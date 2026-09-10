#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true")
if (length(args) < 3L) {
  stop(
    "usage: pr1_large_browser.R BEFORE_ROOT AFTER_ROOT CRB [REPEATS] [PERCENT]",
    call. = FALSE
  )
}

roots <- c(
  before = normalizePath(args[[1L]], mustWork = TRUE),
  after = normalizePath(args[[2L]], mustWork = TRUE)
)
crb_path <- normalizePath(args[[3L]], mustWork = TRUE)
repeats <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
percentage <- if (length(args) >= 5L) as.numeric(args[[5L]]) else 10
if (is.na(repeats) || repeats < 1L) {
  stop("REPEATS must be a positive integer.", call. = FALSE)
}
if (is.na(percentage) || percentage < 10 || percentage > 100) {
  stop("PERCENT must be between 10 and 100.", call. = FALSE)
}

quote_r <- function(value) encodeString(value, quote = '"')

run_once <- function(version, root, round) {
  app_dir <- tempfile(paste0("pr1-large-browser-", version, "-"))
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(
      sprintf("devtools::load_all(%s, quiet = TRUE)", quote_r(root)),
      "launchCerebro(",
      "  mode = \"closed\",",
      sprintf(
        "  crb_file_to_load = c(\"1M mouse brain\" = %s),",
        quote_r(crb_path)
      ),
      sprintf("  percentage_cells_to_show = %s,", percentage),
      "  projections_show_hover_info = TRUE",
      ")"
    ),
    file.path(app_dir, "app.R")
  )

  suppressWarnings(shinytest2::local_app_support(app_dir))
  started <- proc.time()[["elapsed"]]
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = paste0("pr1_large_", version, "_", round),
    height = 950,
    width = 1619,
    load_timeout = 900000,
    timeout = 900000
  )
  on.exit(app$stop(), add = TRUE)
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-loadData\"]').click();"
  )
  app$wait_for_value(
    output = "load_data_number_of_cells",
    timeout = 900000
  )
  cell_count <- app$get_value(output = "load_data_number_of_cells")
  if (
    is.null(cell_count$html) ||
      !grepl("1,000,000", cell_count$html, fixed = TRUE)
  ) {
    stop("Data Info did not report 1,000,000 cells.", call. = FALSE)
  }
  data_ready <- proc.time()[["elapsed"]]

  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-overview\"]').click();"
  )
  app$wait_for_js(
    paste0(
      "(() => {",
      "const h=document.getElementById('overview_projection_cell_view_host');",
      "const c=h?.querySelector('canvas:not(.cv-mini)');",
      "return c && c.width > 0 && c.height > 0;",
      "})()"
    ),
    timeout = 900000
  )
  canvas_ready <- proc.time()[["elapsed"]]
  canvas <- app$get_js(
    paste0(
      "(() => {",
      "const c=document.querySelector(",
      "'#overview_projection_cell_view_host canvas:not(.cv-mini)');",
      "const d=c.getContext('2d').getImageData(0,0,c.width,c.height).data;",
      "let ink=0;",
      "for(let i=0;i<d.length;i+=16){",
      "if(d[i+3] && (d[i]<248 || d[i+1]<248 || d[i+2]<248)) ink++;",
      "}",
      "return {width:c.width,height:c.height,ink:ink};",
      "})()"
    )
  )
  if (as.numeric(canvas$ink) < 1000) {
    stop(
      "Overview Canvas did not contain enough painted pixels.",
      call. = FALSE
    )
  }

  process <- app$.__enclos_env__$private$shiny_process
  pid <- process$get_pid()
  rss_mib <- ps::ps_memory_info(ps::ps_handle(pid))[["rss"]] / 1024^2
  logs <- app$get_logs()
  messages <- if (is.null(logs$message)) character() else logs$message
  errors <- messages[grepl(
    "(^|[[:space:]])(error|fatal|unhandled)(:|[[:space:]])",
    messages,
    ignore.case = TRUE
  )]
  errors <- errors[
    !grepl(
      "fixed layout requires the slimscroll plugin",
      errors,
      fixed = TRUE
    )
  ]
  if (length(errors)) {
    stop("browser run logged an error: ", errors[[1L]], call. = FALSE)
  }

  data.frame(
    version = version,
    round = round,
    percentage = percentage,
    data_ready_ms = (data_ready - started) * 1000,
    overview_ms = (canvas_ready - data_ready) * 1000,
    total_ms = (canvas_ready - started) * 1000,
    shiny_rss_mib = rss_mib,
    canvas_width = as.numeric(canvas$width),
    canvas_height = as.numeric(canvas$height),
    ink_samples = as.numeric(canvas$ink),
    check = "1M loaded; canvas painted",
    check.names = FALSE
  )
}

rows <- list()
for (round in seq_len(repeats)) {
  order <- if (round %% 2L) c("before", "after") else c("after", "before")
  for (version in order) {
    message("browser round ", round, ": ", version)
    rows[[length(rows) + 1L]] <- run_once(version, roots[[version]], round)
  }
}

raw <- do.call(rbind, rows)
summary <- aggregate(
  raw[c("data_ready_ms", "overview_ms", "total_ms", "shiny_rss_mib")],
  list(version = raw$version),
  median
)
summary$time_change_pct <- NA_real_
summary$rss_change_pct <- NA_real_
before_row <- summary$version == "before"
after_row <- summary$version == "after"
summary$time_change_pct[after_row] <-
  (summary$total_ms[after_row] / summary$total_ms[before_row] - 1) * 100
summary$rss_change_pct[after_row] <-
  (summary$shiny_rss_mib[after_row] / summary$shiny_rss_mib[before_row] - 1) *
  100

cat("RAW\n")
write.table(raw, row.names = FALSE, sep = "\t", quote = FALSE)
cat("SUMMARY\n")
write.table(summary, row.names = FALSE, sep = "\t", quote = FALSE)
