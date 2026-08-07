#!/usr/bin/env Rscript

.bench_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("runner must be invoked with Rscript", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

bench_root <- dirname(.bench_script_path())
repo <- normalizePath(file.path(bench_root, "..", ".."), mustWork = TRUE)
source(file.path(bench_root, "config.R"), local = globalenv())
source(file.path(bench_root, "helpers.R"), local = globalenv())

main <- function() {
  parsed <- bench_parse_args(commandArgs(trailingOnly = TRUE), "full_scale")
  if (parsed$dry_run) {
    schedule <- bench_full_schedule(
      c(
        common = BENCH_CONFIG$common_target,
        BENCH_CONFIG$full_scale_fixed_tiers
      ),
      BENCH_CONFIG$full_scale_repeats
    )
    full_pairs <- sum(schedule$n_cells == BENCH_CONFIG$source$n_cells)
    cat(
      "Panel B dry-run: ",
      nrow(schedule),
      " technical pairs / ",
      2L * nrow(schedule),
      " workers; full ",
      BENCH_CONFIG$source$n_cells,
      " x ",
      full_pairs,
      "; UNQUALIFIED\n",
      sep = ""
    )
    return(invisible(0L))
  }
  command <- paste(
    "Rscript --vanilla",
    shQuote(.bench_script_path()),
    paste(shQuote(commandArgs(trailingOnly = TRUE)), collapse = " ")
  )
  bench_run_panel("full_scale", parsed, repo, bench_root, command)
  cat("Panel B VALID\n")
  invisible(0L)
}

tryCatch(main(), error = function(error) {
  message("ERROR: ", conditionMessage(error))
  quit(save = "no", status = 1L, runLast = FALSE)
})
