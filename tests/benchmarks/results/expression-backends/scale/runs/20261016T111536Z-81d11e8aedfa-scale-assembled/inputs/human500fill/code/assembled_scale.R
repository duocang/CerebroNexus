# Validation of an explicitly assembled evidence collection, not a new run.
# Original acquisition identities, environments, schedules and skips are retained.
bench_validate_assembled_scale <- function(stage) {
  read <- function(name, dir = stage) read.csv(file.path(dir, name), stringsAsFactors = FALSE)
  inventory <- read("assembly_inventory.csv")
  stopifnot(nrow(inventory) > 0L, !anyDuplicated(inventory$path),
            !any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", inventory$path)))
  paths <- file.path(stage, inventory$path)
  stopifnot(all(file.info(paths)$size == inventory$bytes),
            all(vapply(paths, function(p) digest::digest(file=p, algo="sha256"), character(1)) == inventory$sha256))
  manifest <- bench_manifest_values(read("run_manifest.csv"))
  stopifnot(manifest[["result_kind"]] == "assembled", manifest[["profile"]] == "scale",
            manifest[["new_measurements"]] == "0")
  sources <- read("acquisitions.csv")
  ex <- read("10_export.csv"); ac <- read("20_access.csv"); ti <- read("21_access_timings.csv")
  skips <- read("resource_skips.csv")
  same_rows <- function(actual, expected, columns = names(expected)) {
    stopifnot(nrow(actual) == nrow(expected), all(columns %in% names(actual)))
    # CSV union adds columns, never changes a source value or its row order.
    for (column in columns) {
      left <- actual[[column]]; right <- expected[[column]]
      if (is.numeric(right)) {
        stopifnot(isTRUE(all.equal(as.numeric(left), as.numeric(right), tolerance=1e-12)))
      } else stopifnot(identical(as.character(left), as.character(right)))
    }
  }
  for (i in seq_len(nrow(sources))) {
    origin <- file.path(stage, sources$path[i]); id <- sources$source_run_id[i]
    e <- read("10_export.csv", origin); a <- read("20_access.csv", origin)
    t <- read("21_access_timings.csv", origin)
    stopifnot(all(e$run_id == id), all(a$run_id == id), all(t$run_id == id))
    bench_validate_timing_evidence(e, a, t)
    if (file.exists(file.path(origin, "05_schedule.csv"))) {
      m <- bench_manifest_values(read("run_manifest.csv", origin))
      stopifnot(m[["git_sha"]] == manifest[["measurement_git_sha"]], m[["git_dirty"]] == "false")
      bench_validate_results(read("05_schedule.csv", origin), e, a, read("crashes.csv", origin), bench_profile(m[["profile"]]))
      if (file.exists(file.path(origin, "evidence_manifest.csv"))) {
        inv <- read("evidence_manifest.csv", origin)
        p <- file.path(origin, inv$path)
        stopifnot(all(file.info(p)$size == inv$bytes), all(unname(tools::md5sum(p)) == inv$md5))
      }
    } else {
      v <- jsonlite::fromJSON(file.path(origin, "validation.json"))
      stopifnot(v$status == "PASS", v$run_id == id, v$build_repeats == 1L,
        nrow(e) == 1L, nrow(a) == 2L, e$nnz == 1982146320,
        all(a$query_plan_fingerprint == "47fca3dda5ed5d1c8b7ddeb57c94d078"))
    }
    same_rows(ex[ex$source_run_id == id, , drop=FALSE], e[e$status == "OK", , drop=FALSE])
    same_rows(ac[ac$source_run_id == id, , drop=FALSE], a)
    same_rows(ti[ti$source_run_id == id, , drop=FALSE], t)
    same_rows(skips[skips$source_run_id == id, , drop=FALSE], e[e$status != "OK", , drop=FALSE])
  }
  stopifnot(setequal(ex$source_run_id, sources$source_run_id),
    all(ex$run_id == ex$source_run_id), all(ac$run_id == ac$source_run_id),
    all(ti$run_id == ti$source_run_id),
    all(ex$collection_id == manifest[["run_id"]]), all(ac$collection_id == manifest[["run_id"]]),
    nrow(ex) == 216L, nrow(ac) == 432L, nrow(ti) == 16416L, nrow(skips) == 5L)
  bench_validate_timing_evidence(ex, ac, ti)
  bench_validate_results(read("05_schedule.csv"), ex, ac, profile=bench_profile("scale"))
  # Full and supplemental observations must cover the new grid, while n=1 is
  # explicitly distinct from the prospective five-repeat measurement protocol.
  expected <- unique(bench_scale_schedule(BENCH_SOURCES)[c("source", "n_cells", "backend")])
  key <- function(d) paste(d$source, sprintf("%.0f", d$n_cells), d$backend, sep="|")
  stopifnot(setequal(key(expected), key(ex)))
  counts <- table(key(ex))
  stopifnot(all(counts[names(counts) != "human_pfc_hbcc|500000|embedded"] == 5L),
            counts[["human_pfc_hbcc|500000|embedded"]] == 1L)
  for (src in unique(ac$source)) for (n in unique(ac$n_cells[ac$source == src])) {
    rows <- ac[ac$source == src & ac$n_cells == n, ]
    for (field in c("query_plan_fingerprint", "row_fingerprint", "block_fingerprint", "subset_row_fingerprint", "subset_block_fingerprint"))
      stopifnot(length(unique(rows[[field]])) == 1L)
  }
  message("PASS: assembled Scale; 216 real builds, 432 reads, 16416 monotonic observations; 5 historical skips retained; no new measurement")
  invisible(TRUE)
}
