args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
out <- file.path(root, "tests/bench/viewer-followup/repeated-work-audit")
pkgload::load_all(root, quiet = TRUE)
env <- new.env(parent = globalenv())
for (x in parse(file.path(root, "inst/viewer/immune_repertoire/data.R"))) {
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
      identical(x[[3]][[1]], as.name("function"))) eval(x, env)
}
raw <- readCerebro(args[[2]])$getImmuneRepertoire()
rows <- list()
for (i in 1:3) {
  start <- nanonext::mclock()
  profiles <- env$ir_clone_size_profiles(raw, "both", NULL, 1)
  prepare_ms <- nanonext::mclock() - start
  start <- nanonext::mclock()
  tree <- stats::hclust(stats::dist(profiles$profile), method = c("ward.D2", "complete", "average")[[i]])
  cluster_ms <- nanonext::mclock() - start
  rows[[i]] <- data.frame(iteration = i, preparation_ms = prepare_ms, cluster_ms = cluster_ms,
    profile_rows = nrow(profiles$profile), profile_columns = ncol(profiles$profile),
    profile_bytes = as.numeric(object.size(profiles$profile)), full_result_bytes = as.numeric(object.size(profiles)))
}
write.csv(do.call(rbind, rows), file.path(out, "size-timings.csv"), row.names = FALSE)
print(do.call(rbind, rows))
