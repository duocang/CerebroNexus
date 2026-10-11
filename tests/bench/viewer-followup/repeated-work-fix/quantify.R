args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
definitions <- function(expressions, env) {
  for (x in expressions) if (is.call(x) && identical(x[[1]], as.name("<-")) &&
    is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function"))) eval(x, env)
}
old <- new.env(parent = globalenv())
candidate <- new.env(parent = globalenv())
definitions(parse(text = system2("git", c("show", "d40e785a:inst/viewer/immune_repertoire/data.R"), stdout = TRUE)), old)
definitions(parse("inst/viewer/immune_repertoire/data.R"), candidate)
raw <- readCerebro(args[[2]])$getImmuneRepertoire()
# Linkage-only changes reuse the compact result; initial preparation remains.
profiles <- candidate$ir_clone_size_profiles(raw)
profiles$clone_counts <- NULL
results <- list()
for (i in 1:3) {
  method <- c("ward.D2", "complete", "average")[[i]]
  start <- nanonext::mclock()
  b <- ggplot2::ggplot_build(old$ir_empirical_size_distribution_plot(raw, method = method))$data
  baseline_ms <- nanonext::mclock() - start
  start <- nanonext::mclock()
  c <- ggplot2::ggplot_build(candidate$ir_size_profile_plot(profiles, method))$data
  candidate_ms <- nanonext::mclock() - start
  stopifnot(isTRUE(all.equal(b, c)))
  results[[i]] <- data.frame(pair = i, method, baseline_ms, candidate_ms,
    equal = TRUE, retained_bytes = as.numeric(object.size(profiles)))
}
result <- do.call(rbind, results)
write.csv(result, "tests/bench/viewer-followup/repeated-work-fix/timings.csv", row.names = FALSE)
print(result)
cat("MEDIAN", median(result$baseline_ms), median(result$candidate_ms), "ms\n")
