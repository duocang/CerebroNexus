# system2(env=) is not portable for Rscript on Windows: assignments become
# script arguments. Keep environment changes scoped to the subprocess call.
bench_test_system2 <- function(command, args = character(), ..., env = character()) {
  assignments <- stats::setNames(
    sub("^[^=]*=", "", env), sub("=.*$", "", env)
  )
  args <- gsub("\\", "/", args, fixed = TRUE)
  resolved <- Sys.which(command)
  if (nzchar(resolved)) command <- unname(resolved)
  if (!length(assignments)) return(system2(command, shQuote(args), ...))
  withr::with_envvar(assignments, system2(command, shQuote(args), ...))
}
