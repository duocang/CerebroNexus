# Compatibility entry point for the interactive one-million-cell demo.
# The canonical implementation remains shared with the CRB benchmark harness.
.viewer1mSearchRoots <- unique(c(
  getwd(),
  dirname(getwd()),
  dirname(dirname(getwd())),
  dirname(dirname(dirname(getwd())))
))
.viewer1mFixtureCandidates <- unique(c(
  file.path(
    .viewer1mSearchRoots,
    "tests",
    "bench",
    "harnesses",
    "crb",
    "fixture.R"
  ),
  file.path(.viewer1mSearchRoots, "harnesses", "crb", "fixture.R")
))
.viewer1mFixture <- .viewer1mFixtureCandidates[
  file.exists(.viewer1mFixtureCandidates)
][1L]
if (is.na(.viewer1mFixture)) {
  stop(
    "Could not locate tests/bench/harnesses/crb/fixture.R.",
    call. = FALSE
  )
}
sys.source(.viewer1mFixture, envir = environment())
rm(.viewer1mSearchRoots, .viewer1mFixtureCandidates, .viewer1mFixture)
