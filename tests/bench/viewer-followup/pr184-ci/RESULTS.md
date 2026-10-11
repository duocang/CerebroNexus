# PR184 CI follow-up

Date: 2026-10-11. Failure baseline: `551645e5e5a0f7a6b4a35e1f71ed7f24e3125b58`.
Existing checkout and branch: `perf/pr6-ui-accessibility-polish`.

## Remote failures

- R tests: https://github.com/mihem/CerebroNexus/actions/runs/38129309237
  (8 failed, 14,242 passed, 3 skipped, no warnings).
- R CMD check: https://github.com/mihem/CerebroNexus/actions/runs/38129309251
  (15 failed tests, 6 test warnings; documentation and dependency warnings).
- pkgdown passed. The previous local targeted suite was insufficient: it did
  not cover all affected tests or the installed-package layout.

## Corrections

- Layout assertions now account for the existing page-loading wrapper.
- The benchmark contract tests the existing page-local completion gate;
  global Shiny busy state is not reintroduced as page readiness.
- Gene and Spatial render test doubles accept the existing feedback argument.
- Portable-view context assertions inspect the two configuration observers
  and their response helper. Synchronous expression feedback is a separate
  operation; the test no longer incorrectly forbids its live-context capture.
- Resource-boundary tests resolve Viewer files through the existing
  source/installed-package path helper. Three Node fixtures receive resolved
  asset paths from R, including the Groups UI used by the loader test.
- Tests use the declared dplyr re-export of `%>%`, removing their undeclared
  direct magrittr dependency.
- Regenerated the HLA aggregation documentation for the existing `.keys`
  argument. Unrelated generator-version changes were discarded.

No production R/JS/CSS changes, performance experiments, or weakened ready,
identity, precision, deferred-metadata or generation semantics.

## Validation

`run-tests.R` runs the complete test directory and a separate built-package
check, retaining `full.csv`, `full.log`, and check output. Local environment:
Windows checkout, WSL R 4.6.1 and Linux Chrome; this is not Windows Brave or
the GitHub Nix environment.

Full source suite (`full.csv`, `full.log`): **14,266 assertions passed, zero
failures/errors/warnings, two conditional skips**. The skips are the test for
an absent HDF5Array installation (installed here) and a Mouse trajectory
artifact not supplied to this run. All previously failing source-suite tests
pass. No test was newly skipped to resolve these failures.

Built-package `R CMD check --as-cran` (`check.log`, `installed-check.log`,
`installed-tests.log`, `check-result.rds`): **zero errors and zero warnings**.
Installed tests: **13,696 passed, zero failures/warnings, 74 conditional skips**.
Examples, vignettes and vignette rebuilding pass. A Chrome startup timeout was
printed during testing but did not cause a final failure. Four notes remain:
`.dockerignore`, the top-level `deploy` directory, the existing global
`Cerebro.options` assignment, and Chrome temporary directories. These match
the categories reported before this CI correction; they are not suppressed.

These are local verification results. GitHub Nix validation is a separate run
on the commit containing this report; its checks must be inspected after push.
