# Running tests

Run these commands from the repository root on Linux or WSL. Use the project environment so R and its dependencies match the checkout:

```bash
nix-shell default.nix -A shell

# Run the full regression suite; a failure returns a nonzero exit code.
Rscript -e 'devtools::test(reporter="summary", stop_on_failure=TRUE)'

# Run one group while working on it.
Rscript -e 'devtools::test(filter="million-cell-renderer", reporter="summary", stop_on_failure=TRUE)'

# Run the Shiny browser tests.
Rscript -e 'devtools::test(filter="app-inst", reporter="summary", stop_on_failure=TRUE)'
```

`devtools::test()` loads the current source, so you do not need to reinstall the package after each edit. Replace the filter with the part after `test-` in a test filename. For a package build and check, run `Rscript -e 'devtools::check()'`; this tests the built package rather than only the source checkout.

## Directory layout

```text
tests/
├── README.md
├── testthat.R              # R CMD check entry point
├── testthat/               # Regression tests and helpers
│   ├── fixtures/           # Small test inputs and JavaScript test scripts
│   ├── _snaps/             # Expected snapshots, tracked in Git
│   └── _artifacts/         # Generated browser diagnostics, ignored by Git
└── benchmarks/             # Performance experiments, separate from regression tests
    ├── expression-backends/
    ├── viewer/
    └── results/
```

See [benchmark commands](benchmarks/README.md) for H5 / BPCells / embedded comparisons and Viewer measurements. Benchmarks and generated browser diagnostics are excluded from R package builds. `_snaps/` contains expected test output and must not be treated as disposable logs.

## Dependencies

- R and package dependencies: use the project Nix environment; supported R versions are declared in `DESCRIPTION`.
- R test tools: `devtools`, `testthat`, `shinytest2` and `chromote`.
- Browser tests: Chrome or Chromium, discoverable by `chromote`.
- JavaScript tests: Node.js on `PATH`. Tests that require Node.js are skipped when it is unavailable.

```bash
node --version
Rscript -e 'chromote::find_chrome()'
```

The JavaScript tests use Node.js built-in modules (`assert`, `fs`, `path` and `vm`). **No `npm install` is needed.** The old `tests/node_modules/` held a local Acorn parser installation; no current tests referenced it, so it was removed. There is no npm project or npm dependency manifest under `tests/`.

If Chrome is not found, set its path before running the browser tests:

```bash
export CHROMOTE_CHROME=/path/to/chromium
Rscript -e 'devtools::test(filter="app-inst", reporter="summary", stop_on_failure=TRUE)'
```

## Failures and snapshots

Read the test output first. Browser diagnostics may appear in `tests/testthat/_artifacts/` or temporary directories printed in the failure report. Keep them while investigating; they are generated output and are not committed.

Review snapshot changes in R before accepting them:

```r
testthat::snapshot_review()
# Accept only after checking that the new output is correct.
testthat::snapshot_accept()
```

`tests/testthat/setup.R` sets `NOT_CRAN=true` and a longer browser startup timeout. Some tests also need external data or optional packages; a skip means that check did not run. POSIX file-permission tests need Linux/WSL. See the [authentication guide](../vignettes/control_access_to_cerebro_with_a_login_page.Rmd) for deployment details.

`tests/testthat.R` loads the installed package for `R CMD check`. Calling `testthat::test_dir()` alone does not load the development package; use `devtools::test()` for normal local work.
