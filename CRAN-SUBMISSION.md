# CRAN submission status

This branch contains the first implemented CRAN-compatibility pass for
CerebroNexus 3.2.0. It is not a submission record and must not be submitted
until the remaining cross-platform checks and maintainer review are complete.

## Current result

- Source package: **4,217,216 bytes** (`CerebroNexus_3.2.0.tar.gz`).
- Local check: `R CMD check --as-cran --no-manual --no-vignettes`.
- Result: **0 errors, 0 warnings, 1 note**.
- The CRAN-mode test suite completed successfully in 64 seconds.
- The remaining note reports a new submission and the optional `BPCells`
  dependency. The check confirms that `BPCells` is available from the declared
  `Additional_repositories` URL.

## Implemented

### Package size

The CRAN package keeps one compact runnable data set,
`inst/extdata/v1.4/example.crb`. Eight large domain demos, the standalone H5
example, and demo-only images are no longer shipped in the source package.
Together these removals reduced the tarball from 33.2 MB to 4.2 MB.

The source repository still contains the demo-generation scripts and tutorial
sources. Vignettes remain available through the documentation site and source
repository, but are excluded from the CRAN tarball. The large README feature
image is loaded from the repository rather than duplicated in the tarball.

The default bundled app now opens the compact PBMC example. Data-dependent
tests skip cleanly when an external domain demo is not installed.

### Dependencies and metadata

- Replaced `Remotes: bnprks/BPCells/r` with
  `Additional_repositories: https://bnprks.r-universe.dev`.
- Kept `BPCells` in `Suggests` and retained guarded runtime calls.
- Moved eight conditionally used packages from `Imports` to `Suggests`:
  `base64enc`, `biomaRt`, `future.apply`, `GSVA`, `httr`, `pbapply`, `qvalue`,
  and `scRepertoire`.
- Updated Title, Description, project URL, and author metadata.
- Replaced the legacy CITATION helpers with `bibentry()` and corrected the
  journal metadata for the 2020 Bioinformatics article.

### CRAN behavior

- Replaced all `msigdbr:::` access with the public `msigdbr()` API and added a
  small in-session cache plus graceful failure behavior.
- Changed legacy launcher export defaults from the user's Desktop to
  `tempdir()` (the explicit Docker `/plots` behavior is unchanged).
- Removed the global `NOT_CRAN=true` override and marked browser test files with
  `skip_on_cran()`.
- Wrapped the long `getMarkerGenes()` example in `\donttest{}`.
- Expanded `.Rbuildignore` so local documentation, benchmark, and maintainer
  artifacts cannot leak into a submitted tarball.
- Added focused regression tests for these CRAN contracts.

## Remaining before submission

1. Build and check the PDF manual (the current local run used `--no-manual`).
2. Validate R-devel on Windows, macOS, and Linux (win-builder/rhub or equivalent).
3. Add `cran-comments.md` explaining the single unavoidable BPCells/new-submission
   note and the external demo-data strategy.
4. Have the `Authors@R` maintainer, Michael Heming, review and submit.

The richer demo collection should ultimately live in a separately versioned
data package or data repository. That follow-up is independent of this CRAN
source-package reduction and must not reintroduce large files here.
