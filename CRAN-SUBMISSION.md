# CRAN submission plan

Working document for the `chore/cran-submission` branch. The measurements below
were collected on the original 3.0.1 research snapshot and are retained as a
historical baseline. The branch now follows 3.2.0, so sizes and check findings
must be measured again before they are used for a submission decision. Nothing
here has been implemented yet, and several steps need a decision from the
maintainer before they can be executed.

## Where we stand

The original research snapshot produced a **35 MB** source tarball against
CRAN's **5 MB** limit. That result identifies the likely dominant constraint,
but it is not a current 3.2.0 measurement.

Measured breakdown of what ships (after `.Rbuildignore` removes `data-raw/`,
`other_documents/`, `docs/`, `pkgdown/`, `default.nix`, `create_env.R`):

| Path | Size | Note |
| --- | ---: | --- |
| `inst/extdata/v1.4/demo_*.crb` (8 files) | 24 MB | the crux |
| `inst/extdata` (everything else) | 4.2 MB | `example.crb`, `example.h5`, two gene-ID tables, fixtures |
| `vignettes/` | 1.9 MB | 1.6 MB of it is images |
| `inst/shiny/` | 1.8 MB | app source |
| `tests/` | 1.0 MB | |
| `R/` + `man/` | 0.8 MB | |
| **Total** | **~34 MB** | uncompressed |

Note: `inst/shiny/v1.4/www/tutorials/` (8.4 MB) is *untracked* local pkgdown
output. It is not part of the repository, but `R CMD build` does include
untracked files, so it inflated the 35 MB figure above. A clean checkout builds
to roughly 27 MB. It should be added to `.Rbuildignore` regardless, so a
maintainer's local build output can never leak into a submission.

## Blockers, in the order they must be solved

### 1. Size — the only genuinely hard problem

Removing the eight `demo_*.crb` files takes shipped content from ~34 MB to
~10 MB. That is necessary but **not sufficient**; three further cuts are needed
to land under 5 MB:

| Step | Removes | Running total |
| --- | ---: | ---: |
| Drop the 8 `demo_*.crb` | −24 MB | ~10 MB |
| Move `example.crb`, `example.h5`, `pbmc_*.rds`, the two gene-ID tables | −3.6 MB | ~6.2 MB |
| Move vignette images to the pkgdown site and reference them by URL | −1.6 MB | ~4.6 MB |

Compressed, ~4.6 MB of mostly-text content lands comfortably under the limit.
(The `.crb` files are already xz-compressed and would not have shrunk further,
which is why they cannot be "compressed away".)

**The standard R solution is a companion data package.** Publish
`CerebroNexusData` (r-universe or a drat repo), list it in `Suggests` plus
`Additional_repositories:`, and have every example, test and vignette that needs
a data set call `skip_if_not_installed()` / degrade gracefully. This is a real
piece of work: `example.crb` alone is referenced in 22 places.

**Decision needed:** shipping no runnable demo materially weakens "install it and
it just works", which is currently one of the package's selling points. The
maintainer has to accept that trade, or CRAN is not the right target.

### 2. `Remotes:` — CRAN does not accept it

I resolved every declared dependency against the live CRAN and Bioconductor
indexes (28,178 packages). **`BPCells` is the only one in neither.** Everything
else — `scRepertoire`, `GSVA`, `biomaRt`, `qvalue`, `HDF5Array`, `monocle`,
`DelayedArray`, `ggtree`, `SingleCellExperiment`, `SummarizedExperiment` — is on
Bioconductor and therefore acceptable.

So `Remotes: bnprks/BPCells/r` must go. Two options are to drop the BPCells
backend or point `Additional_repositories:` at an r-universe build of BPCells.
The backend decision must use the current guarded real-data benchmark; the old
README pilot is not sufficient evidence for removing a supported backend.

### 3. Policy violations in the code

- **`msigdbr:::` in 4 places** (`inst/shiny/v1.4/utility_functions.R` ×3,
  `gene_expression/UI_projection_input_type.R` ×1). CRAN forbids `:::` against
  another package. This is also already broken at runtime — msigdbr 26.1.0
  removed those internal objects. `fix/deploy-hardening` has a partial fix
  (`f93d4515`) but one call site still remains there.
- **`plot_export_path <- "~/Desktop/"`** in `launchCerebroV1.0.R`,
  `V1.1.R`, `V1.2.R`. Writing outside `tempdir()` without explicit consent is a
  policy violation. Still present on `fix/deploy-hardening`.
- **`tests/testthat/setup.R` sets `NOT_CRAN = "true"`**, which forces the
  shinytest2 suite to run everywhere. On CRAN that means launching headless
  Chrome and running 8,000+ tests on their machines. Only one test file
  currently calls `skip_on_cran()`. The whole shinytest2 layer must become
  skippable.

### 4. What `R CMD check --as-cran` actually reports

Run on this branch, R 4.6.1, aarch64-apple-darwin23, with
`--no-tests --no-vignettes` (so the list below is a floor, not a ceiling):

| Finding | Severity |
| --- | --- |
| `Remotes` is an unknown DESCRIPTION field; `BPCells` not in mainstream repositories | NOTE |
| `getMarkerGenes` example runs **53 s elapsed** (CRAN's limit is 5 s) | NOTE |
| `Namespace in Imports field not imported from: 'scRepertoire'` | NOTE |
| CITATION file uses old-style `personList()` and `citEntry()` | NOTE |
| `URL` in DESCRIPTION and NEWS.md returns **301** — `mihem.github.io/CerebroNexus/` now redirects to `www.mheming.com/CerebroNexus/` | NOTE |
| `Title` is not in title case | NOTE |
| `Description` starts with the package name | NOTE |
| `Author:` disagrees with `Authors@R` (ORCID present in one, absent in the other) | NOTE |
| Non-portable file paths (all inside the untracked `www/tutorials/site_libs/`) | NOTE |
| Non-standard top-level files: `AGENTS.md`, `CLAUDE.md`, `CONTRIBUTING.md`, four `*.png`, `nix/`, `scripts/`, `tmp_refactor/` | NOTE |
| Installed size 38.7 MB (`extdata` 28.1 MB, `shiny` 9.9 MB) | INFO |
| "Imports includes 33 non-default packages… move as many as possible to Suggests" | INFO |
| Files in `vignettes/` but none in `inst/doc` | WARNING |

Three of these are more interesting than they look:

- **The 53 s example is a hard blocker.** `getMarkerGenes` cannot ship as-is;
  it needs a much smaller fixture or `\donttest{}`.
- **`scRepertoire` should move from `Imports` to `Suggests`.** It is already
  accessed through `requireNamespace()` everywhere after the lazy-loading work,
  so check correctly reports it is never imported. This also chips away at the
  "33 non-default Imports" complaint — and the same argument applies to `GSVA`,
  `biomaRt`, `httr`, `qvalue`, `future.apply` and `pbapply`, which are all
  already `requireNamespace()`-guarded.
- **The 301 on the project URL affects live files today**, not just a future
  submission: `DESCRIPTION`, `NEWS.md`, and every `mihem.github.io` link in the
  README point at a redirect. Worth raising with the maintainer independently of
  CRAN.

The non-portable paths and most of the non-standard top-level files come from
untracked local artefacts, not the repository. `.Rbuildignore` needs entries for
`www/tutorials/`, `AGENTS.md`, `CLAUDE.md`, `CONTRIBUTING.md`, `scripts/`,
`nix/`, `tmp_refactor/` and stray `*.png`, so no maintainer's working directory
can leak into a tarball.

## Sequencing

1. **Refresh the evidence on a clean 3.2.0 checkout.** Re-run source-package
   size accounting and `R CMD check --as-cran`, then replace the historical
   measurements above. Do not merge the mixed `fix/deploy-hardening` experiment
   branch wholesale; carry over only independently verified fixes that remain
   relevant.
2. **Do the cheap compliance work** (§3 remainder + refreshed §4). Mechanical, no design
   decisions, verifiable with `R CMD check --as-cran`: `.Rbuildignore` entries,
   CITATION modernisation, Title/Description wording, drop the legacy `Author:`
   field, move the `requireNamespace()`-guarded packages to `Suggests`, shrink
   or `\donttest{}` the `getMarkerGenes` example.
3. **Decide on the data strategy** (§1). This is the gate. Nothing downstream
   can be finished until it is settled.
4. **Build the data package**, rewire the 22 `example.crb` references, trim
   vignette images.
5. **Resolve BPCells** (§2).
6. **Check on all three platforms** — win-builder, macOS, Linux — via `rhub` or
   `devtools::check_win_devel()`. CRAN requires a clean run on their
   infrastructure, not just locally.
7. **Write `cran-comments.md`** and submit.

## Who can actually submit

`DESCRIPTION` lists `Maintainer: Michael Heming`. CRAN accepts submissions only
from the maintainer, and the maintainer answers for every subsequent CRAN
complaint. This cannot be driven from a fork — it needs mihem's agreement first,
and steps 3 and 5 are his calls, not ours.

## The alternative worth putting to him

`DESCRIPTION` already carries a `biocViews:` field. That field means nothing to
CRAN — it is a **Bioconductor** requirement. Somebody set this package up for
Bioconductor already.

Given 11 Bioconductor dependencies, a single-cell domain, 22 vignettes and a
large test suite, Bioconductor is the better-fitting home: heavy Bioc
dependencies are normal there rather than a liability, and ExperimentHub is
purpose-built for exactly the demo-data problem that §1 spends most of its
effort working around. Steps 2, 3 and 4 above are needed either way, so the work
is not wasted if the target changes later.
