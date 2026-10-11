# PR184 feedback and post-PR6 history consolidation

Date: 2026-10-11. Existing branch/worktree: `卧槽/pr6_clean_perf`.
Starting HEAD: `79e26c60809dd83a203b2f4e47a61aa56c5c8be9` (47 commits after PR6).
Unchanged PR6 boundary: `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`.
Feedback: https://github.com/mihem/CerebroNexus/pull/184#issuecomment-6098041614
(mihem, 2026-10-10). PR184 targets the PR5 branch; the test target here is clean
PR6 with subsequent fixes, not necessarily the revision tested by the reviewer.

## Changes

- Restore the previous per-page sidebar icon palette, adding colours for Linked,
  Trekker and HLA. Keep text labels, focus outlines and the active navigation pill.
- Replace Linked's static-mode notice/button with one native checkbox, "Show
  linked cell hover", in the same location. Remove the duplicate Settings item.
  Default on for every dataset size. Preserve the user's choice on revisit and
  supplemental data arrival; reset to on for a new dataset. Keep this preference
  independent of Data info's specialist hover-detail preference.
- Remove Linked-only static-mode state and handlers. Keep specialist network
  static mode. When restoring the shared surface to Linked, clear specialist
  pointer suppression and its static pane class. Hover off still leaves click,
  brush, pan, zoom and explicit pinned Details available.
- Share the page loader's 400 x 104 px card, 28 px ring, spacing and typography
  with legacy output spinners (including Immune statistics). Retain the
  shinycssloaders hidden/busy lifecycle, proxy sizing and reduced-motion support.
  Browser QA caught a CSS specificity conflict in the first candidate: shared
  `display:flex` overrode the package's hidden class. The final rule explicitly
  hides completed output spinners; final DOM checks confirm `display:none`.

No R analytical code, data payload, expression precision, geometry, Viewer Pack,
materialization policy, caching, scheduling or ready/paint protocol was changed.
Default-on hover enables the existing hit-testing/hover rendering during pointer
interaction; this is an intentional behaviour change, not a claim of zero work.

## Immune loading boundary

The current Clonal projection already uses the shared cell-view renderer and
packed repertoire availability path. Default Abundance already has a native
Plotly/count path. Neither needs the scRepertoire namespace on its fast path.
Other statistics, such as Homeostasis, still call scRepertoire and may pay its
first namespace load, repertoire processing and Plotly conversion. See
`immune_repertoire/server.R`, `data.R`, and `visualizations.R` for these existing
boundaries. Namespace cost is not eliminated by prewarming on another page.

This change fixes the inconsistent loading presentation. It does **not** establish
that every Immune statistic loads as quickly as Projection. No new timing A/B,
profiling series, blanket latency claim, or performance rewrite was performed.
Analytical latency remains a separate limitation; Gene/Linked/Trajectory/HLA/
Spatial performance investigations remain closed. No Builder/Spatial contracts
were imported.

## Validation and evidence

- WSL R 4.6.1 `testthat::test_local()` passed the seven filters in `tests.log`:
  linked-hover-toggle, linked-interaction-performance, viewer-accessibility-design,
  result-page-loader, viewer-review-feedback, ir-definition-sharing and
  result-page-controls. Node syntax and `git diff --check` passed.
- The Linked Node fixture covers million-cell default-on, disabled hover avoiding
  nearest-point lookup, retained selection, specialist hover independence,
  supplemental update preference retention, dataset reset, and restoring a
  specialist-static shared canvas to interactive Linked mode.
- Real **Windows Brave**, extension-controlled; production app served by **WSL R**
  at `http://127.0.0.1:8947/`. `server.R` supplies Demo, Mouse and Ren; only Demo
  (1,476 cells) and Ren (1,462,702 cells) were exercised here. Natural viewport
  changed from 1941 x 1429 to 1488 x 1301 during the session; no latency comparison
  is inferred. This is a targeted functional smoke, not a benchmark.
- Demo: coloured icons; Linked default hover/pinned tooltip; Settings has no
  duplicate hover control; hover off preserves selection and zoom; Space toggles
  the native checkbox. Clonal projection, Abundance and Homeostasis rendered.
- Ren: default-on hover works before deferred annotations finish; hover off still
  supports box selection (3,397 cells in this gesture, matching both panels).
  Clonal projection, Abundance and Homeostasis rendered; final output spinners
  were hidden. Returning from Immune retained hover off and the 3,397-cell cohort.
  Ren -> Demo restored default hover on, showed "No selection" and no old tooltip.
- Final browser warning/error capture was empty; no R error or visible Shiny
  output error was observed. The owned test tab and WSL service were closed.
- `browser-checks.json` contains DOM observations, including explicitly labelled
  intermediate candidate observations and final-CSS rechecks. Screenshots show
  actual loading and drawn/interactive states, without injected delays or ready
  signals. `console.json` and `r-session.log` retain diagnostics. Ren Abundance
  emits existing RColorBrewer Set2 size warnings; no R analytical code was changed
  here. These warnings are not described as an error-free/warning-free session.

## Backup and consolidation procedure

External backup directory:
`C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-11-pr6-before-pr184-and-squash-083417`.
It contains a verified full-history bundle at the starting HEAD, binary working
tree/index patches, copied untracked files, ref/commit manifests and SHA-256
checksums. The first backup deliberately includes the uncommitted UI edits made
before the user's interruption; the bundle itself preserves their clean parent.

After committing the validated fix, `compact.ps1` creates and verifies another
bundle including that commit. It consolidates only contiguous groups after the
unchanged PR6 boundary, preserving their order and each group's endpoint tree.
It checks exact final-tree equality before compare-and-swap updating only the
current branch. No reset/checkout, new branch/worktree, other-ref rewrite or push
is needed. The external `old-to-new-commits.csv` maps every original SHA to the
consolidated commit; `squash-result.json` records both tips and the common tree.
Historical benchmark SHAs remain historical identifiers, recoverable from the
bundles; reports are not rewritten to pretend they ran on new commit IDs.
