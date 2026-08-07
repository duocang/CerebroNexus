# Builder Viewer Content Implementation Plan

**Goal:** Add per-dataset Viewer content catalogs, defaults, previews, Review
summaries, and build persistence without changing unrelated Builder behavior.

**Approach:** Extend the existing typed profile and worker preview boundary,
normalize one canonical settings shape, and let UI/build code consume that same
shape. Work test-first in seven small commits. Run only focused Builder tests
and `git diff --check`.

## Task 1: Profile and catalogs

**Files:**

- Modify `inst/builder/profile.R`
- Modify `inst/builder/content_tables.R`
- Modify `inst/builder/preview.R`
- Modify `inst/builder/session.R`
- Test `tests/testthat/test-builder-profile.R`
- Test `tests/testthat/test-builder-preview.R`

1. Add failing tests for metadata classification, friendly eligibility reasons,
   missing percentages, bounded five-row previews, level counts, exportable 2-D
   reductions including PCA, supported trajectory records, and capped previews.
2. Extend profile summaries without returning full metadata or expression.
3. Add deterministic worker preview requests for projection and supported
   trajectory records.
4. Run the profile/preview focused tests and commit.

## Task 2: Canonical per-dataset settings

**Files:**

- Modify `inst/builder/plan.R`
- Modify `inst/builder/state.R`
- Modify `inst/builder/preview.R`
- Test `tests/testthat/test-builder-state.R`
- Test `tests/testthat/test-builder-plan.R`

1. Add failing upgrade, normalization, invariant, reorder, deletion, and
   dataset-isolation tests.
2. Normalize the eight Viewer-content fields, preferring existing selections
   and repairing defaults only when absent or excluded.
3. Upgrade legacy color fields to `group_color_overrides` with read fallback.
4. Run state/plan tests and commit.

## Task 3: Groups UI

**Files:**

- Modify `inst/builder/ui/core_stage.R`
- Modify `inst/builder/app.R`
- Modify `inst/builder/www/builder.css`
- Modify `inst/builder/www/builder.js`
- Test `tests/testthat/test-builder-stages.R`
- Test `tests/testthat/test-builder-app.R`

1. Add failing HTML/model tests for the Viewer-content section, complete
   metadata catalog, search, selection helpers, real default-group radios,
   bounded detail preview, and per-group colors.
2. Replace the old default-group select with the expandable Groups card.
3. Route inclusion/default/color mutations through dataset replacement so they
   invalidate Reviewed, while client-only search/disclosure does not.
4. Run stage/app focused tests and commit.

## Task 4: Projection gallery

**Files:**

- Modify `inst/builder/ui/core_stage.R`
- Modify `inst/builder/app.R`
- Modify `inst/builder/www/builder.css`
- Modify `inst/builder/www/builder.js`
- Test `tests/testthat/test-builder-stages.R`
- Test `tests/testthat/test-builder-preview.R`

1. Add failing tests for all exportable 2-D cards, include/default controls,
   actual preview points, deterministic caps, shared group colors, and point-size
   changes.
2. Add lazy worker-backed preview requests keyed by dataset/reduction/group.
3. Render accessible SVG mini-scatters and dataset-level point-size control.
4. Run stage/preview focused tests and commit.

## Task 5: Trajectory catalog

**Files:**

- Modify `inst/builder/ui/core_stage.R`
- Modify `inst/builder/app.R`
- Modify `inst/builder/content_tables.R`
- Modify `inst/builder/preview.R`
- Modify `inst/builder/session.R`
- Modify `inst/builder/www/builder.css`
- Test `tests/testthat/test-builder-stages.R`
- Test `tests/testthat/test-builder-preview.R`

1. Add failing tests for conditional visibility, monocle2 selection, unsupported
   secondary content, coverage/state summaries, preview, and default radio.
2. Build a stable method/name catalog and worker-backed bounded preview.
3. Render the card only when trajectories exist and persist supported choices.
4. Run stage/preview focused tests and commit.

## Task 6: Review and Reviewed semantics

**Files:**

- Modify `inst/builder/ui/review_stage.R`
- Modify `inst/builder/app.R`
- Test `tests/testthat/test-builder-stages.R`
- Test `tests/testthat/test-builder-app.R`

1. Add failing tests for compact content summaries and absence of raw catalogs,
   previews, identifiers, and internal diagnostics.
2. Render one concise Viewer-content summary per dataset.
3. Verify content mutations invalidate Reviewed and transient UI actions do not.
4. Run stage/app focused tests and commit.

## Task 7: Build and Viewer persistence

**Files:**

- Modify `inst/builder/plan.R`
- Modify `inst/builder/build.R`
- Modify `inst/builder/app_bundle.R`
- Modify `R/createShinyApp.R`
- Modify Viewer initialization only where required by the frozen contract
- Test `tests/testthat/test-builder-plan.R`
- Test `tests/testthat/test-builder-build.R`
- Test relevant App bundle and Viewer preference tests

1. Add failing tests for filtered groups/reductions/trajectories, preserved safe
   metadata, opening defaults, point size, and legacy fallback behavior.
2. Freeze the normalized settings into BuildPlan items, filter copied objects,
   and persist App defaults without runtime Builder paths.
3. Make Viewer initialization prefer frozen defaults and retain its existing
   first-available fallback for older artifacts.
4. Run focused build/App persistence tests and commit.

## Final verification

Run:

```sh
R -q -e 'devtools::test(".", filter="builder-(profile|state|plan|stages|preview)", reporter="summary")'
R -q -e 'devtools::test(".", filter="builder-(build|app|app-bundle)", reporter="summary")'
git diff --check
```

Then perform one specification review and one code-quality review. Do not run
package check, pkgdown, broad browser E2E, rebase, push, or create a PR.
