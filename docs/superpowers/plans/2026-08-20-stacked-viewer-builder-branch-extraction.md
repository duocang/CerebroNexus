# Stacked Viewer and Builder Branch Extraction Implementation Plan

> **For the AI worker:** Required sub-skill: use
> `superpowers:executing-plans` to implement this plan inline. Track each step
> with the checklist below.

**Goal:** Move every Viewer/public-API capability that runs without the Builder
UI from `feat/builder-project-workspace` into
`feat/coordinated-linked-views`, then rebuild the Builder branch on that new
Viewer head without changing the effective Builder product tree.

**Architecture:** Protect both published heads, construct a final-state Viewer
snapshot in an isolated extraction branch, and commit it as production,
verification, and documentation groups. Rebase the existing Builder commit
range onto that Viewer head, splitting the first mixed commits and retaining
the remaining logical history. Verify ancestry, path/dependency boundaries,
tree equivalence, focused tests, and the full project check before moving and
publishing the feature refs with exact leases.

**Technical stack:** Git worktrees and plumbing, R/testthat, Shiny browser
contracts, shell path audits, `git push --force-with-lease`.

---

## File responsibilities

- `R/createShinyApp.R`, `R/bundle_path_contract.R`: public generated-App
  assembly and portable publication paths; Viewer branch.
- `R/class-Cerebro.R`, `R/convertSeuratToCerebro.R`, `R/exportFromSCE.R`,
  `R/exportFromSeurat.R`, `R/export_audit.R`, `R/spatial_image_manifest.R`:
  public data/export support used by Viewer or `createShinyApp()`; Viewer
  branch when independent of `inst/builder/`.
- `R/viewer_content_contract.R`, `inst/viewer/core/viewer_content_contract.R`:
  shared generated-App/Viewer page contract; Viewer branch.
- `inst/viewer/**`: standalone Viewer runtime; Viewer branch.
- `man/createShinyApp.Rd`, related public API documentation: Viewer branch.
- `tests/testthat/test-createShinyApp-*.R`, Viewer/Spatial/Trekker/generated-App
  tests and their non-Builder helpers: Viewer branch when they do not source
  `inst/builder/`.
- `inst/builder/**`, `R/launchCerebroBuilder.R`, Builder helpers/tests,
  `docs/builder-projects.md`, Builder vignettes and images: Builder branch.
- `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `README.md`: split by hunk; only
  dependencies/exports/documentation required by the standalone Viewer/public
  API move to the Viewer branch.
- `docs/superpowers/specs/2026-08-20-stacked-viewer-builder-branch-extraction-design.md`
  and this plan: Builder branch history documentation.

### Task 1: Freeze exact recovery points

- [ ] **Step 1: Record clean starting state and exact refs**

Run from `/Users/nuioi/projects/shiny/_wt_builder_project_workspace_codex`:

```bash
git status --short --branch
git rev-parse feat/coordinated-linked-views
git rev-parse origin/feat/coordinated-linked-views
git rev-parse feat/builder-project-workspace
git rev-parse origin/feat/builder-project-workspace
```

Expected: clean worktree; local Viewer equals its remote; local Builder may be
ahead only by the committed design/plan documents.

- [ ] **Step 2: Create immutable local and remote backup refs**

Use the date-stamped names:

```bash
git branch backup/coordinated-linked-views-pre-extraction-20260820 \
  feat/coordinated-linked-views
git branch backup/builder-project-workspace-pre-viewer-extraction-20260820 \
  feat/builder-project-workspace
git push origin \
  backup/coordinated-linked-views-pre-extraction-20260820 \
  backup/builder-project-workspace-pre-viewer-extraction-20260820
```

Expected: both remote backup refs point to the recorded local object IDs.

- [ ] **Step 3: Commit this implementation plan**

```bash
git add -f docs/superpowers/plans/2026-08-20-stacked-viewer-builder-branch-extraction.md
git commit -m "docs: plan stacked viewer and builder extraction"
```

Expected: one documentation commit on the Builder branch.

### Task 2: Construct the standalone Viewer production snapshot

- [ ] **Step 1: Create an isolated extraction worktree**

From `/Users/nuioi/projects/shiny/cerebroAppLite`:

```bash
git worktree add /Users/nuioi/projects/shiny/_wt_viewer_extraction \
  -b rewrite/coordinated-linked-views-20260820 \
  feat/coordinated-linked-views
```

Expected: the new worktree starts at `c31d2558` with a clean index.

- [ ] **Step 2: Import standalone production paths from the protected Builder head**

Restore the final versions of the public Viewer/runtime files from
`backup/builder-project-workspace-pre-viewer-extraction-20260820`:

```bash
git restore --source=backup/builder-project-workspace-pre-viewer-extraction-20260820 -- \
  R/bundle_path_contract.R \
  R/class-Cerebro.R \
  R/convertSeuratToCerebro.R \
  R/createShinyApp.R \
  R/exportFromSCE.R \
  R/exportFromSeurat.R \
  R/export_audit.R \
  R/spatial_image_manifest.R \
  R/viewer_content_contract.R \
  inst/viewer \
  man/convertSeuratToCerebro.Rd \
  man/createShinyApp.Rd \
  man/exportFromSeurat.Rd
```

Then inspect references:

```bash
rg -n "inst/builder|launchCerebroBuilder|builder_project|prepare_builder" \
  R/bundle_path_contract.R R/class-Cerebro.R R/convertSeuratToCerebro.R \
  R/createShinyApp.R R/exportFromSCE.R R/exportFromSeurat.R R/export_audit.R \
  R/spatial_image_manifest.R R/viewer_content_contract.R inst/viewer
```

Expected: no production dependency on the Builder UI. Shared pure helper names
beginning with `builder_` are allowed.

- [ ] **Step 3: Add only required package metadata hunks**

Use `git checkout -p`/`git restore -p` against the protected Builder head to
stage only metadata needed by the imported public runtime. Do not move
`launchCerebroBuilder`, Builder-only packages, the 5.0.0 version bump, or
Builder documentation.

Verify with:

```bash
git diff -- DESCRIPTION NAMESPACE .Rbuildignore .gitignore
```

Expected: every added dependency is referenced by the standalone production
paths; no Builder export or Builder fixture rule is present.

- [ ] **Step 4: Run focused production-source checks**

```bash
Rscript -e 'files <- c(list.files("R", "[.]R$", full.names=TRUE), list.files("inst/viewer", "[.]R$", recursive=TRUE, full.names=TRUE)); bad <- vapply(files, function(f) inherits(try(parse(f), silent=TRUE), "try-error"), logical(1)); stopifnot(!any(bad))'
git diff --check
```

Expected: both commands exit 0.

- [ ] **Step 5: Commit the Viewer production snapshot**

```bash
git add R inst/viewer man DESCRIPTION NAMESPACE .Rbuildignore .gitignore
git commit -m "feat(viewer): sync standalone runtime from builder stack"
```

Expected: no `inst/builder/` path is in the commit.

### Task 3: Move standalone Viewer verification

- [ ] **Step 1: Restore independent test helpers and test files**

Use the protected Builder head as source for final versions of:

```text
tests/testthat/helper-app-privacy.R
tests/testthat/helper-generated-app-assertions.R
tests/testthat/helper-generated-app-e2e.R
tests/testthat/helper-generated-app-fixtures.R
tests/testthat/helper-synthetic-spatial.R
tests/testthat/test-app-inst.R
tests/testthat/test-bundle-path-contract-drift.R
tests/testthat/test-configured-colors.R
tests/testthat/test-createShinyApp-auth.R
tests/testthat/test-createShinyApp-lock.R
tests/testthat/test-createShinyApp-publication.R
tests/testthat/test-createShinyApp-run-options.R
tests/testthat/test-createShinyApp-sibling.R
tests/testthat/test-export-equivalence-current.R
tests/testthat/test-exportFromSeurat.R
tests/testthat/test-extra_material.R
tests/testthat/test-generated-app-fixtures.R
tests/testthat/test-generated-app-multidataset.R
tests/testthat/test-generated-app-pages-analysis.R
tests/testthat/test-generated-app-pages-core.R
tests/testthat/test-generated-app-pages-immune.R
tests/testthat/test-generated-app-pages-spatial.R
tests/testthat/test-generated-app-pages-trekker.R
tests/testthat/test-generated-app-pipeline.R
tests/testthat/test-generated-app-security.R
tests/testthat/test-generated-app-server.R
tests/testthat/test-multisection-spatial.R
tests/testthat/test-seurat-v5-split-layers.R
tests/testthat/test-smoke-production.R
tests/testthat/test-spatial-coordinate-contract.R
tests/testthat/test-spatial-image-manifest.R
tests/testthat/test-spatial-image-payload.R
tests/testthat/test-spatial.R
tests/testthat/test-trekker.R
tests/testthat/test-utility_functions.R
tests/testthat/test-viewer-content-contract.R
```

Before staging, remove any restored test whose transitive helper chain sources
`inst/builder/`; leave that test on the Builder branch instead.

- [ ] **Step 2: Verify test dependency closure**

```bash
rg -n "inst/builder|helper-builder|launchCerebroBuilder" \
  tests/testthat/helper-app-privacy.R \
  tests/testthat/helper-generated-app-*.R \
  tests/testthat/test-{app-inst,bundle-path-contract-drift,configured-colors,createShinyApp-auth,createShinyApp-lock,createShinyApp-publication,createShinyApp-run-options,createShinyApp-sibling,export-equivalence-current,exportFromSeurat,extra_material,generated-app-fixtures,generated-app-multidataset,generated-app-pages-analysis,generated-app-pages-core,generated-app-pages-immune,generated-app-pages-spatial,generated-app-pages-trekker,generated-app-pipeline,generated-app-security,generated-app-server,multisection-spatial,seurat-v5-split-layers,smoke-production,spatial-coordinate-contract,spatial-image-manifest,spatial-image-payload,spatial,trekker,utility_functions,viewer-content-contract}.R
```

Expected: no test requires the Builder UI/runtime. A helper that shares a pure
path contract may be replaced with the package/public helper rather than moving
Builder code.

- [ ] **Step 3: Run focused Viewer/public API tests**

```bash
Rscript -e 'testthat::test_local(filter="coordinated-views|viewer-shell|viewer-content-contract|trekker|spatial-coordinate-contract|createShinyApp", load_package="source")'
```

Expected: zero failures. Record any environment-only conditional skips.

- [ ] **Step 4: Commit the standalone verification**

```bash
git add tests/testthat
git commit -m "test(viewer): move standalone integration coverage"
```

Expected: the commit contains no `test-builder-*` or `helper-builder-*` path.

### Task 4: Rebuild the Builder branch on the new Viewer head

- [ ] **Step 1: Create a temporary rebuilt Builder worktree**

```bash
git worktree add /Users/nuioi/projects/shiny/_wt_builder_restack \
  -b rewrite/builder-project-workspace-20260820 \
  rewrite/coordinated-linked-views-20260820
```

- [ ] **Step 2: Replay the old Builder range**

Replay commits from `c31d2558..backup/builder-project-workspace-pre-viewer-extraction-20260820`
in order. For `e7995aef`, `af49294e`, and `f644e1ac`, apply without committing,
discard paths/hunks already present in the new Viewer head, and commit only the
Builder-specific remainder with the original subject. Replay later commits in
order, resolving duplicate final-state Viewer hunks by keeping the updated
Viewer branch version.

Use:

```bash
git cherry-pick -n e7995aef
# resolve/stage only Builder-specific remainder
git commit -C e7995aef
git cherry-pick -n af49294e
# resolve/stage only Builder-specific remainder
git commit -C af49294e
git cherry-pick -n f644e1ac
# resolve/stage only Builder-specific remainder
git commit -C f644e1ac
git cherry-pick a39a2b35^..backup/builder-project-workspace-pre-viewer-extraction-20260820
```

Expected: logical later commits remain ordered; duplicate Viewer changes are not
reintroduced.

- [ ] **Step 3: Restore exact final Builder tree if replay metadata differs**

Compare against the protected head:

```bash
git diff --name-status \
  backup/builder-project-workspace-pre-viewer-extraction-20260820..HEAD
```

If differences are not solely intentional branch-design documentation, restore
the protected final content and commit the reconciliation:

```bash
git restore --source=backup/builder-project-workspace-pre-viewer-extraction-20260820 -- .
git add -A
git commit -m "refactor(builder): reconcile restacked final tree"
```

Expected: `git diff --quiet` against the protected Builder head succeeds.

### Task 5: Verify the two-PR contract

- [ ] **Step 1: Verify ancestry, path boundary, and tree identity**

```bash
git merge-base --is-ancestor \
  rewrite/coordinated-linked-views-20260820 \
  rewrite/builder-project-workspace-20260820
git diff --name-only master..rewrite/coordinated-linked-views-20260820 | \
  rg '^(inst/builder/|tests/testthat/(test-builder|helper-builder)|docs/builder-projects[.]md|vignettes/img/builder_)' && exit 1 || true
git grep -n -E 'source\(.+inst/builder|inst/builder/' \
  rewrite/coordinated-linked-views-20260820 -- R inst/viewer
git diff --quiet \
  backup/builder-project-workspace-pre-viewer-extraction-20260820 \
  rewrite/builder-project-workspace-20260820
```

Expected: ancestry succeeds, forbidden-path search is empty, production
dependency search is empty, and Builder tree comparison exits 0.

- [ ] **Step 2: Run final Viewer focused verification**

From the Viewer extraction worktree:

```bash
Rscript -e 'testthat::test_local(filter="coordinated-views|viewer-shell|viewer-content-contract|trekker|spatial-coordinate-contract|createShinyApp", load_package="source")'
```

Expected: zero failures.

- [ ] **Step 3: Run the repository-wide project verification once**

From the rebuilt Builder worktree:

```bash
NOT_CRAN=true Rscript -e 'testthat::test_local(load_package="source")'
```

Expected: zero failures; conditional skips are recorded. If the suite exposes a
pre-existing environmental failure, diagnose it and run the narrowest proving
command before deciding whether publication is safe.

### Task 6: Publish verified rewritten refs

- [ ] **Step 1: Move local feature refs to verified heads**

The Viewer feature branch is not checked out, so move it after detaching or
removing the extraction worktree. The Builder feature branch is checked out by
the existing Codex worktree; move it by resetting that clean worktree to the
verified rebuilt head after the backup ref exists:

```bash
git branch -f feat/coordinated-linked-views \
  rewrite/coordinated-linked-views-20260820
git -C /Users/nuioi/projects/shiny/_wt_builder_project_workspace_codex \
  reset --hard rewrite/builder-project-workspace-20260820
```

- [ ] **Step 2: Force-push with exact old-object leases**

```bash
git push origin \
  --force-with-lease=refs/heads/feat/coordinated-linked-views:c31d2558b907b3b02912160831c180551bd8ac93 \
  feat/coordinated-linked-views
git push origin \
  --force-with-lease=refs/heads/feat/builder-project-workspace:c7b90df12148c11878d4cf957fe29a8fe1353730 \
  feat/builder-project-workspace
```

If the Builder remote advanced to include the design/plan commits before this
step, replace the expected lease object with the recorded remote object from
Task 1, never with an unverified value.

- [ ] **Step 3: Confirm remote refs and retain recovery refs**

```bash
git fetch origin
git rev-parse feat/coordinated-linked-views origin/feat/coordinated-linked-views
git rev-parse feat/builder-project-workspace origin/feat/builder-project-workspace
git merge-base --is-ancestor \
  origin/feat/coordinated-linked-views \
  origin/feat/builder-project-workspace
```

Expected: each local/remote pair is identical and remote ancestry succeeds.
Keep both backup refs and temporary worktrees until the final report is read.
