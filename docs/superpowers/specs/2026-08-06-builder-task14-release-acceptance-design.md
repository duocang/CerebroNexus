# Builder Task 14 Release Acceptance Design

## Status and decisions

Task 14 is the release-acceptance task for the capability-aware Builder. It is
not a broad redesign and it does not introduce new scientific functionality.
The following decisions are approved:

- create a backup reference for the current local head, then rebase
  `feat/cerebro-builder` onto the latest `upstream/master` before implementation;
- retain upstream's current unreleased version, expected to be `3.2.0`, unless
  the rebase shows that version has already been released;
- use a four-layer orthogonal test design instead of multiplying every example
  by every artifact combination;
- store new permanent Builder input examples under `inst/builder/fixtures/`,
  where they ship with the Builder but are not copied into generated Viewer
  apps;
- allow narrowly scoped runtime fixes only when a new Task 14 RED test proves
  that the current behavior blocks canonical acceptance;
- do not push or open/update a pull request.

## Goal

Prove that every supported Builder content class can enter through a real
Seurat input, produce a truthful frozen Review, build and read back the expected
CRB, optionally produce a private generated app, publish without damaging
foreign data, and expose the expected Viewer behavior. Finish the English user
documentation, screenshots, unreleased version trio, and release gates.

## Non-goals

- No SCE, CRB, h5ad, h5Seurat, loom, or remote-upload input adapter.
- No service for untrusted users or arbitrary server-side uploads.
- No new scientific analysis or unsupported trajectory method.
- No unrelated Builder, Viewer, publication, or fixture-framework refactor.
- No promise of zero downtime after process death; the existing non-destructive
  recovery contract remains the boundary.
- No remote push or PR mutation.

## Architecture

Task 14 uses four orthogonal verification layers:

1. **Semantic example layer.** Each valid example runs through adapter,
   inspection, recommendation, Review, build, CRB read-back, and expected page
   validation. The invalid example must fail before any publishable artifact is
   created.
2. **Artifact matrix layer.** One minimal compatible fixture runs the exact
   eighteen combinations of three expression backends, three content modes,
   and two output modes.
3. **Browser layer.** A small number of representative multi-dataset builds
   verify actual Builder result actions and actual Viewer startup/page/privacy
   behavior.
4. **Publication lifecycle layer.** Real releases verify App removal, dataset
   shrinkage, legacy/malformed ownership, and foreign occupants without
   repeating content semantics.

This separation is intentional. Running all ten semantic scenarios through all
eighteen artifact combinations would require at least 180 expensive builds,
increase flakiness, and make failures harder to diagnose without proving a
different contract.

## Fixture catalog

`builder_examples()` becomes a projection of one catalog rather than a second
source of example truth. Each catalog entry records:

```text
id
label
detail
provenance = real | synthetic
constructor
serialized_path
expected_manifest
expected_dispositions
expected_pages
expected_supporting_content
gallery_visible
```

The initial catalog contains:

| Example | Source | Purpose |
| --- | --- | --- |
| Basic PBMC | reuse `inst/extdata/v1.4/pbmc_seurat.rds` | expression, metadata, groups, projections, colours, basic analysis |
| Spatial multi-section | new small synthetic Seurat plus small images | coordinate normalization, per-section images, alignment, Spatial page |
| Unified TCR + HLA | new small synthetic Seurat | Immune and Motif pages with typing context |
| TCR without HLA | new small synthetic Seurat | Motif page remains visible when HLA is absent |
| HLA without TCR | new small synthetic Seurat | neither Immune nor Motif page is falsely exposed |
| BCR-only | new small synthetic Seurat | Immune page visible, Motif page hidden |
| Metadata TCR conversion | new small synthetic Seurat | metadata candidate conversion reaches the expected pages |
| Legacy TCR conversion | new small synthetic Seurat | legacy content converts without changing page semantics |
| All-content synthetic | new small synthetic Seurat | all supported conditional pages and supporting fields |
| Invalid content | test-only constructor | stable blocker and zero publication side effects |

The new files live under `inst/builder/fixtures/`. The generator lives in
`data-raw/build_builder_fixtures.R` and writes only deterministic, compact,
offline inputs. It preserves the caller's random state. Existing CRB, H5, and
image demos under `inst/extdata/v1.4/` remain the source for direct Viewer,
backend, and HTTP-privacy checks; they are not duplicated or treated as Builder
inputs.

Every gallery constructor has a serialized-file equivalent. The
`ExampleAdapter` and `SeuratFileAdapter` may differ in source provenance and
file fingerprint, but from `inspect()` onward they must agree on profile,
recommendations, manifest, readiness, frozen artifact expectations, and
Viewer-page expectations.

## Runtime corrections permitted by the design

Runtime changes require a failing Task 14 test first and must stay within these
acceptance gaps:

### Conditional-page read-back

The Builder's post-build page detector must match the real Viewer gate. In
particular, the HLA & TCR Motifs page requires a supported TCR chain and does
not require HLA typing. BCR-only content must not expose the Motif page.
Conditional-page validation must inspect the required content structure, not
merely test that a broad slot is non-empty.

### Default group and projection

The confirmed `default_group` and `default_projection` must control the actual
Viewer startup state. Both must remain members of their frozen included sets.
The implementation may establish the Viewer default by a small explicit runtime
contract or by preserving the selected item first in the existing ordered
contract, but the CRB read-back and browser must prove the result. The choice
must not silently change selector membership or labels.

No other runtime change is authorized merely because Task 14 touches nearby
code.

## Data and control flow

```text
fixture catalog
  -> ExampleAdapter or SeuratFileAdapter
  -> inspect / DatasetProfile / ContentManifest
  -> deterministic recommendations
  -> user-equivalent overrides and acknowledgements
  -> immutable snapshot
  -> frozen Review / BuildPlan
  -> coordinator-owned private stage
  -> worker build
  -> CRB structure and page-gate read-back
  -> optional generated-app verification
  -> parent-owned publication
  -> final-path reopen
  -> representative Viewer browser assertions
```

The browser never constructs an output path and never owns publication.

## Failure behavior

- Invalid content produces a stable blocker before worker dispatch and leaves
  no stage, target, ownership record, or partial result.
- A selected optional analysis failure enters `Needs decision`; it cannot be
  documented or rendered as a successful skipped step.
- A manifest/read-back or page-gate mismatch makes the artifact unpublishable.
- Missing H5 or BPCells requirements fail the affected artifact-matrix case;
  the release matrix cannot count a skip as a pass.
- Each parameterized case reports its fixture/backend/content/output coordinate
  so a failure is attributable.
- Each matrix case owns an independent stage and release. Only immutable input
  fixtures may be reused.
- Existing direct `createShinyApp()` coverage for byte-identical external
  spatial assets, server-side allowlisting, and direct-URL 404 remains in place
  and is not reimplemented in Builder-specific helpers.

## Verification design

### Catalog and adapter contracts

- IDs and labels are unique; provenance and gallery visibility are explicit.
- Constructors are deterministic and do not change the caller's `.Random.seed`.
- Constructor and serialized-file paths converge after inspection.
- The invalid fixture produces the exact intended blocker.

### Semantic content scenarios

Each valid example performs a real build and read-back. The immune/HLA cases
must produce this page matrix:

| Scenario | Immune page | Motif page |
| --- | ---: | ---: |
| Unified TCR + HLA | yes | yes |
| TCR without HLA | yes | yes |
| HLA without TCR | no | no |
| BCR-only | yes | no |
| Metadata TCR conversion | yes | yes |
| Legacy TCR conversion | yes | yes |

All-content covers every supported conditional page and required supporting
field. Each page has a structure validator and at least one representative
smoke render; getter non-emptiness alone is insufficient.

### Exact artifact matrix

Run all combinations:

```text
{embedded, h5, bpcells}
  x {plain, histology, Trekker}
  x {CRB only, generated app}
```

Verify backend descriptors, sidecar relative paths, lazy reopen, stage-to-final
path remapping, CRB identity, generated-app verification, and private asset
closure.

### Browser flows

Use representative Basic, Spatial, immune/HLA, all-content, and invalid flows
rather than one browser per matrix coordinate. Verify:

- conditional page insertion/removal/reinsertion (`has -> no -> has`);
- automatic and explicitly pinned initial dataset;
- non-first default group and projection;
- group and cell-cycle palettes;
- upload visibility;
- Open App, Reveal Folder, Copy Path, and Copy Report keyboard/click behavior;
- no direct CRB, H5, BPCells, `private-data`, or spatial file URL;
- Builder-normalized histology remains embedded, retains bounds/transforms,
  renders, and has no HTTP file URL.

### Publication lifecycle

- Publish two datasets plus App, then one dataset without App to the same
  release.
- Preserve and block on a legacy release without an ownership record.
- Preserve and block on malformed ownership.
- Preserve and block on top-level and nested foreign occupants.
- Reuse existing coordinator and privacy helpers instead of creating a parallel
  publication implementation.

## Documentation and screenshots

README remains short: positioning, launch command, four-stage summary, major
safety boundaries, and a link to the full Builder vignette.

The vignette is rewritten around the novice path:

1. Import and Inspect.
2. Core setup.
3. Enhance content.
4. Exact Review and Build.
5. Result actions and recovery.

It explicitly distinguishes:

- CRB-only output from generated private App output;
- private data from `viewer_bundle_assets` and from actual HTTP exposure;
- snapshot disk cost and cleanup;
- interruption, retry, rollback, recovery, and ownership migration;
- every phase-one limitation;
- selected-analysis failure as `Needs decision`, not silently skipped work.

Replace stale pre-Task-13 screenshots with a small final set captured from an
installed package: source/gallery, Core/preview, Enhance spatial/content,
exact Review, success/actions, plus one narrow Dataset Manager view when it
adds information. Screenshots must contain no user-specific absolute path.

## Version and release checks

After the approved rebase, inspect the current upstream unreleased heading.
Use that version consistently in `DESCRIPTION`, `inst/app.R`, and the current
unreleased `NEWS.md` heading. Move the Task 13 Builder entry out of any released
historical heading rather than rewriting other history.

Final verification includes:

- focused RED/GREEN loops for each layer;
- all Builder tests;
- full repository test suite with exact failure/warning/skip/pass counts;
- `R CMD check` on a freshly built source package;
- pkgdown build with warnings and stale references reviewed;
- clean temporary-library install and launcher/package smoke;
- final stale-reference search, `git diff --check`, status review, generated
  artifact review, and complete branch diff against the PR base.

## File boundaries

Expected files include:

- modify `inst/builder/io.R` for the catalog and constructors;
- add small files under `inst/builder/fixtures/`;
- update `data-raw/build_builder_fixtures.R` and
  `data-raw/builder_fixtures.md` as the fixture source of truth;
- modify `inst/builder/build.R` only for RED-proven page/default gaps;
- add `tests/testthat/helper-builder-end-to-end.R` for shared expensive fixture
  setup;
- add `tests/testthat/test-builder-end-to-end.R` for Task 14 contracts;
- extend existing browser/privacy/coordinator tests only when ownership of an
  assertion already belongs there;
- update `README.md`, `vignettes/build_a_data_set_by_pointing.Rmd`, final Builder
  screenshots, `NEWS.md`, `DESCRIPTION`, and `inst/app.R`;
- leave `_pkgdown.yml` unchanged unless the article name or path changes.

## Commit structure

Use two logical commits after the rebase:

1. `feat(builder): complete example matrix` — fixture catalog and files,
   RED-proven runtime corrections, and end-to-end tests.
2. `docs(builder): document guided workflow` — README, vignette, screenshots,
   unreleased NEWS, and version trio.

Do not preserve a permanently failing RED commit and do not push.

## Acceptance

Task 14 is complete only when all four layers pass, the installed-package user
path is exercised, the current unreleased documentation describes the final
behavior, no Critical or Important review finding remains, the working tree is
clean, and the branch remains local and unpushed.
