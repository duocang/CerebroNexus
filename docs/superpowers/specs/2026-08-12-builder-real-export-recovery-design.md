# Builder Real Export Recovery Design

## Status

Approved direction: restore the shortest correct path from two real Seurat
uploads to a complete Viewer App, with external multi-image spatial assets.

## Context

The command-line export of the two `anna_lena` datasets succeeds, but the same
configuration cannot currently be completed in Builder. Investigation found
two product-contract failures and one verification gap:

- Builder's top-level UI is a bare `tagList()`, while the spatial editor uses
  `shiny::showModal()`. The generated modal calls Bootstrap's jQuery
  `.modal()` plugin, but Builder does not provide that JavaScript dependency.
- Configure-stage readiness freezes a CRB-only preview plan. External images
  are valid only when a Viewer App is included, so a valid future App export is
  rejected before the user can reach the output-mode control.
- The generic staged-workflow browser test passes. The reported Continue
  failure therefore needs a regression test using uploaded real RDS files and
  the same metadata and image settings before it is treated as a separate
  production defect.

## Goal and success criteria

Builder must export a complete App from the two real RDS files with:

- all 11 supported source metadata columns retained, producing 12 CRB columns
  after generated `cell_barcode` is included;
- eight Groups with `cell_type` as the default;
- UMAP and t-SNE, six spatial sections, and Trekker per dataset;
- seven named external images per dataset, 14 total;
- an App that starts successfully and returns HTTP 200;
- a build report and Viewer configuration that describe the same storage
  topology and image manifest as the produced files.

The existing command-line API and H5/BPCells implementations are out of scope.

## Design

### Modal dependency

Builder will explicitly provide the Bootstrap JavaScript dependency required
by `shiny::showModal()` while preserving the existing custom layout and CSS.
The change must not wrap the application in a Bootstrap page that silently
changes layout semantics. A browser regression test will open and complete the
duplicate-image naming dialog and assert clean console logs.

### External-image output requirement

External images imply a complete App. Configure and Review must represent that
requirement without constructing an invalid CRB-only preview plan.

The frozen-plan validator remains strict: a final plan with external images and
`make_app = FALSE` is invalid. The workflow layer will detect external image
requirements and freeze preview/review plans with `make_app = TRUE`. The build
stage will initialize and retain App output mode for such plans, and the UI
will explain that external images require `CRB files + Viewer App`. Users may
switch back to CRB-only output only after changing image storage to embedded or
removing external images.

This keeps correctness in the plan authority instead of weakening final build
validation.

### Real-data workflow regression

A focused browser test will upload both real fixture-compatible RDS datasets,
apply full metadata retention and all eligible Groups, select external image
storage, add multiple named images to the same FOV, save alignments, and enter
Review. The test will verify that Review declares a complete App and that no
browser errors occur.

If repository tests cannot depend on the user's Downloads directory, the test
will use the packaged `all_content.rds` fixture twice or generate equivalent
temporary inputs, while a separate local acceptance script exercises the
actual `/Users/nuioi/Downloads/anna_lena/data` files.

## Error handling and invariants

- Image labels remain non-empty and unique within a section.
- Unsaved alignments still block freezing.
- External image paths remain App assets, never private-data members.
- Final CRB-only plans with external images remain rejected.
- Modal cancellation leaves the prior image collection unchanged.
- Existing embedded-image and CRB-only workflows remain valid.

## Verification

Verification proceeds from smallest to largest:

1. Unit/server tests for App requirement inference and strict final validation.
2. Browser regression for modal completion and clean logs.
3. Existing focused Builder spatial, workflow, plan, and build tests.
4. Real Builder UI export of the two `anna_lena` datasets.
5. CRB, configuration, image-manifest, file-tree, and HTTP comparison against
   the regenerated command-line App.
