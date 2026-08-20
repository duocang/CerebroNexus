# Builder Storage and Metadata Parity Design

## Status

Approved design direction: explicit, independent storage controls for expression
matrices and spatial images, plus a metadata-retention policy that is separate
from Group eligibility.

## Context

The Builder and the command-line export path currently produce equivalent core
expression, projection, spatial-coordinate, and Trekker content, but they do
not make the same decisions about spatial images and metadata:

- `exportFromSeurat()` already supports `embedded`, `h5`, and `bpcells`
  expression backends. Builder exposes this choice under Advanced settings.
- `createShinyApp(spatial_images = ...)` copies file-backed images into
  `spatial-assets/`, while Builder-managed uploads are converted to Base64 data
  URIs and embedded in the CRB.
- Builder's metadata recommendation policy excludes columns with fewer than
  two usable values. Build preparation then removes those columns before
  calling `exportFromSeurat(add_all_meta_data = TRUE)`. As a result, a constant
  `orig.ident` column is intentionally absent from Builder output even though
  the direct command-line export retains it.
- The Review stage can report zero excluded columns even when the frozen build
  policy removes `orig.ident`. This makes an intentional policy look like
  accidental data loss.

The product goal is not to make every Builder choice identical to every
possible R call. It is to let a user reproduce the important command-line
choices explicitly, understand the resulting artifact topology, and verify
the choices before building.

## Goals

1. Keep expression storage and spatial-image storage as independent concepts.
2. Let Builder users select external or embedded storage for Builder-managed
   spatial images.
3. Retain ordinary, supported metadata independently of whether a column can
   be used as a Group.
4. Preserve conservative Group recommendations.
5. Make Review and `build-report.json` describe the artifacts that will
   actually be produced.
6. Preserve existing CRBs, existing Builder plans where possible, privacy
   boundaries, atomic publication, and Viewer compatibility.
7. Provide a concrete parity fixture: the two `anna_lena` Seurat objects must
   be exportable with 12 metadata columns, eight Groups, six spatial sections,
   and seven named images per dataset.

## Non-goals

- Redesigning the H5 or BPCells backends.
- Automatically choosing image storage by file size.
- Supporting a different image storage mode for every individual image in the
  first release.
- Turning continuous or high-cardinality metadata into Groups automatically.
- Making private CRB, H5, or BPCells data HTTP-addressable.
- Introducing a new image database, object store, or remote URL protocol.

## Product model

### Two independent storage axes

Builder presents two separate choices:

1. **Expression storage**, per dataset:
   - `embedded`
   - `h5`
   - `bpcells`
2. **Spatial image storage**, per dataset:
   - `external`, the default for new Builder sessions
   - `embedded`

Expression storage continues to use the existing recommendation engine. Small
matrices are recommended as embedded; large sparse matrices prefer BPCells;
HDF5 is the fallback supported on both the build and Viewer sides.

Spatial image storage is explicit rather than size-dependent. The default is
external because it matches `createShinyApp(spatial_images = ...)`, keeps CRBs
inspectable, avoids Base64 inflation, and provides a natural home for multiple
named images per section.

### Metadata has two independent decisions

Each source metadata column has two properties:

- `retain_in_crb`: whether the value is exported as ordinary metadata.
- `group_enabled`: whether it is registered as a categorical Group.

The invariant is one-way:

```text
group_enabled = TRUE  =>  retain_in_crb = TRUE
retain_in_crb = TRUE  does not imply group_enabled = TRUE
```

Supported, non-sensitive metadata is retained by default, including constant,
continuous, and high-cardinality columns. The existing conservative rules are
used only to recommend Group eligibility and default Group selection.

`orig.ident` is therefore retained by default but is not Group-eligible when
it has only one value.

Unsupported metadata types remain excluded. Sensitive-looking fields are not
retained automatically and require explicit confirmation before inclusion.
Mandatory identity, QC, and feature-dependent columns remain forced-retained.

## Builder user interface

### Dataset configuration

The existing Advanced settings area keeps the expression control, renamed for
clarity to **Expression storage**. Help text explains the output topology:

- Embedded: expression is inside the CRB.
- HDF5: CRB plus a private `.h5` companion file.
- BPCells: CRB plus a private `.bpcells` companion directory.

The Spatial editor adds **Image storage** at dataset scope:

- External files in App (`spatial-assets/`) — default.
- Embedded in CRB.

Changing image storage does not discard uploaded files or alignment settings.
It only changes how frozen image declarations are materialized during build.

The first release applies one image-storage mode to all Builder-managed images
in a dataset. Images already embedded in an uploaded CRB remain embedded and
are identified as source content rather than silently extracted.

### Metadata workspace

The Groups workspace becomes a Metadata and Groups workspace. Each column row
shows:

- column name and type;
- non-missing and distinct counts;
- Keep in CRB state;
- Group eligibility and Group selection;
- a reason when retention or Group use is restricted.

The interface offers separate actions:

- Keep all supported metadata;
- Restore recommended retention;
- Select suggested Groups;
- Select all eligible Groups.

Forced-retained columns have a locked Keep control. Unsupported columns have a
disabled control with the exact reason. Sensitive fields require an explicit
per-column confirmation and are never included by a bulk action.

### Review

Review reports independent summaries, for example:

```text
Metadata: 12 retained · 0 excluded
Groups: 8 included · Default: Cell type
Expression storage: Embedded
Spatial: 6 sections · 7 images · External spatial-assets
```

Excluded metadata is listed with reasons. The counts are derived from the same
frozen policy consumed by the build rather than reconstructed from Group
choices.

Spatial Review counts actual spatial entries and actual image declarations.
Fixture catalog descriptions such as "seven histology images" must not be
used as a section count.

## State and plan contracts

### Metadata policy schema

The effective metadata policy gains an explicit retention decision per column.
The frozen representation contains:

```r
metadata_policy <- list(
  columns = list(
    orig.ident = list(
      retain_in_crb = TRUE,
      group_eligible = FALSE,
      group_enabled = FALSE,
      retention_reason = "Supported ordinary metadata.",
      group_reason = "This column does not contain two usable categories.",
      forced = FALSE,
      sensitive = FALSE
    )
  ),
  retained = c("cell_barcode", "orig.ident", ...),
  excluded = character(),
  groups = c("cell_type", ...)
)
```

Existing disposition fields may remain as compatibility evidence while all new
build and report code reads the explicit retained and Group decisions.

Plan validation enforces:

- all referenced columns exist, except the reserved generated
  `cell_barcode`;
- retained and excluded sets are unique and disjoint;
- every Group is retained and Group-eligible;
- forced columns cannot be excluded;
- unsupported columns cannot be retained;
- sensitive columns carry an explicit confirmation token when retained.

### Spatial image policy schema

Dataset settings and frozen plan items gain:

```r
spatial_image_storage <- "external" # or "embedded"
```

The value is required in new frozen plans. Upgrade logic assigns `embedded` to
legacy in-memory Builder entries so reopening an existing session does not
silently change its output. Newly imported datasets receive `external` from
the new defaults.

Frozen image records retain their original path identity, image label,
section/FOV identity, bounds, and alignment settings. Freezing does not Base64
encode image bytes.

## Build data flow

### Expression data

No architectural change is required:

```text
Builder selection
  -> frozen item.expression_backend
  -> exportFromSeurat(expression_matrix_mode = ...)
  -> CRB only, CRB + H5, or CRB + BPCells
  -> createShinyApp copies private artifacts under private-data/
```

### Embedded spatial images

For `spatial_image_storage = "embedded"`:

1. Export the CRB.
2. Encode each Builder-managed image into the canonical
   `histology_images[[label]]` payload.
3. Store bounds and alignment identity in the CRB.
4. Pass no external declaration for those images to `createShinyApp()`.

This preserves today's portable-CRB behavior.

### External spatial images

For `spatial_image_storage = "external"`:

1. Export spatial coordinates in the CRB without adding Builder-managed Base64
   image payloads.
2. Convert frozen image records into the nested manifest accepted by
   `createShinyApp(spatial_images = ...)`:

   ```r
   dataset -> spatial entry -> image label -> list(path, bounds)
   ```

3. Convert alignment controls into `spatial_image_settings` using the same
   dataset, spatial-entry, and image-label keys.
4. Let `createShinyApp()` copy verified files into
   `spatial-assets/<dataset>/<spatial>/` and write relative config paths.
5. Keep CRBs, H5 files, and BPCells directories under `private-data/`; spatial
   images remain server-read assets and are not registered as public Shiny
   resources.

The Builder app assembly request carries the image manifest and settings as
typed data. It must not discover image files by walking the output directory.

### CRB-only builds

An external image is not part of a CRB. Therefore:

- Embedded mode may continue to produce standalone CRB files.
- External mode with selected images cannot claim to produce a standalone CRB.

For the first release, CRB-only + external images is rejected at Review with a
clear resolution: select Embedded image storage or build CRB files + Viewer
App. A future manifest bundle format is outside this design.

## Existing embedded images and rebuild behavior

Existing canonical `histology_images` already present in a source object or CRB
are preserved. Builder only manages images explicitly uploaded in the current
Builder record.

- In Embedded mode, a rebuilt Builder-managed image replaces the prior image
  with the same Builder identity while unrelated embedded images remain.
- In External mode, Builder-managed images are emitted externally and are not
  duplicated inside the CRB. Unrelated source-embedded images remain embedded.
- A label collision between a preserved embedded image and a new external image
  is rejected before build with dataset, section, and label in the message.

## Multi-image compatibility

This design must not preserve the current one-upload-per-section limitation in
new contracts. Dataset state, frozen plans, app requests, and reports model
images as named collections even if the first implementation initially wires
the storage selector before the complete multi-image editor.

Each image identity is the tuple:

```text
(dataset id, spatial entry id, image label)
```

Labels are non-empty and unique within one spatial entry. The planned
multi-image editor will use this identity for H&E, DAPI, IF, PAS, replacement,
ordering, and alignment settings.

## Build report

`build-report.json` adds explicit per-dataset records:

```json
{
  "metadata": {
    "retained": ["cell_barcode", "orig.ident", "cell_type"],
    "excluded": [],
    "forced": ["cell_barcode", "nUMI", "nGene"]
  },
  "groups": ["cell_type"],
  "expression_storage": {
    "mode": "embedded",
    "sidecars": []
  },
  "spatial_image_storage": {
    "mode": "external",
    "image_count": 7,
    "section_count": 6
  }
}
```

The existing top-level fields remain during a compatibility period. Report
validation rejects disagreement between old summary fields and the new typed
records.

## Privacy and integrity

- Metadata retention defaults include only supported, non-sensitive columns.
- Sensitive-field confirmation is preserved in the frozen plan and verified
  at build time.
- External image paths must pass the existing canonical-path, symlink,
  mutation, duplicate-target, and private-stage checks.
- Only files named by the frozen manifest are copied.
- CRBs, H5 files, and BPCells directories remain outside any HTTP resource
  mapping.
- External images are read by the server and converted to data URIs; the
  `spatial-assets/` directory is not exposed directly.
- Build reports contain relative artifact members and column names, not source
  absolute paths or metadata values.

## Error handling

Build is blocked before writing artifacts when:

- HDF5 or BPCells dependencies are unavailable for the selected expression
  backend;
- image storage has an unknown value;
- CRB-only output is combined with external Builder-managed images;
- a frozen image is missing, changed, unreadable, linked unsafely, or collides
  with another target;
- an external image label conflicts with a preserved embedded label;
- a Group is excluded from metadata;
- a forced metadata column is excluded;
- unsupported metadata is selected;
- sensitive metadata lacks confirmation.

Messages identify the dataset and relevant column or image without including
metadata values.

## Compatibility and migration

- CRB readers continue to support canonical embedded images and legacy singular
  image fields through existing normalization.
- Existing command-line `createShinyApp()` calls are unchanged.
- Existing Builder sessions without `spatial_image_storage` upgrade to
  `embedded`, preserving prior output.
- New Builder imports default to `external`.
- Existing metadata policies upgrade conservatively: columns previously marked
  effectively included remain retained; previously excluded supported columns
  remain excluded in reopened sessions. New imports use the new retain-all-
  supported default.
- Frozen plan schema/version changes are explicit. Unknown newer schemas fail
  closed.

## Testing strategy

### Metadata tests

- Constant `orig.ident` is retained but not Group-eligible in a new import.
- Continuous metadata is retained but not automatically grouped.
- Existing low-cardinality Groups remain selected as before.
- Unsupported list columns remain excluded.
- Sensitive columns require explicit confirmation.
- Build preparation exports exactly the frozen retained columns.
- Review and report counts match the built CRB.

### Spatial storage tests

- New imports default to external images; upgraded old sessions remain
  embedded.
- Embedded mode stores canonical Base64 payloads in the CRB and creates no
  external declaration.
- External mode leaves Builder-managed Base64 payloads out of the CRB and
  copies named files under `spatial-assets/`.
- Alignment settings reach the Viewer for both modes.
- Existing source-embedded images survive both modes.
- CRB-only + external image selection is blocked.
- Missing, changed, linked, duplicate, and colliding image sources fail closed.

### Expression backend regression tests

- Embedded, HDF5, and BPCells plans still freeze, build, bundle, and reopen.
- H5 and BPCells companion artifacts remain under `private-data/`.
- The image-storage selection does not change the expression backend.

### End-to-end parity fixture

Using both `anna_lena` objects, compare Builder output with the direct R script:

- 180 cells and 160 genes per dataset;
- identical expression matrices and UMAP/t-SNE projections;
- 12 metadata columns including `orig.ident`;
- the same eight Group names and levels;
- six spatial sections;
- seven named images per dataset, 14 total;
- labels DAPI, H&E, IF, and PAS preserved;
- external mode uses `spatial-assets/` without new Builder Base64 images;
- both generated Apps start on temporary ports and return HTTP 200;
- Spatial and Trekker Viewer content is available.

## Delivery sequence

1. Separate metadata retention from Group recommendation and fix Review/report
   accounting.
2. Add the spatial-image storage contract and UI selector.
3. Route external image manifests and alignment settings through app assembly.
4. Extend the spatial editor to multiple named images per section using the
   same contracts.
5. Run expression-backend regressions and the complete `anna_lena` parity
   acceptance test.

Each step must be independently tested and committed. The implementation plan
will name the exact production and test files, specify red-green test commands,
and include compatibility checkpoints for the current dirty integration
worktree.
