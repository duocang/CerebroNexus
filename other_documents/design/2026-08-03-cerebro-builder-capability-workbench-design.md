# Cerebro Builder Capability Workbench Design

- **Status:** Approved; amended for private generated-app integration
- **Date:** 2026-08-03
- **Amended:** 2026-08-05 after upstream PR #102 and Builder Task 9
- **Target branch:** `feat/cerebro-builder`
- **Current branch head when designed:** `a539cba5`
- **Current branch head when amended:** `a236e0a9`
- **Primary user:** A first-time Cerebro user who should not need to write R
- **Scope:** Complete Seurat-to-Cerebro coverage first; keep the input boundary
  extensible for later SCE and CRB adapters

This specification governs the next Builder redesign and supersedes the
original implementation outline that produced the current branch. Existing
Builder code remains useful evidence, but it is not the acceptance contract for
the next phase.

## Executive summary

The current Builder has a strong visual foundation and several sound technical
ideas: large Seurat objects live in a background R process, build settings are
converted into a plain plan, and outputs are staged before publication.

It is not yet a trustworthy novice workflow. It currently presents exporter
parameters as one long card wall, reports content from shallow presence checks,
always embeds expression matrices, hides the metadata disclosure policy, and
does not verify that the generated CRB will actually enable the Viewer pages it
promised. Its asynchronous and publication paths also contain high-impact
failure modes: a dead worker can permanently strand the session, duplicate
Build requests can queue, two sessions can interleave one batch of outputs, and
a failed rollback can delete the only backup while reporting that recovery
succeeded.

The redesign is a capability-aware guided workbench with four freely navigable
stages:

```text
Import and inspect -> Core setup -> Content enhancement -> Review and publish
```

The central architectural change is one `ContentManifest` contract shared by
the import report, UI readiness state, frozen build plan, post-build CRB
verification, and Viewer page gates. The Builder must always answer three
questions accurately:

1. What was detected in the input?
2. What will survive the build?
3. Which Viewer pages will the generated artifact enable?

Correctness and recoverability precede visual polish. The implementation order
is therefore: privacy prerequisite, manifest contract, worker and publication
state machines, recommendation logic, UI restructuring, then motion and visual
refinement.

### 2026-08-05 implementation amendment

Tasks 0-9 are complete on a branch rebased onto upstream `c192a6a0`. Upstream
PR #102, merged at `18dcef0f`, now supplies the private `createShinyApp()`
publication contract that Task 0 was waiting for. Task 9 additionally supplies
the parent-owned whole-release transaction and recovery boundary. The original
reason for omitting Task 10 therefore no longer applies.

Task 10 is restored as the separately approved activation step. It declares an
exact installed privacy-contract marker only after the real Builder-to-App path
passes, assembles the App inside the coordinator-assigned stage through the
accepted `createShinyApp()` implementation, verifies it before publication,
and leaves final publication exclusively to the parent coordinator. Old or
unmarked package installations remain fail-closed and CRB-only.

## Confirmed product decisions

| Decision | Approved outcome |
| --- | --- |
| Primary audience | First-time Cerebro users who should not need R knowledge |
| Input scope | Complete Seurat support first; define adapter interfaces for future SCE and CRB support |
| Workflow | A guided workbench with a persistent dataset rail and four stages |
| Existing content | Automatically preserve content that passes the real Viewer contract |
| Invalid content | Explain partial, invalid, and unsupported content; never report it as carried |
| Selected analysis failure | Stop before publication; offer retry or explicit skip, then review again |
| Metadata | Automatically classify; include required and useful visual variables, exclude obvious unsafe/noise columns by default, and require review of ambiguous columns |
| Expression backend | Show a plain-language recommendation; expose H5/BPCells/embedded names only in advanced settings |
| Example data | First-class, offline, repeatable inputs that use the same downstream inspection/build path as user files |
| Motion | Use motion only to explain a real state or location change; support reduced motion completely |
| Generated app privacy | Enable App output only for exact installed privacy contract v1; reuse the accepted upstream `createShinyApp()` implementation and keep old or unmarked installations CRB-only |
| Optional analyses | Always opt-in; a recommendation badge never adds work to the frozen plan by itself |

## Original baseline findings

The design was informed by direct source inspection, live use of both basic and
spatial examples, Viewer page inspection, and adversarial base-R
reproductions.

| Area | Current problem | User-visible consequence |
| --- | --- | --- |
| Page structure | Object diagnosis, required export settings, optional analyses, spatial alignment, and publication are all on one long page | A new user cannot tell what matters now or what may be skipped |
| Content reporting | `describe_misc()` mostly checks whether a field is non-empty | Unsupported trajectory, malformed HLA, or incomplete Trekker content can be reported as present even when the page will be absent or fail |
| Expression storage | The Builder never passes `expression_matrix_mode` | Every object uses embedded storage, including datasets better suited to H5 or BPCells |
| Metadata | `add_all_meta_data = TRUE` is implicit | Users cannot see which potentially identifying or large columns are being shared |
| Preview identity | Reduction rows and metadata are joined by position | A shuffled embedding can give every point the wrong colour while the final export is correct |
| Spatial identity | Preview and export use different coordinate-column rules | The alignment preview can disagree with the generated Spatial page |
| Publication options | The action bar is rebuilt with hard-coded defaults | Changing another setting can silently reset app or overwrite choices |
| Build state | Build is not single-flight and editing remains enabled | The page may show a new draft while the worker builds an older snapshot |
| Worker failure | There is no explicit crashed/restarting state | A dead worker can leave the page permanently unusable |
| Batch publication | There is no cross-session destination lock | Two successful builds can produce a mixed batch containing files from each |
| Rollback | Restoration results are not checked before backup cleanup | Recovery can fail, delete the only backup, and still claim success |
| App privacy | The original baseline did not provide a verified private-data publication capability | Task 0 correctly kept Builder App output unavailable until upstream PR #102 landed; Task 10 now performs the separately approved activation |

## Goals

1. Let a new user build a correct Cerebro dataset and app without understanding
   exporter arguments.
2. Cover every Cerebro content type that can originate from a Seurat object,
   including content that is preserved rather than generated by the Builder.
3. Make the final Viewer page set predictable before Build and verifiable after
   Build.
4. Provide safe recommendations for assay, layer, grouping, projection,
   metadata, QC fields, cell cycle, expression backend, and default Viewer
   state.
5. Preserve valid existing analyses automatically while identifying content
   that will be filtered, skipped, or rejected.
6. Keep optional analyses explicit about runtime, network use, dependencies,
   and replacement of existing results.
7. Make preview identity, spatial alignment, and configured colours match the
   final output.
8. Make Build single-flight, cancellable before publication, restartable after
   worker failure, and honest about partial analysis results.
9. Publish a whole release as one locked transaction with conservative crash
   recovery and preserved backups.
10. Treat bundled examples as both user education and end-to-end contract
    fixtures.
11. Meet keyboard, focus, contrast, status-announcement, responsive-layout, and
    reduced-motion accessibility requirements.

## Non-goals for this phase

1. Implementing SCE, CRB, h5ad, h5Seurat, or loom import. The adapter contract
   must allow them later, but this phase implements only Seurat and examples.
2. Supporting arbitrary server-side uploads from untrusted users. The Builder
   remains a local application that may deserialize executable R objects.
3. Implementing every possible analysis algorithm. The Builder must preserve
   all valid supported result shapes and expose only analyses it can run with a
   clear method contract.
4. Adding Viewer support for unsupported trajectory methods. Such content must
   be diagnosed as unsupported and explicitly `filtered` from the CRB rather
   than silently discarded.
5. Hiding expert controls permanently. They remain available under advanced
   disclosure, but they cannot dominate the novice path.
6. Guaranteeing uninterrupted service through process death in the middle of a
   two-rename directory replacement. The contract is durable recovery without
   loss of the old release, not zero-downtime deployment.

## User mental model

The user should only need to understand this sequence:

```text
Add data -> Check what was found -> Confirm how it should appear
         -> Optionally add content -> Review the exact result -> Build
```

The Builder owns the technical sequence:

```text
Source adapter
    -> DatasetProfile + ContentManifest
    -> recommendations + user decisions
    -> frozen BuildPlan
    -> staged CRB/App
    -> post-build manifest verification
    -> locked release publication
```

## Information architecture

### Persistent dataset rail

The left rail remains visible across all four stages and owns multi-dataset
management. Each row shows:

- final dataset order;
- dataset label and cell count;
- `Ready`, `Needs attention`, `Blocked`, or `Reload required` state;
- the number of unresolved warnings or blockers;
- remove and reorder actions.

Adding data opens one consistent source picker. It supports multiple file
selection and a first-class example gallery. A failed example or file load must
remain available; the UI cannot optimistically remove it before the server has
confirmed success.

A rail row represents one output-dataset configuration, not necessarily one
unique source file. The user can duplicate a loaded source to configure another
assay or modality as a separately named Cerebro dataset. The worker may share
the loaded source object, but each row owns independent settings, manifest
compatibility, colours, and output identity.

Removing a dataset with settings or applied spatial work requires confirmation
and offers a session-local Undo action. Keyboard users receive the same action
and confirmation as pointer users.

### Stage 1: Import and inspect

This stage reports object facts and the capability manifest. It does not expose
exporter controls.

For every capability it shows:

- detected source and count;
- structural validity;
- whether it will be preserved, generated, filtered, or rejected;
- the Viewer page or supporting behavior it affects;
- a concrete action when attention is required.

The stage may contain warnings and still be complete. A warning means the user
must confirm a documented consequence before publication; a blocker means no
valid BuildPlan can be created.

The default view is progressive: it shows one overall summary plus every
attention and blocking item. Ready and not-applicable capabilities are grouped
behind "View all detected content"; technical diagnostics require a second
disclosure. An all-content object must not recreate the original card wall.

### Stage 2: Core setup

The Builder presents recommended values first. The novice-facing decisions are:

- dataset name;
- confirmed organism;
- default grouping variable;
- default projection.

The recommender produces an included group set and included projection set,
with one default member of each. The novice confirms the defaults. A secondary
control edits the complete included sets, and validation enforces that each
default remains a member of its included set.

Assay/layer, QC fields, cell cycle, expression backend, and other technical
settings are summarized in plain language. Users may expand advanced controls,
but a safe recommendation must always be available when the input makes one
possible. Metadata is the exception: ambiguous or potentially identifying
columns create a visible `Needs attention` item in the novice path and cannot
be hidden in a skippable advanced panel.

The preview consumes only groups and projections that the current BuildPlan
will export. It is a product preview, not an unrelated object explorer.

### Stage 3: Content enhancement

Only relevant capability modules appear in the main flow. Existing valid
content is listed as automatically preserved. Optional analysis and attachment
modules state:

- what page they enable;
- estimated cost;
- whether they use the network;
- prerequisite content;
- whether they replace an existing method/result;
- the exact consequence of skipping them.

Modules that are not applicable remain visible in the Stage 1 manifest but do
not consume main-stage space.

### Stage 4: Review and publish

The review is generated from the exact candidate BuildPlan. It lists, per
dataset and for the complete release:

- dataset labels and order;
- cells, genes, groups, and projections;
- expression backend and sidecar files;
- included, excluded, and ambiguous metadata columns;
- preserved and newly generated analyses;
- Viewer pages expected to appear;
- spatial sections and histology coverage;
- app-only colour configuration;
- private-data artifacts versus Viewer-bundle assets, with an explicit
  statement that neither label creates an HTTP-public URL;
- estimated runtime and disk size;
- acknowledged warnings;
- output release directory and replacement policy.

Build becomes enabled only when all blockers are resolved and all required
warnings are acknowledged.

## Example library

Examples are permanent source options, not decorative empty-state controls.
They must be small, offline, repeatable, clearly labelled as real or synthetic,
and processed through the same manifest, recommendation, build, and post-build
verification pipeline as user files after adapter loading.

The initial library contains:

| Example | Purpose |
| --- | --- |
| Basic PBMC | Expression, metadata, groups, projections, colours, and basic analysis |
| Spatial multi-section | Coordinate normalization, per-section images, alignment drafts, and Spatial page verification |
| Immune and HLA scenarios | A small scenario set: unified TCR + HLA, TCR without HLA, HLA without TCR, BCR-only, metadata-only conversion, and legacy conversion |
| All-content synthetic | Every supported conditional page plus supporting fields; primarily a contract fixture but also available as an explicitly synthetic exploration example |

Removing an example returns it to the gallery. A load failure also leaves it
available. Examples must not require network analysis to reach a valid built
state.

Example construction does not exercise file probing or deserialization. Each
core example therefore also has at least one serialized test fixture that runs
through `SeuratFileAdapter`; the interactive Example path and real-file path
become identical from `inspect()` onward.

## Input adapter contract

The phase-one adapters are `SeuratFileAdapter` and `ExampleAdapter`. Future
adapters must produce the same source contract without changing downstream UI
or build logic.

Conceptually, an adapter provides:

```text
probe(reference)       -> supported format or actionable rejection
fingerprint(reference) -> provenance/change hint for the original source
load(reference)        -> in-worker object
inspect(object)        -> DatasetProfile + ContentManifest
snapshot(object)       -> immutable session-private storage closure for repeatable builds
prepare(object, plan)  -> exporter-ready source
```

The Example adapter may construct an object rather than read a file, but from
`inspect()` onward it follows the identical path.

File support for this phase remains `.rds`, `.qs2`, and `.qs` when the required
package is installed. Dependency messages must name the format and installation
action before the user chooses the file.

An immutable snapshot includes the serialized object and the complete
transitive storage closure needed to read it. File-backed assays such as
BPCells, HDF5-backed, or DelayedArray-backed data cannot keep references to the
original source or its sibling files. The adapter copies or creates an
immutable private representation of every required backing file and rewrites
the snapshot to use it. Import is blocked when that closure cannot be discovered
and frozen safely. The concrete snapshot format and budget remain engineering
choices; independence from mutable source storage is a product contract.

## Dataset profile and content manifest

### Dataset profile

`DatasetProfile` contains stable facts and valid choices, not current UI
selections:

```text
source identity and fingerprint
object class and format
cell and feature identity
assays and complete exportable layers
metadata column profiles
valid grouping candidates and levels
reductions, dimensions, and cell coverage
spatial sections and normalized coordinates
organism inference with confidence/reason
content manifest
```

Every cell-associated structure is matched by cell barcode. Row position is
never an identity contract.

### Capability entry

Each `ContentManifest` entry contains at least:

```text
capability id
source type and source location
status
disposition
artifact scope: CRB, generated app, or both
count/summary
structural diagnostics
compatibility with selected groups/organism/method
target Viewer pages and page visibility class
required user action
post-build verifier
```

### Status vocabulary

| Status | Meaning |
| --- | --- |
| `checking` | Inspection is still running |
| `valid` | The evidence and prerequisites for the selected disposition are valid |
| `attention` | The user must confirm a consequence or provide missing optional information |
| `blocking` | A correct artifact cannot be built until resolved |
| `not_applicable` | The capability does not apply to this dataset |

Colour is never the only status signal. Every state has an icon, label, and
plain-language explanation.

Status describes only validity/readiness; disposition describes only what the
plan does with the content. They are orthogonal fields. For example, valid
content may be `preserved`, `converted`, `attached`, `filtered`, or
`stored_only`; valid prerequisites for a selected analysis may be `generated`.
Recognized content that the Viewer cannot consume is represented by Viewer
compatibility plus `stored_only`, not by overloading status. Content with no
safe export path is `blocking` with disposition `rejected` until the user
explicitly filters it or changes the plan.

| Disposition | Meaning |
| --- | --- |
| `preserved` | Existing valid content is copied into the artifact |
| `generated` | The frozen plan creates new content |
| `converted` | Existing content is normalized into a supported representation |
| `attached` | A user-selected external item is added after validation |
| `filtered` | Valid content is intentionally excluded by the user or group/method policy |
| `stored_only` | The CRB can retain it, but the current Viewer has no consumer |
| `rejected` | Invalid content prevents the claimed capability from being built |

### Viewer page contract

The manifest models Viewer outcomes directly, but page visibility and page
readiness are separate. A permanent sidebar item is never reported as absent
merely because an optional section is empty.

| Always-visible page | Readiness contract |
| --- | --- |
| Data info | Core dataset identity is required; parameters, technical information, and gene lists are optional sections |
| Projection | At least one verified projection, grouping metadata, and matching cell identities; otherwise the page is visible but invalid and Build is blocked |
| Groups | At least one valid, factorized group; mean-expression content is optional |
| Gene expression | Valid expression backend, features, cells, and compatible projection/group metadata |
| Gene ID conversion | Always visible and backed by its static mouse/human tables; confirmed `mm`/`hg` may preselect the relevant table, while dataset nomenclature does not gate the page |
| Color management | Always visible; valid group levels and any exported cell-cycle states drive the useful controls |
| About | Static and independent of input content |

| Conditional page | Visibility contract |
| --- | --- |
| Marker genes | At least one valid method/group result that the page-specific validator and smoke render can consume |
| Most expressed genes | At least one result compatible with an exported group |
| Enriched pathways | At least one valid method/group result compatible with exported groups |
| Extra material | At least one valid table or plot |
| Immune repertoire | Valid unified repertoire or successfully converted legacy/metadata source |
| Trajectory | At least one Viewer-supported method and structurally valid trajectory |
| Spatial | At least one valid section whose cells and expression overlap the exported dataset |
| Trekker | A structurally valid Trekker payload, not merely a non-empty list |
| HLA & TCR Motifs | A valid TCR chain contract; HLA is optional but provenance is explicit when present |

### Supporting content

Content without its own sidebar page still belongs in the manifest because it
changes another page or the dataset contract. This includes:

- cell-cycle columns;
- mean expression;
- trees;
- gene lists;
- experiment parameters;
- technical information;
- app-only group-level and cell-cycle colour configuration;
- HLA provenance;
- expression backend descriptors.

Trees are `stored_only` in this phase because the current Viewer has no active
`getTree()` consumer. The manifest must not imply that they change the Groups
page.

## Seurat capability coverage

### Phase-one authoring matrix

"Coverage" does not mean that the Builder can generate every scientific result.
It means the UI is explicit about whether content can be configured, generated,
attached, converted, only preserved, or only diagnosed.

| Content | Phase-one Builder action |
| --- | --- |
| Expression, assay, layer, backend | Configure and export |
| Metadata, groups, default group, projections, default projection | Recommend, configure, normalize, and export |
| QC and cell cycle | Detect; safely derive supported QC fields; configure and export |
| App colours and Viewer defaults | Configure generated app; report separately from CRB content |
| Most expressed genes | Preserve or generate, always opt-in for generation |
| Mean expression | Preserve compatible existing content; no separate authoring UI |
| Marker genes | Preserve, import a validated marker table, or generate with an explicit method/replacement policy |
| Enriched pathways | Preserve valid methods; generate Enrichr from an explicit marker method; preserve but do not generate other supported result methods in this phase |
| Trajectory | Preserve structurally valid `monocle2`; diagnose other methods; no trajectory generation or file attachment |
| Spatial coordinates | Preserve valid Seurat image sections; allow an explicit x/y metadata mapping to create a section |
| Histology | Offer a safely extractable Seurat raster as a draft; attach/replace with PNG or JPEG; require external conversion for TIFF/OME-TIFF in this phase |
| Trekker | Preserve and validate existing content; no generation or external attachment |
| Immune repertoire | Preserve unified content; convert metadata-only scRepertoire or legacy content; optionally attach validated BCR/TCR RDS supported by the existing converter |
| HLA typing | Preserve valid content or attach a validated table with explicit provenance and sample/unit mapping |
| Extra tables | Preserve, preview, or attach CSV/TSV/TXT |
| Extra plots | Preserve and preview existing valid plots; no new plot attachment in this phase |
| Trees, gene lists, parameters, technical info | Preserve with an explicit `stored_only` or supporting-content disposition |

Every row has positive, negative, and mixed-content acceptance fixtures. The
Builder guarantees content/page gates, default appearance, and representative
previews; it does not duplicate every runtime plotting control available inside
the Viewer.

### Expression and backends

The Builder supports one chosen assay and one complete logical layer per output
dataset. Split Seurat v5 layers are offered only when they form the complete,
non-overlapping partition accepted by the shared expression resolver.

A multi-assay source can therefore produce multiple dataset entries without
being read from disk again. Duplicate source configurations must receive unique
dataset IDs and labels; the current "same file already added" rejection cannot
prevent this workflow.

Backend recommendation considers:

- matrix dimensions and sparsity;
- projected in-memory and on-disk cost;
- installed H5/BPCells dependencies;
- portability of the release;
- runtime read characteristics.

Recommendation inspection is bounded and may sample; it cannot materialize an
otherwise lazy large matrix merely to choose a backend. It also checks target
filesystem space/permissions and that required packages are available both at
build time and in the intended Viewer runtime.

The novice label describes the result, for example "Lower memory use; suitable
for this dataset." Advanced disclosure shows `embedded`, `h5`, and `bpcells`
and their sidecar consequences. The final review always names the actual mode.

### Metadata

Metadata classification is explicit and reversible:

| Class | Default |
| --- | --- |
| Required core fields: selected groups, QC, cell cycle, and fields required by preserved content | Include |
| Useful Viewer variables with reasonable cardinality | Include, visible in summary |
| Obvious cell identifiers, duplicate constants, and one-value noise | Exclude |
| High-cardinality or potentially identifying fields | Require review |
| Unsupported list/data-frame columns | Block or exclude with an explanation |

Heuristics cannot claim to identify every sensitive field. Review therefore
shows column names, type, unique count, a small non-sensitive summary, and the
reason for each default. The user can include or exclude ambiguous fields.

The canonical `cell_barcode` identity is generated and mandatory. It is not an
"obvious identifier" that the user may exclude. A dependency graph also marks
metadata required by groups, trajectories, immune/HLA mapping, spatial data,
Trekker, and selected analyses; excluding a required field either updates the
dependent selection or blocks the plan with a concrete explanation.

### Groups and default group

Valid grouping candidates include character, factor, logical, integer, and
numeric columns that safely represent categories. Numeric values are not
treated as categories solely because they have few unique values; the manifest
records the reason and requires confirmation when ambiguous.

The user confirms one default group. Other selected groups remain available for
filtering and colour management. Empty strings, `NA`, unused factor levels, and
non-syntactic values are normalized through the same level contract used by the
exporter and generated app.

After confirmation, every categorical source is frozen to an explicit factor
before export. The contract defines level order, converts missing/blank values
to the documented `N/A` level, removes unused levels, and verifies the same
levels after reading back the CRB. Passing logical/integer/numeric values
unchanged to `addGroup()` is forbidden because the current exporter would
derive `NULL` levels.

The selected default group and projection must affect the actual Viewer. The
build writes validated `default_group` and `default_projection` values into a
single runtime contract, and the Viewer is updated to consume them before its
existing fallback. Both defaults must belong to their included sets. An
app-level test proves the initial dataset, group, and projection rather than
only checking configuration text.

### QC and cell cycle

The Builder detects count/UMI and feature/gene columns by structure as well as
name. If they are absent and the selected expression source allows safe
calculation, it offers to calculate them. It does not block solely because the
column names are unfamiliar.

Detected cell-cycle fields are preserved through the dedicated cell-cycle
contract, not merely copied as ordinary metadata.

### Organism and gene nomenclature

Organism inference is labelled as a guess and requires confirmation. Gene
nomenclature is inferred separately and also confirmed when ambiguous. The
phase-one choices are constrained to the package's actual resource matrix:

| Organism | Allowed nomenclatures |
| --- | --- |
| `hg` | `name`, `ensembl`, `gencode_v27` |
| `mm` | `name`, `ensembl`, `gencode_vM16` |

The UI never offers cross-organism Gencode combinations for which no resource
exists.

Every organism-dependent analysis declares both organism and nomenclature.
Mitochondrial/ribosomal calculation cannot assume that human/mouse alone
identifies the gene naming scheme. Symbol, Ensembl, and the two valid Gencode
combinations have positive fixtures; crossed Gencode combinations are rejected
before execution. The Viewer Gene ID conversion page is a separate static
mouse/human lookup tool: confirmed `hg` or `mm` can choose its initial table,
but dataset nomenclature does not limit that page and another organism merely
removes the dataset-specific preselection.

### Reductions and preview

Every selected reduction is verified for dimensions, unique cell identities,
and coverage. PCA behavior mirrors the final exporter exactly; the Builder
cannot offer a selection that will be silently dropped.

Preview data is joined by cell barcode and reports missing, duplicate, or extra
cells. Its groups, projections, labels, and colours are limited to the current
candidate plan so that preview means "what the product will show."

### Existing analyses and content

Valid existing content is preserved automatically. Preservation is
method-aware and group-aware:

- most and mean expression must map to exported groups;
- marker and enriched-pathway methods are named and structurally validated;
- enrichment cannot assume a specific marker method merely because any marker
  object exists;
- only Viewer-supported trajectories are marked valid and Viewer-compatible;
- scRepertoire columns in metadata can be converted through the repository's
  existing extractor rather than silently treated as ordinary metadata;
- HLA input is validated by accepted shape and carries source provenance;
- Trekker validates the fields used by the Viewer;
- existing extra tables and plots are counted, validated, and previewable;
- a newly added table cannot silently replace an existing item with the same
  display name.

Immune source priority is deterministic: an explicit user attachment, then a
valid unified repertoire, then metadata conversion, then legacy BCR/TCR. When
two sources overlap or disagree by sample/chain/barcode, the Builder reports an
attention or blocking conflict rather than silently merging. Validation covers
barcode overlap with the main object, sample-group keys, duplicate sample names,
chain availability, and the TCR-versus-BCR page gates.

Full immune-repertoire readiness and HLA/TCR motif readiness are separate
contracts. Full readiness requires the complete unified scRepertoire columns;
motif readiness requires a parseable barcode plus `CTgene` and `CTaa` TCR
chain. A source may therefore enable the motif page without claiming the full
immune-repertoire page.

HLA validation includes accepted table shape and provenance. HLA sample names
first match immune list names exactly. Donor IDs may collapse those already
matched samples only when every in-scope sample has one unambiguous donor;
aliases never manufacture a match. Unmatched or ambiguous units are reported.
HLA without a parseable TCR chain remains supporting content and does not claim
the HLA & TCR Motifs page.

A valid Trekker payload has unique barcodes; equal-length `x`, `y`, `ux`, `uy`,
and zero-based integer `clusters`; a `celltype` lookup indexed by those cluster
values; equal-length named confidence vectors; matching nested field vectors;
valid Moran/QC/evidence structures; and gene/cell identities compatible with
the main object. Missing fields, wrong lengths, duplicate barcodes, out-of-range
cluster indices, and unknown genes have separate diagnostics.

For trajectory content, each `monocle2` trajectory validates pseudotime, state,
embedding, unique cell identity, and overlap with exported cells. In a mixed
method object, valid `monocle2` content is preserved while unsupported methods
receive their own `filtered` dispositions with the exporter's explicit
diagnostic; the phase-one exporter does not write those methods into the CRB,
so they cannot be called `stored_only`. The whole category is not reduced to
one vague status.

### Optional analyses

Every optional analysis is off by default. Cheap analyses may carry a
"Recommended" badge, but the user must opt in before they enter the frozen
plan. The phase-one executable list is:

- mitochondrial/ribosomal percentages;
- most expressed genes;
- marker genes;
- Enrichr enrichment from an explicitly selected marker method.

GSEA and trajectory results may be preserved when already valid but are not
generated in this phase. Immune metadata conversion and validated HLA/IR
attachments are content-normalization actions, not hidden analyses. Each
executable analysis declares:

```text
supported organisms and assays
required source layer
required prior content/method
estimated cost
network use
output method/key
replacement policy
analysis assay/layer, which may differ from the exported expression layer
```

Re-running an analysis that will replace an existing result requires explicit
confirmation. Enrichment requires selecting the marker method it consumes.
Analysis gating uses the user's confirmed organism, not the initial inference.

If a selected analysis fails, the stage remains unpublished. The result view
offers:

1. retry the failed item together with its complete transitive dependency
   closure; or
2. explicitly remove that item from the plan, return to Review, and build a new
   frozen plan.

Every retry starts from the immutable source snapshot. It never reuses a
partially mutated analysis object from the failed attempt. Dependencies in the
retry closure are recomputed even when they succeeded previously; if the
dependency closure cannot be isolated safely, the coordinator reruns the whole
frozen plan. There is no silent green partial success.

### Spatial data and histology

Spatial inspection and export share one coordinate-normalization helper. The
preview cannot choose the first two numeric columns while the exporter uses a
different named-coordinate rule.

A section requires unique barcodes, non-empty overlap with exported cells,
compatible assay/layer expression, and a valid two-dimensional coordinate
mapping. Zero overlap, partial overlap, duplicate barcodes, and section/output
assay disagreement have explicit diagnostics and blocking policies. A Seurat
object with no `@images` may create a named spatial section by explicitly
mapping two metadata columns; the mapping is never inferred from the first two
numeric columns.

Each section owns separate state:

```text
source image identity
source pixel dimensions
encoded/display dimensions
coordinate bounds
transform draft
applied transform
coverage diagnostics
```

Coordinate extents use source dimensions, never downsampled display
dimensions. Grayscale, grayscale-alpha, RGB, and RGBA images retain valid
channel structure through rotation and flips.

Changing dataset or section with an unapplied draft prompts to Apply or
Discard. An applied section can be reopened and edited. Multi-section matching
supports per-file assignment and never reports full success when only a subset
was applied. Missing histology is a warning when spatial coordinates remain
valid, not automatically a blocker.

When a Seurat image object contains a safely extractable raster, it is offered
as the initial section draft and still passes the same source/display-dimension
and alignment checks. External PNG/JPEG replacement is supported. TIFF and
OME-TIFF are detected with plain conversion guidance rather than being accepted
and failing later.

## Build state model

### Per-dataset state

```text
loading -> ready | needs_attention | blocked
ready/needs_attention -> reload_required
reload_required -> loading
```

Loading, profiling, and group-level extraction must all succeed before the
source is registered. The worker then writes an immutable session-private
snapshot of the object and its complete file-backed storage closure through
temporary sibling paths and atomic renames. It verifies that reopening the
snapshot no longer depends on the original source or backing files. Failure
removes the candidate object and partial snapshot immediately.

### Worker state

```text
starting -> ready -> running -> ready
                    |          ^
                    v          |
                  failed -> restarting
```

The main Shiny process owns settings, manifest snapshots, immutable-snapshot
registry, source provenance, and UI state. The worker owns large live R objects
and expensive execution. If the worker dies, settings remain available and the
replacement worker reloads the immutable session snapshot; it never silently
re-reads the original source path. Explicit "Refresh from source" creates a new
snapshot and re-runs inspection.

Every Build starts from a fresh load/clone of the immutable snapshot. Analyses
cannot mutate the worker's canonical inspected object, and retry cannot inherit
half-written `@misc` content from a failed prior attempt. Replacing or deleting
the original serialized object or any original BPCells/HDF5/DelayedArray
backing file cannot change or break a registered snapshot. Snapshot creation
cost, disk use, owner-only permissions, and cleanup are visible operational
requirements. If the complete storage closure cannot be frozen safely, loading
fails before the dataset becomes Ready.

### Request classes

Requests are separated by semantics:

| Class | Examples | Queue behavior |
| --- | --- | --- |
| Replaceable query | preview, coordinates | Latest generation may replace an older pending query for the same dataset and key |
| Persistent command | save alignment, remove dataset | FIFO; never silently replaced |
| Build command | build and publish | Single-flight; a second Build is disabled rather than queued |

`align_all` is a persistent per-dataset command, not one global latest-wins
query.

Review and Build establish a queue barrier: every prior persistent command must
finish and be acknowledged before the plan is frozen, while replaceable queries
are cancelled or discarded. Applying alignment and immediately pressing Build
therefore cannot omit the just-saved state.

### Frozen build

Pressing Build creates an immutable plan and records its visible timestamp or
revision. Editing controls enter a frozen state. Progress identifies the
dataset and stage currently running. Cancellation requests a worker interrupt,
waits a bounded grace period, terminates an uncooperative worker, then restarts
it from immutable snapshots. The parent-owned coordinator cleans or quarantines
the registered stage. Once the short final publication transaction begins,
cancellation is disabled until it completes or fails safely.

If the user changes the draft after a completed build, the result is labelled
as belonging to the prior revision until a new review and build occur.

## Publication and recovery

Publication is owned by a `PublicationCoordinator` in the main Shiny process
(or a dedicated transaction executor controlled by it), never by the disposable
analysis worker. The coordinator owns the build ID, stage registry, lock,
journal, expected prior-release identity, final rename, and recovery result. The
worker may generate and verify artifacts only inside the assigned stage.

Before dispatching work, the coordinator allocates and records a build ID,
owner token, immutable-snapshot identities, and stage path. Worker crash,
cancellation, browser-session close, and process restart therefore leave a
known stage that can be removed or conservatively quarantined rather than an
anonymous temporary directory.

### Release unit

The user chooses a parent directory and release name. The Builder publishes one
release directory rather than several unrelated top-level targets:

```text
<parent>/<release-name>/
├── 01-<dataset>.crb
├── expression sidecars when requested
├── cerebro_app/                 # optional
├── .cerebro-builder-release-v1 # parent-written ownership record
└── build-report.json
```

This layout makes the complete set of CRBs, sidecars, app, and report one
transactional publication unit. The Review screen shows the exact paths and
estimated duplication when both raw exports and an app are requested.
`build-report.json` is private release metadata and is never registered as an
HTTP resource by the generated app. It is a portable, redacted report: it may
name artifact-visible datasets, methods, and metadata columns, but it excludes
source absolute paths, host/PID, lock/backup paths, raw values, and private
diagnostics. The parent derives, atomically writes, rereads, and validates it
from the frozen plan plus verified result before the ownership record is
written. Operational diagnostics remain owner-only inside the sibling control
directory and are not copied into the release.

The hidden ownership record is smaller and stricter than the user-facing
report. The parent writes it only after verifying the complete stage. Its first
line is exactly `CEREBRO_BUILDER_RELEASE_V1`; subsequent sorted lines use
`F<TAB>path` or `D<TAB>path` for every recursively owned payload entry. The
record file itself is an implicit fixed member and is not self-listed. Its
bytes and parsed member set are part of the captured prior-release identity and
are checked again under the publication lock. On a later build it allows an old
optional App or removed dataset to be retired without treating it as foreign.
Any unlisted entry anywhere under the release, malformed record, symbolic link,
or otherwise unverifiable occupant blocks replacement and remains untouched. A
legacy release without this record may be replaced in place only when its
complete existing topology remains in the new plan; shrinking it requires a
prior record-bearing publication or explicit manual resolution.

### Transaction sequence

```text
drain persistent commands and freeze plan + expected prior-release identity
  -> acquire the canonical release owner-token lock
  -> coordinator registers build ID, same-filesystem private stage, and durable prepared record
  -> worker builds and validates artifacts in that stage
  -> parent rereads CRBs and independently reverifies any generated App
  -> validate the exact worker-payload target set
  -> parent writes and rereads any portable build report
  -> parent writes and rereads the exact release-ownership record
  -> validate the exact final target set and recursive stage identity
  -> compare-and-swap the actual prior-release identity
  -> atomically write durable transaction phase record
  -> move prior release to a unique backup
  -> rename stage to final release
  -> verify final location
  -> retire backup and transaction record
  -> release the owner-token lock
```

One lock covers the whole canonical release target. Two sessions cannot publish
to the same release concurrently. The lock records an owner token, host, PID,
start time, and target. Stale-lock recovery is conservative: a live or
unverifiable owner is never overridden automatically.

The frozen plan records the identity of the release reviewed by the user. After
acquiring the lock, publication compares that identity with the current target.
If another session published in the meantime, the second publisher stops and
requires a new Review; the lock prevents mixed files, while this compare-and-
swap check prevents a valid but stale build from silently overwriting newer
work.

Stable control state lives outside the final directory at a validated sibling
path such as `<parent>/.<release-name>.cerebro-control/`. It contains owner-only
stage, backup, lock, transaction, and private-diagnostic locations. Renaming the
release cannot move or hide the journal. Each journal update is written to a
temporary sibling and renamed atomically, and records target, stage, backup,
owner, expected/observed identities, and transaction phase.

Every move result is checked. If restoration fails, the backup remains at its
exact reported path and cleanup must not delete it. The UI never says
"restored" unless final existence and identity checks pass.

A durable transaction record allows the next Builder start to identify an
interrupted publication when the user reselects the same parent/release target;
the application does not scan arbitrary filesystem locations. Recovery either
restores the recorded backup or offers a precise, non-destructive manual action.
If an unrelated process has occupied the final path, recovery stops without
deleting it. Unknown files block replacement and remain untouched.

Canonical-target, containment, symbolic-link, case-folding, Windows device-name,
and alias rules must be implemented and verified against the current branch.
Unpublished pull-request code is outside this design's implementation scope.
Time elapsed alone never proves that a lock is stale.

### Generated app privacy contract and activation

The private-data capability has landed upstream through PR #102, and Task 10 is
the approved activation step. It must not infer safety from a package version,
file presence, UI value, or BuildPlan claim. The installed package namespace
must expose one exact eager, locked, non-active integer marker with value `1L`.
The marker is declared only in the final activation commit, after the dormant
Builder App path and its real integration tests pass. Missing, lazy, active,
function-valued, unlocked, or differently valued markers keep App output
disabled.

Privacy contract v1 must:

- keep CRB, H5, and BPCells content outside HTTP resource mappings;
- keep `spatial-assets/` outside generic HTTP resource mappings; configured
  images are read through the validated server-side allowlist and rendered
  without making their files directly downloadable;
- avoid the legacy `/data` path so an older still-running app cannot map the
  new private files after an in-place upgrade;
- preserve the frozen backend descriptor contract;
- retain the app-level publication lock and recovery behavior;
- pass pristine-process and upgrade-path HTTP privacy tests.

The Builder release lock and `createShinyApp()`'s internal app lock protect
different boundaries and both remain necessary.

The disposable worker may call `createShinyApp()` only with already verified
CRBs and `result_dir = <assigned-stage>/cerebro_app`. That is stage assembly,
not final publication. It uses `overwrite = FALSE`, returns typed diagnostic
evidence, and never receives authority to move the release into the user
target. The coordinator freezes the App expectation before dispatch, then the
parent independently rereads and verifies the current staged App rather than
trusting worker evidence. Only after that does it add parent-authored files,
write the ownership record, perform the Task 9 transaction, and remap
`app_dir` to the final release path.

Generated-app options are part of Review rather than invisible defaults:

- `show_upload_ui` defaults to `FALSE` for a fixed, manifest-verified release;
  enabling it is advanced and makes clear that guarantees cover bundled
  datasets only;
- the initial dataset follows the approved rail order or an explicit pinned
  selection; the frozen state distinguishes automatic from explicit even when
  both currently name the first row, and `crb_pick_smallest_file` cannot
  silently override either choice;
- trivial-metadata exclusion agrees with the Builder metadata policy;
- welcome message, point size, and `variable_to_compare` are app-appearance
  settings with validated defaults and advanced controls;
- host, port, request size, display mode, and launch behavior remain deployment
  settings and are serialized safely.

The unpublished internal fields formerly called `public_assets` and
`public_asset_claims` become `viewer_bundle_assets` and
`viewer_bundle_asset_claims` before App activation. They describe eligibility
for inclusion in a Viewer bundle, not HTTP exposure. Contract v1 has no
user-configurable HTTP-public asset class, and these fields can never create a
resource mapping. The manifest and Review report CRB capabilities and
generated-app configuration as separate artifact scopes. Colours, configured
spatial images, upload controls, and Viewer defaults cannot be presented as
embedded CRB content unless they actually are embedded in the CRB.

## Result states and error language

The final result uses four top-level states:

| State | Meaning |
| --- | --- |
| Success | Every selected item succeeded, post-build verification passed, and the complete release was published |
| Needs decision | Nothing new was published; one or more selected optional items failed, and the user may retry or explicitly remove them |
| Failure | No successful release was declared; the report gives the exact final, prior-release, stage, backup, and journal state |
| Recovery required | Publication reached an indeterminate or partially completed transaction state; no cleanup is attempted until a verified recovery action is chosen |

Warnings from Seurat, analysis functions, image processing, export, and app
creation are captured with source and dataset identity. Expected informational
warnings may be acknowledged; structural or privacy warnings are blockers.

Error text answers:

1. what failed;
2. which dataset/content item was affected;
3. whether anything was published;
4. whether the old release remains available;
5. what the user can do next.

A successful novice workflow does not end with an R command. The result page
provides `Open generated app`, `Reveal release folder`, and `Copy release
path/report` actions. If the app cannot be launched on the current platform, the
UI gives a clickable folder action and plain instructions without assuming the
user will type `shiny::runApp()`.

## Visual and interaction design

### Motion

Retain short, state-bearing motion, normally 160-220 ms:

- an added dataset locating itself in the rail;
- active dataset and stage changes;
- preview cross-fade after validated data arrives;
- capability transition from checking to valid/attention/blocking;
- alignment draft becoming applied;
- Build progress and final result transition.

Remove motion that does not carry meaning:

- example buttons disappearing before load success;
- whole-page staggered card entrances;
- hover elevation on every card;
- overshoot or pop effects unrelated to state.

`prefers-reduced-motion` removes every transition without removing information
or delaying state updates.

### Responsive behavior

The desktop dataset rail collapses into a compact current-dataset switcher on
narrow screens. An adjacent Dataset Manager sheet retains status, warning
count, reorder, duplicate, remove, and Undo actions with full keyboard access.

The primary stage action is in normal document flow at the end of the stage;
there is no fixed mobile action bar covering content. A one-line sticky status
summary may remain, contains no exclusive action, and has explicitly reserved
layout height. The design is verified at wide, medium, 390-pixel, and 320 CSS
pixel widths, plus 400% zoom/reflow, using real long labels and error text.

### Accessibility

Required behavior includes:

- WCAG AA text contrast at actual font sizes;
- keyboard access to dataset selection, ordering, file picking, modules, and
  publication;
- `role="dialog"`, `aria-modal`, focus trap, Escape handling, and focus restore
  for file and confirmation dialogs;
- `aria-live` or status semantics for load, validation, progress, errors, and
  completion;
- throttled progress announcements that do not speak every low-level log line;
- visible focus states not dependent on hover;
- status icons and text in addition to colour;
- `aria-current` for the current dataset and stage, with consistent landmarks
  and heading hierarchy;
- Plotly previews with an accessible data summary/table, and colour controls
  that expose each level name plus hexadecimal value;
- no hidden action available only on pointer hover.

All object, table, image, and output paths use one consistent picker. Manual
path entry may remain in advanced settings.

All repository and Builder UI text remains English for the upstream project.
Plain language refers to avoiding R and storage jargon, not translating the
application into a second UI language in this phase.

## Code organization

`inst/builder/app.R` becomes a small bootstrap and composition file rather than
the owner of all state and markup. Proposed responsibility boundaries are:

```text
inst/builder/
├── adapters.R          # source contracts and phase-one adapters
├── prerequisite.R      # installed private-App contract gate
├── manifest.R          # DatasetProfile and ContentManifest rules
├── recommend.R         # deterministic novice defaults
├── plan.R              # immutable BuildPlan and full revalidation
├── worker.R            # worker lifecycle and request protocol
├── build.R             # analysis/export/post-build verification
├── app_bundle.R        # frozen createShinyApp arguments and App read-back
├── report.R            # portable redacted plan/result report
├── coordinator.R       # parent-owned build/stage/publication orchestration
├── publish.R           # release lock, journal, replacement, recovery
├── spatial.R           # shared coordinate and image-transform contracts
├── ui/
│   ├── dataset_rail.R
│   ├── inspect_stage.R
│   ├── core_stage.R
│   ├── enhance_stage.R
│   ├── review_stage.R
│   └── build_status.R
└── app.R
```

Exact file names may change during implementation planning, but these
responsibilities remain separate. Pure rules have no Shiny dependency and are
the primary test surface. UI code consumes typed state rather than reimplementing
readiness checks. There must be one readiness validator, not separate rail,
action-bar, and plan interpretations.

## Verification strategy

### Pure contract tests

Test adapters, manifests, metadata classification, recommendation logic,
backend choice, group levels, plan validation, path handling, coordinate
normalization, and image transforms without a Shiny session.

Boundary fixtures include:

- shuffled reduction and metadata row order;
- missing, duplicate, and extra cell barcodes;
- only-PCA and PCA-plus-non-PCA objects;
- logical and numeric categorical metadata;
- blank, `NA`, non-syntactic, and unused group levels;
- non-standard QC column names and safely derivable QC;
- symbol, Ensembl, and Gencode nomenclature;
- valid and crossed organism-by-nomenclature combinations;
- marker methods other than `cerebro_seurat`;
- unsupported and mixed trajectory methods;
- unified, metadata-only, legacy, overlapping, TCR-only, BCR-only, and HLA-only
  immune inputs;
- matched, unmatched, aliased, and ambiguous HLA analysis units;
- malformed HLA and Trekker payloads;
- Visium/FOV coordinate-column variations;
- zero/partial spatial cell overlap and spatial/output assay disagreement;
- multi-section objects;
- grayscale, grayscale-alpha, RGB, and RGBA images;
- downsampled images whose coordinate extent must retain source dimensions.

### Content end-to-end tests

Build every example to a real CRB, read it back, call the relevant getters, and
compare the resulting page-gate manifest with the pre-build manifest. The
all-content fixture covers every supported conditional Viewer page and
supporting content field.

The tests assert not only that files exist, but that unsupported/invalid content
has the documented disposition and valid content remains structurally readable.
Each conditional module has a page-specific structural validator and at least
one smoke render; shallow getter success alone is insufficient.

The artifact matrix covers:

```text
{embedded, h5, bpcells}
  x {plain, histology, Trekker}
  x {CRB only, generated app}
```

After post-export augmentation, stage movement, release publication, and app
copying, every backend descriptor, relative sidecar location, lazy handle, and
runtime dependency is verified.

### Worker and state-machine tests

Cover:

- load failure after object deserialization without memory leak;
- real callr worker SIGKILL during query, analysis, export, and validation;
- restart from immutable snapshot without re-reading the original source;
- file-backed snapshot restart after the original object and backing files are
  replaced or deleted;
- failed analysis followed by retry from pristine content;
- duplicate Build clicks;
- editing or removing a dataset during a frozen Build;
- persistent alignment command immediately followed by Review/Build;
- cancellation before publication;
- uncooperative cancellation that requires terminate/restart;
- retry and explicit skip after optional-analysis failure;
- per-dataset persistent alignment commands;
- stale replaceable preview rejection;
- publication settings surviving unrelated UI updates.
- browser/session close during Build without an anonymous stage.

### Publication adversarial tests

Use multiple real R processes where concurrency matters. Cover:

- two publishers targeting the same release;
- two sequential publishers whose expected prior-release identities conflict;
- lock owner-token mismatch;
- stale and unverifiable locks;
- canonical aliases, symlinks, case collisions, and Windows reserved/device
  paths;
- failure while moving the old release;
- failure while publishing the new release;
- failure again during restoration;
- process termination between transaction phases;
- retained backup and journal discovery on the next start;
- atomic journal-write interruption and recovery on reselecting the target;
- a foreign occupant taking the final path during recovery;
- unknown content in lock, stage, and backup directories;
- final artifact identity after recovery.

### Generated app and privacy tests

Build and boot a hermetic app without CerebroNexus installed. Verify:

- CRB/H5/BPCells requests are not served;
- Builder-normalized histology remains embedded in the CRB, keeps its
  transforms, renders, and has no HTTP file URL;
- direct `createShinyApp()` external spatial files remain byte-identical inside
  the bundle, render through the server-side allowlist, and are not directly
  downloadable;
- a still-running legacy app `/data` mapping cannot reach a newly published
  private directory;
- frozen backend plans match staged files;
- all expected Viewer page gates match the post-build manifest;
- the configured initial dataset/group/projection and `show_upload_ui` behavior
  match Review;
- group-level and cell-cycle palettes shown in Review match the generated app's
  Color management controls.

### Browser and accessibility tests

App-level browser tests perform the complete flow for Basic, Spatial,
Immune/HLA gate variants, All-content, and Invalid-content examples:

```text
choose Example
  -> inspect manifest
  -> confirm recommendations
  -> configure one enhancement
  -> review
  -> build
  -> inspect success or controlled failure
```

Additional checks cover multi-file add, dataset reorder, option persistence,
dialog focus, keyboard navigation, live status regions, reduced motion, and
narrow-screen action accessibility. Dataset switching explicitly exercises
conditional page insertion and removal (`has page -> no page -> has page`) with
no stale reactive state or cache. Visual snapshots cover empty, loading, ready,
attention, blocking, building, decision-required, failure, recovery-required,
and success states.

## Implementation sequence

This is sequencing guidance, not the task-by-task implementation plan.

1. **Enforce the prerequisite boundary.** Task 0 keeps Builder App generation
   disabled unless the installed package exposes exact privacy contract v1.
2. **Introduce the manifest contract.** Add adapters, profile/manifest data
   structures, Viewer page gates, and all-content fixtures while retaining the
   existing UI temporarily.
3. **Repair correctness foundations.** Fix cell-barcode joins, coordinate
   normalization, image dimensions/channels, metadata/group validation, and
   method-aware content preservation.
4. **Replace worker and publication state machines.** Add single-flight Build,
   crash/restart semantics, request classes, release locks, durable journals,
   and conservative recovery.
5. **Add recommendations and review.** Implement metadata classification,
   backend recommendation, QC derivation, default group/projection, and exact
   pre/post-build reports.
6. **Activate private generated Apps.** After the accepted upstream contract
   and parent transaction are both present, add staged App assembly/read-back,
   the release ownership record, and real privacy integration as Task 10; add
   the installed marker only after that dormant path passes. Do not rewrite
   `createShinyApp()` or give the worker final publish authority.
7. **Restructure the UI.** Implement the dataset rail and four stage modules,
   then add unified pickers, responsive behavior, accessibility, and
   state-bearing motion.
8. **Complete end-to-end verification and documentation.** Run the complete
   precheck, browser flows, privacy upgrade scenario, pkgdown build, and an
   adversarial review before any push.

Each behavior change follows a red-green-refactor cycle. Commits are organized
by logical contract rather than by file type.

## Implementation-plan decisions still to quantify

The following are engineering parameters, not unresolved product behavior:

- matrix-size and dependency thresholds for backend recommendations;
- exact metadata cardinality thresholds and sensitive-name hints;
- immutable snapshot format, storage budget, and cleanup age;
- worker progress transport, interrupt signal, and cancellation grace period;
- release journal serialization and conservative lock-owner evidence policy;
- disk-size estimation accuracy and when to compute it;
- exact UI module file names.

The implementation plan must turn each into a tested, deterministic rule before
code changes begin.

## Acceptance criteria

The redesign is complete only when all of the following are true:

1. A novice can build each bundled example without entering an R field name or
   storage-backend term.
2. The import manifest, frozen review, and post-build CRB verification agree on
   every supported Viewer page and distinguish permanent, conditional, and
   limited page states.
3. No unsupported, malformed, or filtered content is reported as successfully
   carried.
4. Preview cells, groups, projections, group/cell-cycle colours, and spatial
   coordinates match the generated artifact by identity.
5. The user can inspect and change metadata inclusion before publication.
6. The actual expression backend and sidecars match the Review screen.
7. Selected optional-analysis failure cannot produce a green published result.
8. Confirmed grouping variables are read back with the exact documented factor
   levels, including missing and blank values.
9. Default dataset, group, and projection visibly control the launched Viewer.
10. A dead worker can be restarted without losing settings; Build retries start
    from an immutable snapshot containing the complete file-backed storage
    closure, recompute the failed item's full dependency closure (or the whole
    frozen plan), and never inherit partial analysis mutations or mutable source
    dependencies.
11. Build is single-flight, waits for persistent commands, and draft edits
    cannot alter the frozen task.
12. Two concurrent sessions cannot mix one release or overwrite a release that
    changed after Review.
13. Restoration failure preserves the backup and reports its exact location;
    indeterminate publication enters `Recovery required` without destructive
    cleanup.
14. The generated app exposes no CRB, H5, or BPCells data over HTTP and exposes
    no direct spatial-asset file URL; Builder-normalized embedded histology and
    direct `createShinyApp()` external images each render through their
    validated path, with external image bytes preserved.
15. Switching App output off or removing a dataset retires only members proven
    Builder-owned by the prior release record; every foreign occupant at any
    depth blocks replacement and remains untouched.
16. Example, basic, spatial, immune/HLA, all-content, invalid-content,
    concurrency, crash, and browser fixtures all pass.
17. Every phase-one content type has the documented preserve/generate/attach/
    convert/diagnose behavior and artifact scope.
18. Wide, medium, 390-pixel, 320-pixel, and 400%-zoom layouts keep every action
    reachable without a fixed bar covering content.
19. Keyboard, focus, status announcement, contrast, and reduced-motion checks
    pass.
20. Success provides click-based Open App, Reveal Folder, and Copy Path/Report
    actions without requiring an R command.
21. `scripts/precheck.sh`, package tests, R CMD check, and pkgdown complete with
    no new error or warning.
