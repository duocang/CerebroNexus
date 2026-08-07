# Builder to Viewer capability matrix

Date: 2026-08-07

This matrix is based on the active Viewer UI/server modules, CRB accessors,
page-visibility contract, Builder profile/state/BuildPlan, and generated-App
configuration. It deliberately distinguishes frozen build decisions from
temporary Viewer interaction.

Classification means:

- **Configurable**: a build-time choice must be frozen.
- **Preview only**: bounded evidence helps the user, but no choice is needed.
- **Summary only**: say what will be present without exposing internals.
- **Needs attention**: render only when the user must resolve a real problem.
- **Internal only**: keep in validation, logs, or runtime state.

## Identity and core data

| Domain / content | Source | Viewer consumer and use | Class / Builder interaction | Preview and validation | Persistence and legacy fallback | Priority | Current status / real gap |
|---|---|---|---|---|---|---|---|
| Dataset name and experiment name | Builder dataset settings; Seurat/CRB identity | Dataset selector and Data info | Configurable; rename in Core | Short text; unique output filenames | BuildPlan, CRB experiment name, App dataset config; old artifacts keep their label | P0 | Closed |
| Dataset order and initial dataset | Builder dataset rail and Review | App selector order and first loaded dataset | Configurable; reorder plus one initial dataset | Validate initial is in ordered set | App config; legacy uses first available dataset | P0 | Closed |
| Organism | Profile inference or user text | Data info, gene ID conversion, analysis helpers | Configurable; editable selectize | Validate supported analysis prerequisites, allow custom text | CRB experiment field; old value preserved | P0 | Closed |
| Export/analysis dates | CRB experiment metadata | Data info displays export provenance | Summary only | No raw payload preview | Preserved or generated during export | P2 | Optional provenance summary is still absent |
| Parameters and technical information | Seurat `misc` / CRB experiment payload | No active Viewer module currently consumes it | Internal only | Validate safely; no normal UI | Existing CRB payload preserved | P3 | `analysis_info` is not wired into active Viewer UI/server |
| Source and local path | Import record and source snapshot | No Viewer page; build/reproducibility only | Internal only | Fingerprint and safe basename only | Build report/log; never expose full local path | P0 | Closed |
| Cell and feature identity | Matrix names, metadata row names, reductions | All data access depends on stable identity | Internal only; Needs attention on mismatch | Bounded counts/coverage, never full ID lists | Frozen artifact identity and CRB validation | P0 | Closed |
| Cell/gene counts | Profile | Dataset summary and Data info | Summary only | Exact counts | BuildPlan/CRB derived values | P0 | Closed |
| Expression assay and layer | Seurat assays/layers | Gene expression, Spatial, Trekker | Configurable under Advanced settings | Validate existence, dimensions, cell/feature identity | BuildPlan exporter selection; legacy uses existing CRB expression | P0 | Closed |
| Expression storage backend | Builder setting | Viewer expression getter and private sidecars | Configurable under Advanced settings | Estimate size and validate sidecar contract | BuildPlan plus CRB/App private assets; legacy embedded fallback | P0 | Closed |
| Metadata preservation | Seurat metadata and effective metadata policy | Groups, coloring, filtering, tables | Summary only for normal safe columns; Needs attention for ambiguous/blocking columns | Bounded samples, type, missingness, cardinality | Effective policy filters CRB metadata; legacy recommendation fallback | P0 | Policy is closed; Core now reports retained/excluded truthfully |
| Gene nomenclature / identifiers | Profile recommendation and user setting | Gene lookup and analysis prerequisites | Summary only; Needs attention when ambiguous | Identifier examples must remain bounded | BuildPlan/export setting; source feature IDs stay intact | P2 | Compact summary remains absent |

## Annotations and visual spaces

| Domain / content | Source | Viewer consumer and use | Class / Builder interaction | Preview and validation | Persistence and legacy fallback | Priority | Current status / real gap |
|---|---|---|---|---|---|---|---|
| Viewer Groups and levels | Eligible metadata columns | Groups, Projection coloring, filters | Configurable; include set and one default | Bounded distribution and metadata preview | Per-dataset settings, CRB groups/main group; first eligible legacy fallback | P0 | Closed |
| Group colours | Metadata levels and palette | Projection, Groups, Color management | Configurable per dataset/group/level | Color swatches, bounded searchable level list | BuildPlan, CRB/App palette; Viewer may still change colors at runtime | P0 | Closed |
| Non-Group metadata | Metadata policy | Data info and coloring candidates | Summary only | Show retained/excluded; no false Group implication | Physically filtered during CRB build | P0 | Corrected in this slice |
| Cell-cycle annotation | Metadata candidates; CRB `cell_cycle` | Color management and trajectory grouping | Configurable only when a valid candidate exists | Conservative phase-name and categorical-type check | Per-dataset setting → BuildPlan → exporter; old items keep an empty fallback | P1 | Closed end to end |
| Gene lists | Seurat `misc$gene_lists` / CRB payload | No active Viewer module consumes it | Internal only | Validate bounded names/counts | Existing exporter preserves payload | P3 | Do not create an ineffective control |
| UMAP, t-SNE, PCA and other valid 2D reductions | Seurat reductions / CRB projections | Projection page | Configurable include set, default, initial point size | Deterministically capped coordinate preview | Per-dataset settings, CRB pruning, App default; first available legacy fallback | P0 | Closed |
| Unsupported or malformed reductions | Profile validation | No usable Viewer projection | Preview only or Needs attention if none remain | Dimensions, finite values, cell identity | Excluded from output; no fake selection | P0 | Closed |
| Trajectory method/name, states, pseudotime, edges | Seurat `misc$trajectories` / CRB | Trajectory page | Configurable for supported monocle2 trajectories | Bounded points/edges and coverage | Per-dataset settings, CRB pruning, App default; first available fallback | P0 | Closed |
| Unsupported trajectory methods | Profile catalog | Not consumed by this Viewer | Preview only with disabled reason | Method/shape compatibility | Preserved only where existing contract permits; never selected | P1 | Closed |
| Runtime zoom, selection, hover, opacity, axes and filters | Viewer session | Projection/trajectory/spatial interaction | Internal only | None in Builder | Session-local only | — | Intentionally remains in Viewer |

## Analysis results

| Domain / content | Source | Viewer consumer and use | Class / Builder interaction | Preview and validation | Persistence and legacy fallback | Priority | Current status / real gap |
|---|---|---|---|---|---|---|---|
| Marker genes | `misc$marker_genes` / CRB marker tables | Marker genes page by method and group | Summary only for existing results; optional recompute when supported | Method count, group coverage, table count; validate table/group shape | Preserved or generated according to manifest; absent page stays hidden | P0 | Compact Core catalog and Review summary added |
| Most expressed genes | `misc$most_expressed_genes` | Most expressed genes page | Summary only; optional recompute | Group coverage and table count | Preserved/generated through existing analysis pipeline | P0 | Compact catalog and Review summary added |
| Mean expression | `misc$mean_expression` | Metric switch inside Most expressed genes page | Summary only | Group coverage and table count; requires compatible most-expressed content | Preserved with source; no separate Viewer page | P0 | Compact catalog added with correct shared-page label |
| Enriched pathways | `misc$enriched_pathways` | Enriched pathways page by method/group | Summary only; optional recompute when prerequisites are met | Method/group/table counts and marker dependency | Preserved/generated through existing analysis graph | P0 | Compact catalog and Review summary added |
| Invalid/incompatible analysis result | Bounded content evidence | No safe Viewer page | Needs attention; friendly action, no diagnostic codes | Explain recompute/review source only | Rejected by manifest until fixed | P0 | Added to compact catalog |
| Trees | CRB `trees` / `getTree()` | No active Viewer call site | Internal only | Validate/preserve only | Existing payload may be retained | P3 | Do not expose until Viewer consumes it |

## Spatial, Trekker, immune, and HLA

| Domain / content | Source | Viewer consumer and use | Class / Builder interaction | Preview and validation | Persistence and legacy fallback | Priority | Current status / real gap |
|---|---|---|---|---|---|---|---|
| Spatial sections and coordinates | Seurat images/coordinates or CRB spatial payload | Spatial page | Summary only; section selection becomes configurable only with a frozen contract | Section count, coordinate coverage, bounded preview | Existing CRB content preserved | P1 | Closed with bounded section, readiness, and tissue-image counts |
| Tissue image and alignment transform | Native image file plus section coordinates | Spatial page overlay | Configurable in shared alignment workbench | Bounded image/point preview and transform checks | Written to CRB histology fields; points-only fallback | P0 | Closed |
| Spatial section include/default | Multiple spatial sections | Viewer section switcher | Configurable only after settings/CRB/App semantics exist | Section catalog | No frozen contract yet; Viewer currently chooses available section | P2 | Real contract gap; no ineffective UI added |
| Spatial Moran's I for current gene | Viewer runtime calculation | Spatial page | Internal only | Runtime gene-dependent result | Session only | — | Intentionally remains in Viewer |
| Trekker paired spaces, clusters and QC | CRB Trekker payload | Trekker page | Summary only | Bounded transcriptome/physical-space coverage | Existing CRB payload | P1 | Closed with cell, cluster, field, and coverage counts |
| Trekker histology/alignment | Trekker coordinates plus native image | Trekker page | Configurable in the same alignment workbench | Shared bounded preview | Written to Trekker extras; points-only fallback | P0 | Closed |
| Trekker upstream Moran table/evidence images | CRB Trekker payload | Trekker result panels | Preview only | Presence/count only | Existing payload preserved | P2 | Closed with optional compact counts |
| Immune repertoire, TCR/BCR chains and clonotypes | Unified/metadata/legacy immune candidates | Immune repertoire page | Summary only when one source is clear | Chain types, record count, barcode/sample coverage | Selected/converted source in BuildPlan and CRB | P1 | Closed for an unambiguous selected source |
| Multiple plausible immune sources/mappings | Profile candidates | Determines immune content source | Configurable only when choices are genuinely ambiguous | Equivalence, barcode and donor/sample coverage | Versioned per-dataset source decision; legacy priority fallback | P1 | Conditional selector remains a real gap |
| Immune identity mismatch | Candidate overlap evidence | Blocks reliable immune page | Needs attention | Friendly barcode/source action; no readiness codes | Build blocked/rejected until resolved | P1 | Closed with actionable user-facing messages |
| HLA typing and coverage | HLA payload plus sample/donor mapping | HLA/TCR QC and motif module | Summary only | Typing coverage and mapping compatibility | Preserved/converted into CRB | P1 | Closed with sample, locus, and allele counts |
| HLA & TCR motif readiness | TRA/TRB plus optional HLA | Conditional HLA & TCR Motifs page | Summary only | TCR readiness; HLA is not a hard page prerequisite | Page contract gates on compatible TCR data | P1 | Closed with a joint page-readiness statement |

## Extra material and generated-App settings

| Domain / content | Source | Viewer consumer and use | Class / Builder interaction | Preview and validation | Persistence and legacy fallback | Priority | Current status / real gap |
| New supplementary CSV/TSV tables | Native multi-file input | Extra material page | Configurable display name and Remove | Filename/type and bounded table read | BuildPlan attachment → CRB Extra material; no full local path | P0 | Closed |
| Existing CRB tables and plots | CRB extra material | Extra material page | Summary only | Category/type/count | Preserved by existing content policy | P1 | Closed with bounded table/plot counts and names |
| New plots | No safe generic upload contract | Extra material plot renderer | Configurable only after a safe serialization contract | Must validate renderable bounded payload | No current authoring path | P2 | Real gap; no arbitrary R-object upload |
| Histology images | Spatial/Trekker payload | Spatial/Trekker, not generic Extra material | Configurable in alignment workbench | Image/section validation | CRB histology fields | P0 | Closed; intentionally not duplicated |
| Generic supporting files | No Viewer contract | No active Viewer consumer | Internal only | None | None | P3 | Do not add an upload that Viewer cannot use |
| Welcome, uploads, host/port, request limit, display/browser mode | Review App options | Generated App startup/runtime | Configurable | Validate types and private-App boundary | App config; defaults preserve legacy behavior | P0 | Closed |
| Initial Viewer page | Viewer currently starts on Data info | Sidebar selection | Configurable only after Viewer startup/tab timing contract | Must validate conditional page availability | No App field yet; legacy Data info fallback | P1/P2 | Real generated-App contract gap |
| Palette persistence | Group colour overrides | Viewer initial colors | Configurable | Validate hex values and group levels | CRB/App config; runtime Color management can override per session | P0 | Closed |
| Current gene, search, table sort, filters, selected cells, zoom | Viewer session | Interactive pages | Internal only | None in Builder | Never frozen | — | Intentionally remains in Viewer |

## Implementation decision for this slice

The next closed vertical slice is **Analysis results** because the Builder
already has bounded evidence and a canonical manifest for it. Core now lists
only detected or planned results, translates manifest disposition into user
language, and shows bounded method/group/table counts. Review shows only a
compact count and result names. No new selection control, state field, CRB
field, or generated-App option was invented.

The metadata correction uses the existing effective policy. Group eligibility
and metadata retention are now presented as separate facts, and a missing
legacy policy produces no retention promise.

The next summary slice covers **Specialized content**. Core now derives compact
Spatial, Trekker, immune repertoire, HLA, and existing Extra material cards
from the same frozen bounded manifest. Review collapses those cards to one
count-and-name overview. Internal diagnostic codes, cell or section identities,
and raw payloads never enter either UI. Ambiguous immune-source selection,
Spatial section include/default settings, and arbitrary new plot uploads remain
deferred until their persistence contracts exist.

The cell-cycle slice reuses the existing `exportFromSeurat(cell_cycle=)` and
Viewer `getCellCycle()` contract. Builder offers a compact card only for
categorical metadata explicitly named as a phase or cell-cycle annotation;
continuous score columns and unrelated categorical metadata are never offered.
The chosen columns are retained in metadata, frozen in BuildPlan, exported to
the CRB, and summarized once in Review. Legacy plans without the field keep the
previous empty behavior.

Trekker summaries add Moran-result and evidence-image counts only when present;
the underlying values and image payloads remain internal. Immune identity and
source conflicts are translated into short barcode/source actions, while raw
diagnostic identifiers remain out of Core and Review.
