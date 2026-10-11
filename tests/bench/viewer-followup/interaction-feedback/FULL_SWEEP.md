# Viewer page and interaction sweep

Date: 2026-10-10. Baseline: `02ef14fc17fd19ec54b9c1179425d94d02df0a95`.
Branch: `卧槽/pr6_clean_perf`, existing checkout. No branch/worktree creation or push.
This follows the user's request to audit all Viewer pages together, including
Most expressed genes, rather than waiting for individual page complaints.

## Findings and implementation

| Finding | Resolution |
|---|---|
| Most expressed genes, Enriched pathways and Extra material repeat the nested shell/control/result assembly seen in Groups and Marker | A small static `cerebroResultPage()` helper now supplies the heading, controls slot and result box to four stored-result pages. Remove redundant server-rendered box/category wrappers. Retain actual data-dependent controls and results. |
| Nested HTML outputs multiply pending opacity and make a whole page look faded while individual results arrive | The shared invalidation handler dims concrete outputs, not structural `shiny-html-output` wrappers. Busy semantics and existing output messages remain. |
| Most lists only percent-summary groups although it also supports mean summaries | Include the union of both stored catalogs; deduplicate groups, preserve a still-valid metric, reject stale groups. No cell metadata read for these choices. |
| Most metric explanation arrives through another server-rendered output | Use local conditional text in the existing controls. Scientific calculations and table formatting are unchanged. |
| Enriched method changes tear down dependent selector wrappers | Render initial method/group together; update group choices in place and preserve valid selection. Validate method/group before reading results. Reuse the existing result reactive and show stored empty-result explanations directly. |
| Data info and Colour management include fixed structure in dynamic outputs | Put the statistics frame and Colour heading in static UI; keep dataset values and colour-picker lifecycle dynamic. |
| Initial result/toolbar areas collapse before demanded controls/tables arrive | Reserve bounded frame space, with plain preparation text and a narrow-screen controls rule. Extend the existing table placeholder to stored-result pages and Gene ID conversion. No reveal barrier or animation. |
| Some Info modals have no visible dismiss control or dialog semantics | Shared Shiny-modal handler adds dialog labeling and a close button only when dismissible and no existing dismiss button is present. Mandatory dialogs retain their dismissal policy. |
| HLA's four tabs all report selected on outer page entry | Bootstrap 3's accessibility `activate` wrapper marks every descendant tab selected after emitting `shown`. Reconcile native Bootstrap tab selection, expansion and tabindex after the handler returns; also initialize dynamically bound tabsets. No tab-content or rendering scheduler changes. |
| Several navigation messages say only “this page” | Complete the existing page-name mapping for all 17 Viewer destinations. |

This builds on the already committed motion cleanup in [MOTION.md](MOTION.md)
and Groups/Marker changes in [GROUPS_MARKER.md](GROUPS_MARKER.md). No additional
decorative entrance, stagger, artificial waiting period or all-results barrier
was introduced. Results still arrive when their actual work finishes.

## Complete page inventory and current browser sweep

All 17 page implementations were inspected, and each was opened in real Brave
with a compatible existing dataset. “Checked” below describes actual scope,
not every combination of every scientific parameter.

| Page | Current real-browser check | Source/lifecycle decision |
|---|---|---|
| Data info | Demo/Mouse/Spatial/Trekker/HLA information and dataset selector | Static statistics frame; existing dataset overlay and preferences retained |
| Projection | Demo 1,476-point canvas, legend and ready state; visual layout | Existing primary controls and canonical renderer retained |
| Linked views | Demo two linked spaces, 1,476 points each, controls and ready state | Existing progressive primary/supplement and deferred identities retained |
| Groups | Composition and expression metrics populated; both result frames | Keep previous static shells and viewport-demanded metrics |
| Marker genes | Method/group controls, subgroup 0, 147 records, populated table | Shared frame; previous selector/data guards retained |
| Most expressed genes | Sample -> cell_type/T cells, 100 records; search/empty search, download menu, Info close, leave/return | Remove redundant assembly; include mean-only catalog and preserve metric |
| Enriched pathways | Enrichr: 38 records; GSVA: explicit stored-threshold empty state; Info | Paired selectors, direct terminal text, stale catalog guard |
| Extra material | Embedded file/table selectors, cluster_cell_counts, 10 rows | Remove one category wrapper and dynamic result box; retain file/sheet identity and lazy getters |
| Immune repertoire | All nine subtabs: Clonal projection, Abundance, Diversity, Homeostasis, Isotype, Paired Scatter, Clone Sharing, Compare, SizeDist; Settings open/close | Existing data-dependent tabs and requested Settings retained; no eager analyses |
| Trajectory | Mouse: E18_neurogenesis, 981,807 visible points; painted ready, Settings open/close | Static State/excluded-index/auxiliary demand unchanged |
| Spatial | Fixed 1M canonical grid and alignment background visibly drawn; background-loaded and ready feedback | Existing Spatial contract and fast path retained |
| Trekker | Existing 2,536-cell demo, side-by-side Spatial/UMAP; hover barcode, box select 226, matching two plots and cohort count; clear; shared Settings | Existing shared renderer/drawer retained |
| HLA & TCR Motifs | Demo explicit no-motif state; HLA fixture network, Network data, Associations, Data & QC; final nested-tab entry/click/ArrowRight/revisit semantics | Existing parser, network and demand gates retained; shared tab semantics fixed |
| Gene expression | Demo CD74 expression canvas and group summary; leave/return preserves gene and ready state | Mounted selector, deferred identity and existing render confirmation retained |
| Gene ID conversion | Mouse identifier table, 53,379 entries | Existing static selector/table; reserve table frame |
| Colour management | Three grouping-variable colour sections; Info dialog semantics and visible close | Header static; picker dependency/retention unchanged |
| About | Version, authors, citation, license and links present | Already direct/static; no redesign needed |

Shared interaction categories checked in source: navigation and revisit,
dataset replacement, input selectors and dependent choices, buttons and pending
states, tables/filtering/empty results, Info/dialogs, Settings drawers, tabsets,
canvas selection/hover, output errors and loading labels. Prior detailed
Gene/Linked selection-table and specialist control acceptance remains in
[ALL_PAGES.md](ALL_PAGES.md); it is not relabeled as a new exhaustive run.

## Conditions and evidence

- Real **Windows Brave**, extension-driven UI actions; **WSL R 4.6.1** serves
  the production app. Desktop viewport 1369 x 915, existing reduced-motion
  preference. No UI-state injection, fake ready, network throttle or monkey patch.
- Demo sweep used localhost 8942. The expanded fixture catalog used localhost
  8944. Port 8943 was already occupied; it was not replaced. The agent's earlier
  8942 smoke server was stopped after moving to 8944.
- `server.R` only gained optional fifth/sixth fixture arguments (Trekker/HLA).
  Its final SHA-256 is
  `785408063fed1e212cb6d803a35c3c871a6e43e17382c85651226e4c0fec0861`.
- Datasets: repository `inst/extdata/examples/example.crb`; existing private-data
  `cerebro_mouse_brain_1m_pages_clean_v1.crb`,
  `cerebro_spatial_canonical_1m_acceptance_v1.crb`, `demo_trekker.crb`,
  `demo_hla_tcr_dextramer.crb`. No fixture data were modified.
- Same-session route: Demo -> Mouse -> Spatial -> Trekker -> HLA -> Demo.
  Conditional navigation changed with data availability; returning Demo showed
  its sample summary/table without prior dataset content. This is ordinary UI
  replacement coverage, not an adversarial stale-generation injection test.
- Final tab-semantics adjustment was tested after a fresh reload: one selected
  tab and one tabindex 0 on entry, Network data click, ArrowRight to Associations,
  and About -> HLA revisit. No Bootstrap method override was installed.
- Evidence: `implementation-evidence/full-sweep/`: page DOM snapshots,
  screenshots, `sweep.json`, initial/final tab-attribute JSON, console samples,
  and the captured 8944 R-session output. `trajectory.txt` is an intermediate
  loading sample; `trajectory-ready.txt` records the completed visible view.
- Browser warning/error samples are empty. Inspected R output contained no new
  application exception. Empty GSVA/no-motif states are expected results.

## Targeted verification

Passed WSL R suites: `groups-performance`, `marker-page-controls`,
`most_expressed_genes`, `enriched_pathways`, `extra_material`,
`viewer-extra-material-selector`, `dataset-loading-progress`, `view-feedback`,
`viewer-review-feedback`, `viewer-accessibility-design`, `viewer-color-config`,
and new `result-page-controls` (11 assertions). The final focused rerun covered
result-page-controls, extra-material selector, accessibility, colour lifecycle,
and review feedback. The moved Colour heading required the existing lifecycle
test's render counter to observe its dynamic box instead of the now-static header.

All 169 Viewer R files parsed. `node --check viewer-shell.js` and
`git diff --check` passed. The old shinytest2 module selector was updated for
the removed Most wrapper; that separate browser suite was not run. Real Brave
provided this round's browser evidence.

## Boundaries and stop decision

No expression reader, RGB, geometry, barcode order, wire precision, payload
protocol, data cache, generation/epoch isolation, BPCells, RAF or ready gate was
changed. No full metadata materialization or eager hidden-page analysis was
added. Removed UI wrappers reduce structural messages, but no byte/RSS/heap or
speedup claim is made. Small static markup and two bounded shared UI handlers
were added; no per-cell state or second lifecycle framework.

The mean-only metric and stale/empty catalog variants are covered by R tests;
Demo does not provide every metric or multiple Marker methods. Extra plot/file
variants, uploads/exports, all parameter combinations, ordinary-motion duration,
and full mobile layouts were not exhaustively exercised. The current sweep does
not supersede earlier documented reconnect/steady-state/performance boundaries.

All discovered structural/semantic defects in this bounded sweep were addressed.
There is no claim that all future datasets or interaction permutations are
defect-free. Gene <=3 seconds and other closed performance directions remain
closed; PR7 Spatial ROI/background contract work remains for Builder. Stop this
UI sweep after local commit; do not reopen performance experimentation.
