# Remaining repeated-work audit

2026-10-11. Baseline `79de07f9e2407ab3dfe4b91201f04f9622fa8d9b`, existing
`perf/pr6-ui-accessibility-polish` checkout. Production files are unchanged.
This is an audit with bounded R reproductions, not a browser A/B or a claim of
an implemented speedup. No new branch/worktree, history rewrite, or push.

## Evidence and method

Read the shared-layer audit, IR latency investigation and retained-fix results,
and the Groups/Marker/result-page closeout reports first. Then traced current
Viewer output/cache/reactive paths. Previously closed Gene, Linked, Trajectory,
HLA and Spatial performance topics were not reopened.

`probe.R` evaluates actual production helper definitions and selected reactive
expressions in an isolated environment. It does not patch a running app.
`size-probe.R` calls the actual SizeDist profile helper. Environment: WSL R
4.6.1, installed scRepertoire 2.9.3, Ren CRB (1,462,702 cells; 194 repertoire
samples / 503,432 rows). Sidecar loading and package loading are outside the
reported function timings. The clock is `nanonext::mclock()` in milliseconds.
First helper invocations may include JIT/initialization overhead. These are
function-level observations, not whole-page latency or steady-state RSS.

## Ranked findings

### 1. SizeDist recomputes expensive profiles for cheap or irrelevant changes

Anchors: `inst/viewer/immune_repertoire/visualizations.R:1857` (renderer and
cache keys), `data.R:582` (`ir_clone_size_profiles`), `data.R:657`
(`ir_empirical_size_distribution_plot`).

- Both the large-data native path and the small-data scRepertoire path fix the
  clone definition to **strict**. Nevertheless, the output cache key contains
  `input$ir_cloneCall`. Changing that selector can miss the cache even though
  it cannot change the SizeDist calculation.
- Changing the linkage method creates a new whole-plot cache key. The native
  plot helper calls `ir_clone_size_profiles` again before `dist`/`hclust`, even
  though the profiles do not depend on the linkage method.
- The renderer calls `req_scRepertoire()` before choosing the native path.
  Consequently even the >=100,000-row native path forces the scRepertoire
  namespace load. The previous independent diagnostic measured 2.011 seconds
  for that first load; it is a one-time process cost, not a per-change cost.

Measured native profile preparation: **3085 / 1430 / 1377 ms**.
Clustering the resulting matrix using ward.D2 / complete / average: **1 / 1 / 1
ms**, at this clock's resolution. Result matrix: 194 x 10, `object.size` 29,704
bytes. The full helper result, including named clone counts: 86,276,272 bytes.
These object sizes are not measurements of additional RSS or physical sharing.

Recommendation: highest priority. Separate the exact compact profile from plot
construction, reuse only that small matrix for method changes, remove the
irrelevant clone-definition cache dependency, and require scRepertoire only on
the path that calls it. Preserve threshold/chain/group/dataset dependencies and
the existing scientific definition. Do **not** cache the 86 MB helper result.
Requires targeted correctness tests and browser acceptance before a speedup
claim; no production candidate is applied in this audit.

### 2. IR dynamic controls discard non-select user values on remount

Anchor: `inst/viewer/immune_repertoire/settings.R`, `ir_analysis_panel`.
The code reads the current input value, but only the select-input branch uses
it. Numeric, text and checkbox branches always emit the specification default.

Actual generated HTML in `panel-*.html` reproduces:

| Existing input | Newly generated control |
|---|---|
| Homeostasis thresholds `0.001, 0.01, 0.1, 0.5, 1` | `0.0001, 0.001, 0.01, 0.1, 1` |
| Diversity bootstrap iterations `100` | `20` |
| Abundance scale `TRUE` | unchecked |

This is primarily a state-correctness issue. A remount can also change a plot's
cache key and cause another render or calculation, depending on cache contents;
it does **not** prove that every remount reruns an expensive computation. The
previous Brave smoke also observed default Homeostasis thresholds after tab
changes; the isolated helper reproduction establishes the cause here.

Recommendation: preserve validated current values for these three control
types, with explicit dataset-reset behavior. Fix alongside targeted IR work,
not through broad UI reconstruction. No browser timings attributed to it.

### 3. Identical grouping columns are rejoined after unrelated IR tab changes

Anchors: `data.R:53` (`ir_requested_metadata_columns`), `data.R:113`
(`ir_data_annotated`). Requested columns depend directly on the tab, even when
two tabs need exactly the same fields. Reactive invalidation propagates to the
barcode join before equality of the returned field vector is considered.

Using the production reactive expressions with `groupBy = severity` and four
demanded tab states Diversity -> Homeostasis -> Abundance -> Diversity invoked
the annotation helper **four times**. First invocation: 1403 ms; subsequent
invocations: **162 / 156 / 168 ms**. All requested the same grouping column.
The `iteration` field in these four raw timing rows is the post-call counter
plus one because the argument was lazily evaluated; the stage names and explicit
`ANNOTATION_CALLS_SAME_GROUP 4` log are authoritative for operation count.

This is an invalidation/demand reproduction. A cached plot can bypass this
reactive, so it is **not** a claim that every ordinary tab revisit pays the join.
The initial invocation's larger cost is not claimed as repeated savings.

Recommendation: medium priority, after SizeDist. Stabilize the requested-field
dependency when its semantic value is unchanged, while keeping current dataset
and metadata identity in the invalidation boundary. Do not add a global metadata
cache or suppress real field changes.

### 4. Groups has repeated counting, but the measured cost is small

Anchors: `inst/viewer/groups/composition.R:112,134,181`. Bar, Sankey and the
optional table independently call `groupsCalculateTableAB`; plot colors/style
are part of the whole-plot cache key, so a novel presentation can recount.

Ren sample x major_type counting: long table **22 / 17 / 21 ms**; wide table
**318 / 29 / 28 ms**, including the first pivot initialization. No current
multi-second counting bottleneck was found. Hidden outputs/optional-table
suspension mean these calls must not be assumed to all run on initial entry.

Recommendation: low priority. A shared small contingency reactive is possible,
but its likely gain does not justify a cross-page cache refactor or reopening
the finished Groups work solely for this observation.

### 5. Paired Scatter passes more samples than it needs; limited measured gain

Anchors: `visualizations.R`, `ir_paired_scatter_panel` and faceted caller. The
installed upstream `clonalScatter` first wrangles the input list, then chooses
the requested sample columns. The caller passes all samples for each panel.

Three paired calls on Ren, gene / both chains / first two samples / total dot
size, comparing 194-sample input versus just those two named samples:
**24 -> 13, 19 -> 12, 18 -> 13 ms**. Exported 3,466-row tables were identical.
Median difference: **6 ms**. This does not measure ggplotly, facets, other chains,
metadata grouping, or a third sample used for dot size.

Recommendation: do not prioritize. A safe subset would also need to preserve
third-sample dot-size and group semantics; the measured default gain is tiny.

### 6. Table formatting still scans values with formatting disabled

Anchor: `inst/viewer/utility_functions.R:2169-2174`. `prettifyTable` discovers
integer, percentage, p-value and logFC columns before testing display toggles.
Marker, enrichment, Most-expressed and Extra tables use this helper. Some
classification is also used for alignment/coloring, so simply skipping all
classification when number formatting is off is not automatically equivalent.

Recommendation: static candidate only. No large current-fixture timing was
established. If pursued, identify which classifications are actually consumed
under each toggle combination; do not change stored numeric semantics.

## Paths that should not be misclassified as redundant heavy analysis

| Area | Current evidence / disposition |
|---|---|
| Marker / Enriched / Most expressed | Read stored result tables; no FindAllMarkers, Enrichr, GSVA or mean-expression recomputation on entry. Most-expressed reads both stored metric slots to build choices; its R6 getters return existing tables. Filtering/formatting/rendering remain real costs. |
| Extra material | Uses stored tables/plots; interactive ggplotly conversion can be repeated when demanded, but no current multi-second fixture cost demonstrated. |
| Hidden pages | Shared output suspension follows sidebar ownership; IR uses output-space gates and session plot caches. No new periodic compute loop was found; the `invalidateLater` search located the authentication expiry timer, which is intentional. This is code tracing, not exhaustive live instrumentation. |
| Abundance / Homeostasis defaults | The previous retained fix is present; do not re-add eager repertoire reads or duplicate its summaries. |
| Diversity | Bootstrap calculations genuinely depend on metric/group/bootstrap settings. Current full-plot cache avoids repeating identical keys. No evidence here supports reducing iterations or changing statistics. |
| Projection selection | Some selection summaries independently map barcodes; hidden-group filtering combines metadata with projection. This overlaps the already-recorded shared-layer audit; no new measurement or global index proposal. |
| Colour management / Trekker | Colour pickers isolate initial colors; Trekker metadata scan is dataset-reactive. No new repeat-cost claim without a triggering dependency and timing. |
| Gene / Linked / Trajectory / HLA / Spatial | Earlier closeout/stop decisions stand. Shared data foundations, deferred metadata, precision, canonical geometry, generation gates and paint confirmation remain untouched. |

## Result

Recommended next change is the bounded **SizeDist** preparation/cache dependency
fix, followed by IR control retention. Metadata rejoin invalidation is smaller
and should be assessed separately. The other observed duplication is currently
low yield or unquantified. Only diagnostic scripts, logs, generated control
HTML and this report were added. No production/UI behavior changed, and no
browser acceptance or whole-page speedup is claimed by this audit.
