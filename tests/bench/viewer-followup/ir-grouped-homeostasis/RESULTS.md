# Grouped Homeostasis: exact counts and dependency avoidance

2026-10-11. Branch `perf/pr6-ui-accessibility-polish`; baseline
`6dcd900e77e5b90f043300a421a24af0be3f25e9`. Candidate is committed with this report;
`smoke-final/source.csv` records the served production source hashes. No new
branch/worktree, history integration, or push.

## Problem, profile and implementation

The prior diagnostic found severity-grouped Homeostasis still took a median
3.945 seconds after raw data were loaded. The renderer routed any grouping to
scRepertoire, also forcing its first namespace load when not already loaded.

A short Rprof of the unchanged function on Ren took 6.158 seconds. Sampled
total time attributed to wide `rbind` was 60.91%; `order` under clone-string
`table` construction was 28.66%. These are profile observations, not additive
formal benchmark medians. See `profile.R`, `profile.log`, `grouped.Rprof`, and
`profile-{total,self}.csv`.

The native path now covers **All chains, no explicit order, with or without
grouping**. For grouping it selects only clone and grouping columns, pools
same-group cells across source samples, hashes exact clone identities into
integer indices, and tabulates clone sizes. Existing exact cell-weighted bins
and plot construction consume that small histogram. It does not use a
sample-level pack for grouped data, because clones must be pooled across
samples before sizes are counted.

Compatibility details established against installed scRepertoire 2.9.3:

- Blank-string and literal `NA` clone identities count; missing clone values do
  not. Empty/all-missing-clone source samples are removed before grouping.
- Grouping follows base `split`: missing groups are excluded, empty-string
  groups are retained, factor levels/order and zero-count unused levels remain.
- Matrix rows use integer positions so a valid empty-string group is safe.
- Cross-sample repeated clones merge exactly within their group. There is no
  delimiter-based composite identity or precision reduction.
- Single-chain and explicit-order requests retain the prior scRepertoire
  fallback. Default ungrouped Pack behavior and thresholds remain intact.

The first Brave candidate exposed a dataset-switch boundary: an old Ren
`severity` selector could reach the renderer after Demo had become active.
Homeostasis now requires the selected grouping to belong to the current
dataset before rendering; it resumes when the existing controls reset. This
is checked outside the plot error handler. Missing required columns in the
pure helper still raise a real error rather than being silently ignored.

## Validation and timing protocol

Environment: Windows Brave with WSL R 4.6.1; scRepertoire 2.9.3. Ren has
1,462,702 cells, 194 repertoire samples and 503,432 receptor rows. Both arms
use the same already-loaded input annotated with severity and patient (the
latter also supports the separate equality checks). Timings measure exact
Homeostasis plot construction plus `ggplot_build`; they exclude CRB/sidecar
hydration, metadata join, namespace load, ggplotly, serialization and browser
paint. They are not whole-page latency.

`quantify.R` performs one Level 1 pair (pair 0), then three interleaved
baseline/candidate pairs. GC runs outside each timed interval. The baseline is
the unchanged upstream call used by the old renderer. Each pair checks plot
stack positions and colors at tolerance 1e-12. The final run is performed
after the test runner and owned browser service have stopped.

`exploratory*.csv/log` retain the earlier diagnostic, which overlapped the
regression runner/service startup and is excluded from the final series. That
diagnostic completed its numerical assertions but ended with a source parsing
error after its script was edited while running; it is not a clean completion
claim. The final run uses a fixed script and fresh R process.

The same script checks four clone definitions (gene/nt/aa/strict), severity
(3 groups) and patient (196 groups), including custom thresholds for patient,
against the full upstream exported matrices. Fresh-process assertions also
verify scRepertoire is absent from loaded namespaces both before and after
the native grouped plot is built.

Final matched backend series (`timings.csv`, `quantify.log`), milliseconds:

| Pair | Baseline | Candidate |
|---|---:|---:|
| Level 1, excluded from median | 5297 | 689 |
| 1 | 5478 | 812 |
| 2 | 5400 | 733 |
| 3 | 5312 | 762 |
| Median of pairs 1–3 | **5400** | **762** |

Median elapsed time decreases **85.9%**, or 4.638 seconds for this specific
already-loaded backend operation. Each pair improves. Do not compare the final
candidate directly to the earlier unpaired 3.945-second diagnostic to calculate
a speedup; the matched baseline above is authoritative for this series.

In the fresh final process, the first native build took 1278 ms and did not
load scRepertoire. Loading that unused namespace explicitly afterward took
2848 ms. This establishes the avoided dependency on this path, not a global
package-loading improvement or a measured sum of UI savings. Other first-use
package initializations may still contribute to the native first build.

Full Ren matrix checks match within 1e-12; maximum absolute error across the
eight combinations is 7.771561e-16. Severity histograms occupy 29,496–35,256
bytes; patient histograms 196,368–212,480 bytes, depending on clone definition.

`run-tests.R`: **619 assertions passed, 0 failed, 0 skipped, 0 warnings**
(`tests.csv`, `tests.log`). Covers previous IR/Pack paths, exact grouped counts,
empty/NA/Unicode identities, cross-sample pooling, factor levels, thresholds,
plot stacks/colors, native-vs-fallback dispatch, and stale-dataset grouping.

## Real Brave smoke

Final candidate served with the existing `../ir-latency/server.R` harness,
Windows Brave viewport 1424 x 1483, WSL R backend, local port 8947. Ren -> Demo
-> Ren was completed. Verified:

- Ren severity grouping produces its three groups; strict clone definition and
  custom thresholds `0.0002, 0.002, 0.02, 0.2, 1` render correctly.
- Demo resets the invalid Ren grouping and renders sample_1/sample_2/sample_3;
  choosing sample grouping works with the retained thresholds.
- Returning Ren restores its own data and severity choices. Re-entering the
  grouped view renders Ren's three groups, without Demo sample-label leakage.
- Final browser warn/error capture is empty; final R log has no new errors.
  The earlier missing-column error is retained in `smoke-before-guard.log`,
  was fixed, and does not recur in `smoke-final.log`.

Evidence: `ren-final.png`, `ren-return-final.png`, `demo-final.txt`, other DOM
snapshots, `browser-console.json`, and `smoke-final/`. Old-candidate screenshots
and logs are labelled separately. Passive browser timing records are incidental
and excluded from performance claims; no real-browser A/B was performed in this
task. Only owned test tabs/services were closed after acceptance.

## Cost and scope boundaries

No new persistent cache, no prewarming or work moved to startup, no new Viewer
Pack asset or contract. Temporary two-column pooling and compact histograms
replace wide receptor-table copying and sorting. Compact histogram sizes are
reported by the measurement script; these are object sizes, not measured peak
RSS or retained session heap.

Other genuinely scRepertoire-backed analyses still load that package on first
use. The optimization removes that requirement from this Homeostasis path;
it does not make `loadNamespace` itself faster. First complete repertoire
hydration remains necessary for grouped Homeostasis. Namespace and analytical
timings must not be added and advertised as a measured whole-page saving.

No changes to expression readers, geometry, RGB, wire protocol, ready/RAF,
dataset-generation framework or previously closed Viewer performance topics.
