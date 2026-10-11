# IR latency fix: exact summaries and deferred repertoire hydration

Date: 2026-10-11. Branch: `perf/pr6-ui-accessibility-polish`.
Baseline: `203d57930fc53f42fbe5d54a05868e723c5c802d`.
Candidate: the four production files identified by `C1/source.csv` through
`C3/source.csv`, committed with this report. No branch/history rewrite or push.

## Change and scope

- Abundance now checks the existing Viewer Pack summary before forcing
  `ir_data()`. Controls use the existing repertoire descriptor's chain list;
  legacy descriptors still use raw chain detection.
- Default all-chain, ungrouped, unordered Homeostasis reads an optional exact
  clone-size histogram. Its cell-weighted proportions and intervals are the
  same as scRepertoire. Without that asset, an exact raw-data histogram is built
  on demand. Non-default chain, grouping, and ordering retain the original
  scRepertoire path.
- The pack builder writes `immune/homeostasis.qs2` separately from Abundance:
  Homeostasis includes empty-string clone identities, whereas Abundance drops
  them. NA values are excluded in both. No quantization or expression changes.
- No new global/session cache, no loading-style changes, no ready/RAF changes,
  and no work on other Viewer pages.

## Fixture and protocol

Real Windows Brave, controlled through the browser extension; WSL R 4.6.1,
scRepertoire 2.9.3. Viewport 1424 x 1483, document visible in every measurement.
Dataset: Ren, 1,462,702 cells; repertoire 194 samples / 503,432 rows.
CRB fingerprint: `md5-crb-v1:a1b062ac23bb7f01244a869c87df0a00`.

The previously missing Ren Viewer Pack was generated once with the production
builder, into the new sibling `cerebro_ren_covid_1.46m.viewer` directory next to
the CRB in `shinyapps_docker/apps/cerebronexus/private-data`. Full validation
passed (44 assets). Both baseline and candidate use this **same complete pack**.
The two IR summaries are each 14,406 bytes on disk (identical for this fixture,
which does not exercise the blank-string distinction). Complete pack size,
including manifest, is 94,800,584 bytes. These are server-side disk sizes, **not
browser network payload measurements**. No private raw data is committed.

Sequence: B1, C1, B2, C2, B3, C3. Each uses a fresh R process and Brave tab on
port 8947. Start at Demo, select Ren, open IR's Clonal projection, verify its
draw/controls, then first Abundance followed by first Homeostasis. Defaults:
gene / All chains / no grouping / default thresholds. This is first analytical
entry within a new app session, **not a cold OS/file-cache benchmark**, and does
not time application startup or the IR landing page.

`../server.R` serves the production app plus passive `../probe.js`. The probe
records the tab click, first visible Plotly afterplot, then two animation frames.
It does not change app readiness. `painted_ms` is this two-frame acknowledgement,
not an independent GPU-completion measurement. Source MD5s are recorded per run;
`identity.csv` records SHA256s of the common harness and data assets. Frozen arm
bytes are backed up at
`C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-11-ir-fix-203d5793`.

Setup anomalies were observed rather than counted as speed measurements: C1's
first attempted tab click left Clonal projection selected with zero Abundance
traces; the recorded click is the subsequent first successful Abundance entry.
B3 briefly landed on Gene ID conversion during dataset/sidebar initialization;
IR was reselected and its landing draw/controls verified before measurement.
Some tool waits timed out before their predicate was delivered; the passive
browser records subsequently arrived without reclicking. No measured row was
dropped. Setup time is excluded consistently. This is a bounded three-pair
local experiment, not a whole-Viewer or cross-machine performance claim.

## Browser timings

Milliseconds; all Abundance results contain 194 traces, Homeostasis 5 traces.
The first two records per run are the timed series; later C3 records are smoke
interactions and are deliberately excluded from `timings.csv`.

| Run | Abundance afterplot | Abundance painted | Homeostasis afterplot | Homeostasis painted |
|---|---:|---:|---:|---:|
| B1 | 6404.1 | 7225.4 | 4517.0 | 5371.8 |
| C1 | 2048.7 | 3276.7 | 1861.6 | 2381.8 |
| B2 | 8804.8 | 8820.1 | 3571.5 | 4308.4 |
| C2 | 1944.1 | 3022.2 | 1390.9 | 1409.3 |
| B3 | 6737.1 | 7375.1 | 3570.2 | 4339.2 |
| C3 | 3267.3 | 4392.5 | 1510.5 | 2326.1 |
| Baseline median | 6737.1 | 7375.1 | 3571.5 | 4339.2 |
| Candidate median | 2048.7 | 3276.7 | 1510.5 | 2326.1 |
| Reduction of medians | 69.6% | 55.6% | 57.7% | 46.4% |

Every pair improved both measures for both tabs. Browser frame acknowledgement
still contributes variable waiting. No attempt was made to remove those gates.
The earlier no-pack diagnostic runs in the parent directory are not pooled
with this series. These results do not establish that all IR tabs are fast.

## Correctness and compatibility

- `run-tests.R`: **534 passed, 0 failed, 0 skipped, 0 warnings** across IR,
  repertoire validation, plot helpers, and Viewer Pack tests (`tests.csv/log`).
- Tests execute the actual renderer/control bodies with a getter that throws
  if raw repertoire hydration is attempted on the packed path. They cover
  current-dataset asset replacement, old-pack fallback, missing controls,
  non-default chain/group/order fallback, and descriptor chain controls.
- Exact Homeostasis tests cover four clone definitions, blank/NA/literal-NA
  identities, Unicode, empty samples, custom thresholds, stacked coordinates,
  and colors against scRepertoire.
- `verify-ren.R`: all four clone definitions, 194 samples each, match
  scRepertoire for both packed and raw fallback matrices. Maximum absolute
  difference is 2.220446e-16, within 1e-12 tolerance (floating summation order).
- Real Brave: default plots; strict clone definition; custom thresholds;
  all-chain severity-grouped Abundance/Homeostasis; revisit; Ren -> Demo -> Ren.
  Demo has no sibling pack, renders its three sample labels, and returning Ren
  restores its own 194-sample results. No cross-dataset label leakage observed.
- Timed default runs had no output/console/R errors. Browser console warn/error
  queries were empty. See the exception below for an existing unsupported
  fallback combination discovered during the additional smoke test.

### Existing fallback limitation, not silently treated as a pass

Ren `strict / TRB` Homeostasis reports `undefined columns selected` inside
scRepertoire; the UI displays its existing empty-state message. Calling the
**unchanged upstream function directly** on Ren reproduces the same error,
without any new histogram/renderer code (`verify-ren.log`). The fallback
renderer body remains the baseline implementation. This compatibility issue
is not fixed by the default-path latency work. Grouping also emits the existing
RColorBrewer Set2 >8-colors warning. Both are preserved in `C3.log`; do not claim
an error-free pass for every chain/clone combination. All-chain severity plots
did render successfully after leaving that combination.

## Cost, deployment and remaining limits

The default packed path avoids materializing the ~295 MB R repertoire object
solely for these controls/plots. That is a removed allocation requirement,
**not a measured total RSS/heap reduction**. No new browser payload encoding,
persistent cache, precision loss, or preloading of full expression/metadata was
introduced. Existing metadata handling for group selectors remains in place.

New exports include the optional Homeostasis asset. Existing packs and CRBs
remain readable; rebuilding a pack is needed to obtain the packed Homeostasis
benefit, while old datasets still use the exact raw fallback. The local Ren
pack was built and validated; deployed sessions need the updated code loaded.
Pack construction is offline work, not cost eliminated from the system.

The saved browser screenshots, per-run JSON/source hashes/logs, build log, test
results and full-Ren verification log are adjacent to this report. No additional
performance experiments are needed for this change. Gene, Linked, Trajectory,
HLA and Spatial remain outside scope. No push was performed.
