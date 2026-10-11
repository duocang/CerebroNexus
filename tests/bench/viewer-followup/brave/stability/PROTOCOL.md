# Remaining stability work, 2026-10-10

Baseline: 602f93030de745c7ee0256b8666e6ebfb5a0796a, same branch and worktree.
Inherit hardware, dataset SHA, Windows Brave profile, WSL R dependencies and
1619 x 950 lifecycle viewport from ../remaining/PROTOCOL.md. Gad1 remains the first gene.
Trajectory raw records all report1369x859; use that actual measured viewport,
not the inherited intended override. The paint series is compared only internally.
No Spatial, HLA, Projection, Groups, Immune or Ren benchmark.

The five previous first-Gad1 runs are analyzed individually, without rerunning
or pooling different protocols. Timestamped operations and actual matching
render keys define first usable. Inherited recolor timing fields are not new
decode/upload measurements. Missing physical-click/socket-send measurements
are explicitly unavailable. Monotonic clock alignment uses RTT/2 uncertainty.

Lifecycle B2/C use the identical frozen server.R/probes.js. B was interrupted by
a diagnostic JSON serializer error on html_dependency; it is excluded, retained
as failed instrumentation evidence. B2 fixes the recorder to store output names.
It records Shiny wire messages, binding/invalidation/recalculation events, current
context, visibility and duplicate DOM counts. C changes only the Linked selected
UI lifecycle gate. No Rprof in these finite lifecycle runs. Existing browser
console history survives reload; errors must be attributed by timestamp/session.

SUPERSEDED MEASUREMENT: The following original series measures primary draw,
not first usable. The blocking page loader clears after the later painted
event. See paint/PROTOCOL.md for separately versioned acceptance; never pool.

Trajectory diagnostic Level 1 uses perf-server.R/perf.js, fresh R and fresh tab per arm,
Demo -> Mouse -> Trajectory with default seurat_clusters, then warm State and
revisit. Module/helper traces and Rprof are diagnostics; same harness in both
arms. Save only after all initial paints and visible secondary charts. First
draw is page click to primary draw, including selector initialization before
trajectory_point_color input. Warm State is a different operation, never pooled.
The source is not retained unless true browser latency improves. Single-pair
results screen direction only; acceptance of a performance change requires
three interleaved pairs and five final candidate checks.

Trajectory versions are separate: B=5a511f56, C=indexed column assignment only,
D=C plus the existing Gene empty-selection count mechanism applied to Trajectory.
First comparison B1,C1,B2,C2,B3,C3; second stopped at C3,D1,C4,D2,C5.
C3 is the unchanged-code bridge/Level 1 baseline, not counted twice in a pooled
summary. D3/D4/D5 were not run after finding the readiness measurement omission.
Do not pool C1/C2 with the second comparison or treat B versus D as directly
interleaved. Browser/harness/data/process strategy and ready definition are fixed.

R RSS is the server process. JS heap is not native browser RSS; user tabs prevent
isolation of process-wide Brave RSS. Finite cache/gc checks are not a leak proof.
No ready signal, reconnect policy, precision or rendering semantics are changed.
