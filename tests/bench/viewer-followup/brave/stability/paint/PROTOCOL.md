# Painted readiness correction, v2

Do not pool with the parent-directory draw-signal series. Review discovered that
Trajectory's cerebro:specialist-state is emitted synchronously after drawAll,
before its two-frame cerebro:cell-view-ready event. viewer-shell.js removes the
blocking loader only on the latter. The former is a primary draw signal, not
proof that the user can already interact. The old measurements remain intact
as diagnostic preparation/draw data; they are not final first-usable acceptance.

All other conditions are unchanged. Fresh native Brave tab and fresh WSL R,
1369x859 (actual innerWidth/innerHeight in every saved run), fixed Mouse 1M, default seurat_clusters, no prior page visited, same
dataset/harness hash throughout this series. B=5a511f56; D=both small Trajectory
changes, named C in this directory. Interleave B1,C1,B2,C2,B3,C3, then C4,C5 only
if the earlier comparison succeeds. All runs retained. This explicit new version
corrects a measurement omission; it does not modify application ready behavior.

First usable = page click to first trajectory painted event with 981807 rendered
points, current Mouse context, pageReady=true, aria-busy removed and blocking
loader class absent. The remaining 140ms opacity fade is not changed and is not
a blocking interval (pointer-events:none). Actual hover, box selection, methods,
labels/edges, revisit and dataset switching are independently checked at the end.
Draw time remains separately recorded. Warm State and revisit use new operation
timestamps and the same painted checks. No global Shiny idle defines readiness.

diagnostic-listener-B1 exposed listener order: the harness listener executes
before the production loader listener. It is retained but excluded. Before any
formal pair, the probe was corrected to check in a microtask after synchronous
event listeners finish. Both eventAt and check time are recorded; no frame or
timer is added. B1 onward all use that same corrected harness hash.

R RSS covers the process; JS heap is not native browser RSS. Full memory/profiling
results from later functional operations must not be substituted for first-entry
peaks; preserve a first-only snapshot before those operations.
