# Remaining hotspots: real Brave, 2026-10-10

Base 28444813; existing branch/worktree only. Inherits machine, dataset and headed
Windows Brave identity from ../PROTOCOL.md. R runs in WSL; Brave is native Windows.
Fresh R and fresh browser tab per Gene arm; existing profile and warm OS caches.
Demo initial catalogue -> Mouse 1M -> Gene -> first Gad1 selected through real UI.
The user confirmed the Brave window was foreground before gene-L1-B.

Gene stimulus timestamp is shiny:inputchanged for the first nonempty Gad1 selection.
Keyboard dispatch and socket-send are not separately intercepted. First usable
requires the exact Gad1 primary key, 1M points, painted result and correct legend;
actual hover/brush functionality is checked separately and its deferred identity
cost is not hidden in this timing. Complete is the current visible group plot's
SVG after its value event and two animation frames, after primary acknowledgement.
Other outputs' recalculation/value events are retained. Global idle is never ready.

R trace()/Rprof() are diagnostic-only probes, not production monkey patches;
no ready signal is changed. Paired runs share their exact harness hash. R monotonic
clock exchanges provide offset intervals, not an invented exact network time.
R RSS is the server process (unlike the earlier monitor-inclusive tree figures).
Browser JS heap is sampled every 250 ms. Optional all-Brave RSS includes user tabs
and is not isolated Viewer RSS. Max animation-frame gaps and long tasks are logged.

Instrumentation corrections: gene-B1 is invalid for browser operation timing
(zero-argument Shiny handler interrupted listener installation). gene-B2 and
gene-L1-B have primary evidence but their old group-plot probe checked a missing
data property, so complete is unavailable. These are diagnostic/exploratory only.
The corrected SVG probe is frozen from gene-L1-C onward. gene-L1-C is the rejected
no-warmup experiment. gene-L1-B-review / gene-debounce-C form the first pair for
the distinct standard-Shiny prepared-value debounce candidate. Subsequent pairs
are interleaved and use the same frozen server/browser harness. Do not pool the
old Chrome/SwiftShader series, no-warmup experiment or invalid complete samples.

No warm cached gene result is substituted for the first selection. No precision,
expression reader, geometry format, secondary visibility or hover setting change.

## Linked and Trajectory targeted follow-up

Linked baseline f4e4fe3b / candidate 97f91ac5 use linked-frozen-server.R and
linked-frozen.js. Demo catalogue -> Mouse -> Linked, fresh R/tab, existing native
Brave profile and 1619x950 viewport. Level 1 only. Complete uses the last initial
paint after the clone supplement, not the first complete=true event.

Trajectory baseline 97f91ac5 / candidate 5d3d18d7 use the same linked-frozen
harness revision. Fresh R/tab, Demo -> Mouse -> Trajectory default -> State after
settling -> Data info -> revisit. Default coloring input is generated after page
click: first is computed from page click across that input event, not only from
the later selector timestamp. Candidate initial evidence is saved before hover
and brush. Deferred interaction cost is reported separately. No global idle gate.

## Bounded memory and generation regression

memory-final is a diagnostic run on 5d3d18d7, interrupted when a pre-existing stale
shared-coordinate hint caused a full Linked fallback on A return. Its memory
snapshots must not be pooled with memory-fixed, the subsequent fresh-process
confirmation with context validation. Both use gc() live used MiB (second column
of Ncells and Vcells), not maximum gc columns. Resource/object.size estimates are
shallow for environments/R6/closures; live gc is the aggregate check. Open .bpcells
file descriptors are a proxy, not an exact count of native BPCells handles.
memory-fixed records R resource descriptor environments; no new browser static
cache census is performed. The previously proven map cleanup is not re-audited.

The initial Data info catalogue can display counts before the lazy dataset body
has loaded; memory requests there can be silently aborted by Shiny req(). Start
snapshots therefore refer to the loaded A Gene page. Buttons use normal existing
application requests; dataset switches trigger production cleanup and each memory
snapshot performs gc(). No arbitrary forced deletion of application state.
