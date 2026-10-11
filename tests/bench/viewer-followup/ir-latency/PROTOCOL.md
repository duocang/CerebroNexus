# Bounded IR first-statistic investigation

Date: 2026-10-11. Baseline: 52043c992f72caf2b84cb15f8ab986097b41d6c5.
Actual active branch at start: perf/pr6-ui-accessibility-polish. No new branch,
worktree, push, or rewrite of the five consolidated commits.

Scope: IR first-statistic latency. Preserve statistical algorithms, precision,
cache/dataset isolation, Viewer Pack format, and all production ready/paint gates.
Gene, Linked, HLA, Spatial and Trajectory performance are outside this work.

Initial Rprof diagnosis uses the Ren CRB (194 repertoire samples; 503,432 rows),
scRepertoire 2.9.3 and WSL R 4.6.1. The first diagnostic's proc.time elapsed clock
went backwards at Diversity; that file is retained but is not a valid latency
series. CPU sampling still identifies repertoire QS deserialization and the
Homeostasis table/factor ordering path. The corrected profile.R uses nanonext's
monotonic clock. Diagnostic phases are not end-to-end page timings.

Candidate: use existing sidecar chain descriptors for IR controls, keeping
legacy detection when absent; pass repertoire lazily to the existing Abundance
pack-first helper. Do not prewarm dependencies or move work to page startup.

Browser: real Windows Brave with WSL production Shiny service. Fresh R process
and new tab for each arm, same Ren file and natural viewport (1424 x 1483), warm
filesystem/browser asset caches. Start from Demo, select Ren, enter IR and wait
for the Clonal projection to be rendered and its page loader hidden. Click
Abundance first, then Homeostasis after Abundance completes. The latter may
inherit already-read repertoire in baseline, which is deliberately not hidden.

The unchanged server.R/probe.js observe click -> first visible plotly_afterplot
and two subsequent animation frames; each result must also have real plot
traces and a hidden completed spinner. Global Shiny idle is not a ready signal.
No production readiness is modified. Record source MD5 for both arms, raw
browser observations, R logs, and screenshots. No concurrent test/profiler load
during browser timing. Level 1 B1/C1 first; retain only if meaningful page gain,
then extend to three interleaved pairs. Stop without broader optimization if the
candidate lacks benefit. Functional tests separately cover pack miss, legacy
data, empty counts, dataset replacement and unsupported analytical settings.
