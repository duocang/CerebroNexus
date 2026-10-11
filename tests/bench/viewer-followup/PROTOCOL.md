# Focused final follow-up protocol (v4)

Scope: first single-gene operation, Linked completion tail, Trajectory confirmation, and one bounded lifecycle session. No other page performance series. Branch stays `卧槽/pr6_clean_perf`; no additional checkout.

## Fixed Gene experiment

- Baseline: `e8662e47a884bd07f7483dd02328d4ebbcd34573` (production identical to `bad37fca`).
- Rejected first candidate: Gene cache entry tried the existing BPCells integer reader before its compatibility fallback; three pairs did not show stable full-operation gain.
- Final candidate: defer Gene cell-ID core until real hover/brush; preserve the existing recolor path for the unchanged progressive frame before IDs arrive; use existing first-frame metadata for the empty selection counter. Candidate source SHA-256 is recorded per run; final commit is recorded in RESULTS.
- `gene-first-v4.R`, same bytes in every arm. Fresh R/Shiny and Chrome profile per run, warm filesystem/OS cache; no cold-OS claim.
- Same 1M Mouse pages fixture, row-oriented BPCells sidecar and Viewer Pack as the final protocol. Dataset SHA-256 `cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38`.
- WSL Ubuntu 24.04, R 4.6.1, BPCells 0.3.1, Shiny 1.14.0; Chrome 153.0.8010.52 headless, SwiftShader WebGL2, 1619 x 950. This is the established local Chrome series, not Brave or hardware GPU. Full session and source/harness identities accompany each row.
- Launch via devtools::load_all + launchCerebro closed mode, 100% cells, hover enabled, shinytest2 AppDriver. Remove chromote's default --disable-gpu. NOT_CRAN=true.
- Enter Gene, observe an initial primary frame and selectize, then the same one-second websocket quiet observation used in v3. Do not wait for hidden server startup work to finish before clicking Gad1.
- First usable: new primary event, 1,000,000 points and Gad1 label. Complete: primary plus the visible expression_by_group Plotly widget has data and passes two animation frames after its shiny:value. Global Shiny idle is never readiness. Quiet remains a separate transport observation.
- RSS tree sampled every 50 ms in both arms, starting after AppDriver startup. These resource-screen timings form their own v4 series, not pooled with historical timing-only 7.14 s. Byte and message counters cover operation through transport quiet; they include pending page-entry auxiliary traffic.
- Level 1 B1/C1. If correctness, clear gain, memory and complexity pass, extend the identical series with B2/C2/B3/C3. Only if retained, extend to five total interleaved pairs. A final source-file restoration is guaranteed by the runner even on errors.
- The same checkout is switched sequentially between exact baseline/candidate file bytes. HEAD alone does not identify these arms; per-file hashes do. No concurrent benchmarks or package tests.

## Profiling boundaries

Instrumented runs are diagnostic, never pooled with timing runs. Test-only wrappers and optional Rprof identify nested phase costs; nested durations must not be added. First profile exposed a browser Date.now/performance.now clock discontinuity, so its server wall clock cannot be subtracted from browser timestamps. v2 records R elapsed and CPU separately. Recolor events inherit page timing; those inherited fields are not operation measurements. Missing stages remain NA rather than guessed.

## Other acceptance

Linked: one actual-browser profile, then only a minimal candidate if an actionable tail cost exists; one targeted B/C screen, reject unstable/no gain. Trajectory: existing frozen page harness with only trajectory, first/revisit and shared-projection-warm condition kept separate. Lifecycle: finite Gene/Linked page switches, repeated genes, dataset transitions, cache/context checks and supported disconnect/reload; standalone Shiny resumable reconnect is unavailable. No full Viewer run.


## Follow-up interpretation rules

The valid Gene diagnostic v3 uses a monotonic nanonext clock and two browser clock handshakes; its nested durations are not summed. Linked profiling installs its wrappers after the initial flush and after the deferred Linked server is loaded. Instrumented results remain separate from the frozen page confirmation.

The finite lifecycle run uses real Gene/Linked/Trajectory paint events, selected dataset key/generation, fingerprint and point count. After an existing Gene view has actually painted, it allows selector restoration to settle before issuing the next operation. Shiny idle is used only for that unmeasured setup, never as first usable or complete. A prior immediate-input diagnostic showed that input can be overwritten during selector restoration; retain this limitation. Discard aborted/partial harness runs from final RSS summaries.

Trajectory default grouping color and State/static-frame conditions are distinct. A State switch after the dynamic frame may wait behind that old frame's auxiliary work; inherited event timing is not the new operation's server latency. Use the new action timestamp for that operation and inspect geometry protocol separately.
