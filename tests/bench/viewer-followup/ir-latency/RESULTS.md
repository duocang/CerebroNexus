# IR latency investigation: no production change retained

Follow-up: this is the historical diagnosis. The subsequent retained fix and
three-pair Windows Brave validation are in [fix/RESULTS.md](fix/RESULTS.md).
That follow-up builds the previously missing Ren pack; the no-pack findings
below describe the earlier fixture state and are not its A/B baseline.

Date: 2026-10-11. Baseline/source HEAD:
`52043c992f72caf2b84cb15f8ab986097b41d6c5`.
Actual branch: `perf/pr6-ui-accessibility-polish`; existing worktree reused.

## Findings

The loading presentation and analytical latency are separate issues. The prior
PR184 changes fixed presentation, not all first-statistic costs. Default Clonal
projection uses the shared cell renderer; default Abundance already has a native
count/Plotly path. Homeostasis and several other statistics call scRepertoire.

The tested Ren fixture has **no** adjacent
`cerebro_ren_covid_1.46m.viewer/immune/abundance.qs2` asset. Its immune sidecar is
34,908,440 bytes on disk, containing 194 samples / 503,432 repertoire rows and
occupying 294,719,648 bytes as an R object. A first statistics visit must therefore
read the full repertoire. The existing helper cannot use a nonexistent packed
summary. This is a property of this local fixture, not proof about the dataset
used by the GitHub reviewer or every deployment.

The corrected monotonic-clock R diagnostic measured:

| Isolated stage | Seconds |
| --- | ---: |
| Repertoire first materialization | 7.717 |
| First scRepertoire namespace load after package setup | 2.011 |
| Homeostasis calculation / ggplot construction | 1.876 |
| ggplot -> Plotly conversion | 0.250 |
| Plotly JSON serialization | 0.057 |
| Diversity export table, strict / both / 20 bootstraps, already loaded data and namespace | 0.352 |

These are diagnostic samples, **not browser timings or formal A/B medians**.
Do not add them to predict UI latency. Initial package/CRB loading is recorded
separately in `ren-monotonic/timings.csv`. The first `ren-baseline` diagnostic used
`proc.time()` and produced a negative Diversity elapsed value: its elapsed series
is invalid and retained only for audit. `profile.R` was corrected to use
`nanonext::mclock()`; the browser series used `performance.now()` throughout.

Rprof attributes about 80% of Homeostasis sampled self time to `order()` reached
through `table()`/factor construction. Repertoire materialization is dominated
by QS deserialization. Namespace loading spends substantial time in imports/S4
method registration. This does not establish that every IR statistic is slow:
the already-loaded Diversity table sample above was comparatively cheap.

## Level 1 candidate: rejected

Candidate C used existing backend chain descriptors to construct IR controls
without reading the full sidecar, and passed `ir_data()` lazily to the existing
pack-first Abundance helper. No analytical algorithm, ready signal, cache key,
payload format or dataset semantics was changed. The fixture had no relevant
pack asset, so Abundance still needed the sidecar.

Real Windows Brave, WSL R 4.6.1, scRepertoire 2.9.3, same 1424 x 1483 viewport,
same Demo -> Ren -> IR setup, new R process/tab per arm. Production harness plus
passive timing observations; see PROTOCOL.md. Controls remained gene / both /
None for default Abundance. Homeostasis followed Abundance in each session.

| Operation | B1 afterplot | B1 two-RAF acknowledgement | C1 afterplot | C1 two-RAF acknowledgement |
| --- | ---: | ---: | ---: | ---: |
| Abundance | 3.471 s | 3.486 s | 3.910 s | 5.240 s |
| Homeostasis | 3.857 s | 3.901 s | 4.083 s | 4.224 s |

Both arms actually rendered 194 Abundance traces and 5 Homeostasis traces with
no visible Shiny output error. The candidate Abundance acknowledgement included
1.33 seconds after afterplot; no browser-environment root cause is asserted.
Existing Ren Set2 palette-size warnings occurred. There is no evidence of an
end-to-end improvement, so C was restored byte-for-byte to B. No formal three-
or five-pair browser series was opened for this unsuccessful candidate. It is
not called a proven regression on the basis of one pair either.

## Bounded reader check: not adopted

qs2 0.3.1 defaults to one reader thread. An initial same-process 1/4-thread check
was confounded by R string interning/reuse: 6.506 s vs 1.037 s is **not** a valid
thread-scaling claim, although the decoded objects were identical.

Three subsequent interleaved pairs used a fresh R worker for every read:

| Pair | 1 thread | 4 threads | Saving |
| --- | ---: | ---: | ---: |
| 1 | 5.988 s | 5.621 s | 0.367 s |
| 2 | 4.991 s | 4.936 s | 0.055 s |
| 3 | 5.281 s | 4.724 s | 0.557 s |

The reader still takes roughly five seconds, with only 0.326 s average saving
in this small diagnostic. A hard-coded four-thread change is not justified by
that benefit. No production reader, global qs2 option or BPCells path was edited.

## Concrete next implementation boundaries

1. **First Abundance:** generate/validate the existing Viewer Pack assets when
   preparing the deployed dataset (`buildViewerPack` already exists), then
   validate the pack-first UI path with that exact artifact. Avoid eager
   repertoire reads in controls and renderer when the pack is present. Both the
   data artifact and call path are needed; the rejected code-only candidate
   proves neither a deployed fix nor an acceleration for unpacked Ren.
2. **Homeostasis:** a separate, bounded exact-count path could avoid sorting
   clone strings and loading scRepertoire for the default all-chain,
   ungrouped operation. It must first match scRepertoire output for missing
   clones, empty samples, custom thresholds, clone definitions and group
   ordering; non-default chain/group semantics should keep the validated
   fallback. This is a proposed next candidate, not an implemented speedup.
3. **Other statistics:** retain their statistical definitions and bootstrapping.
   Do not replace uncertainty estimates, prewarm synchronously on another page,
   add another large cache, or claim Projection-equivalent latency. Measure
   individual operations before selecting further work.

No Builder/content contract or Viewer Pack format change was made. No existing
dataset asset was overwritten. The production source tree is identical to the
starting HEAD. Only this investigation's scripts, compact evidence and report
are retained; no unrelated regression suite was repeated for unchanged code.
Temporary Brave tabs and owned WSL services were closed; no push.

## Evidence

- `B1/browser.jsonl`, `C1/browser.jsonl`: raw browser observations.
- `B1/source.csv`, `C1/source.csv`: exact source identities for both arms.
- `ren-monotonic/*.summary.csv`, `timings.csv`, `identity.txt`: corrected profile.
- `read-check.csv`, `read-pairs.csv`: reader experiment and fresh-worker check.
- `artifact-identity.csv`: dataset/sidecar/harness SHA256 identities.
- Full R logs, original Rprof files, original/candidate source bytes and rejected
  test draft are retained locally outside Git under
  `C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-11-ir-latency-52043c99/`.
