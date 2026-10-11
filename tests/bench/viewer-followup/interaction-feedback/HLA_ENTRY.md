# HLA small-bin thread startup correction

Date: 2026-10-10. Baseline: `d2d8b49a`, branch `卧槽/pr6_clean_perf`.
Same checkout; no push. User reported the current Demo app on port 8943 showing
an empty HLA page after roughly ten seconds.

## Finding and bounded change

The shipped Demo has TCR data. Its default minimum motif size is 4, which yields
no visible motif clusters. This is a legitimate analysis result, not missing
data. The empty explanation and terminal loading path already work, but first
entry still computes the network to establish that result.

A narrow server trace found 2.689 seconds inside raw graph construction in the
initial sample, out of 5.030 seconds from the sidebar input to the result flush.
The actual Demo TRB input has 580 parsed rows, 579 unique sequences, and nontrivial
length bins of 4–149 sequences. The WSL R environment configures stringdist with
23 threads. Running its exact distance matrices on these small bins took 4.153 s
with that default versus 0.010 s with one thread; matrices were identical.

`hla_process_length_group()` now uses one thread for bins of at most 256 sequences.
Larger bins retain the configured stringdist thread count. This conservative
small-bin bound is not a claim of an optimal universal crossover. No global
thread option changes. The standalone Viewer copy is byte-identical to the
package core. Hamming distance, edges, components, thresholds, layout, parser,
identity, precision, payload, caches, deferred loading and paint gates are unchanged.

## Validation

- `hla-small-bin-threads`: 20 assertions passed. Exact distance and edge agreement
  at 2, 149, 256 and 257 sequences; small/large thread selection and unchanged
  global option verified.
- Full `hla-motif-core` and `hla-app-contract` suites passed, including standalone
  core drift checks.
- Real Windows Brave, WSL R 4.6.1, Demo 1,476 cells. Native viewport was left
  unchanged, not forcibly normalized: candidate recovery screenshot is 1367 x
  858; the user's separate 8943 preview is 1438 x 1084. Same
  `hla-entry-probe.R` harness for baseline port 8945 and candidate port 8946.
  Probes use R trace hooks and input/flush observations only; no browser script
  injection, fake ready, or modified production scheduling.
- First candidate UI observation: empty explanation visible and loader hidden
  at 2.717 s. Three subsequent interleaved fresh-session A/B pairs follow.
  Process/package caches are warm; graph caches are session-local. These are
  warm-process first entries, not cold-process or revisit measurements.

| Pair | Baseline graph (s) | Candidate graph (s) | Baseline input → result flush (s) | Candidate input → result flush (s) |
| --- | ---: | ---: | ---: | ---: |
| 1 | 1.215 | 0.323 | 2.200 | 2.029 |
| 2 | 6.172 | 0.138 | 7.570 | 1.683 |
| 3 | 7.077 | 0.227 | 8.208 | 2.597 |

Server flush is not browser paint. Candidate browser observations were 2.313,
3.064 and 2.855 s to visible empty explanation, with the loader already hidden.
Baseline browser pair 1 was 3.363 s; pairs 2/3 exceeded the tool's approximately
three-second locator deadline despite the requested longer timeout. Their final
empty state and dismissed loader were checked separately; no invented whole-page
timings or whole-page speed ratio are reported. A candidate Data-info readiness
wait also hit the tool deadline, before timing began; the ready page was inspected
before continuing the same session. This does not affect server timestamps.

Enabling “Show unconnected CDR3s” recovered an actual painted network with 573
motifs and `View ready · No selection`, matching the baseline. The empty-result
message, default threshold and scientific interpretation remain unchanged.
Candidate browser warning/error capture is empty. The final diagnostic R sessions
had no warnings/errors. Temporary diagnostic servers and tabs were stopped.

The user's 8943 autoreloading preview rebuilt at `2026-10-10T22:08:32.406527`,
after the core edit at 22:08:28. It was refreshed and its HLA empty explanation
and dismissed loader were verified (`user-8943-final.json/png`). That locator
wait also exceeded the tool deadline, so this check supplies correctness and
version evidence only, not a new latency claim for port 8943.

Evidence: `implementation-evidence/hla-entry/` contains the user-page baseline
screenshot, exact matrix check, both server JSONL streams, extracted stage summary,
interleaved browser observations and network recovery. Failed instrumentation
setup attempts preceded the recorded baseline; their traces were discarded.

This removes the demonstrated thread-startup waste. It does not promise a fixed
page latency, eliminate ordinary initialization, or claim to solve the previously
observed Brave RAF variability. No unrelated Viewer benchmark was reopened.
