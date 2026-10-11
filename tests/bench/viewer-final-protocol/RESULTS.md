# Clean PR6 Viewer results

Protocol: [v2](README.md). Historical reports and v1 preflight remain separate.

## Five-pair key-page A/B, v2 final gate

Baseline `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`; candidate `87c616c5a0491f21fe666fc5b78726dcb84711b9`. Harness SHA-256 values `541c23fbcd80f2e1bd0dc55a57df2efdc7d80ae36c880db1674c8e7db351ceba` and `fb9dfc5cf8e75e9e166af1d2510e341da36d1d15fd5f07272e3ac43d1ff0e276`; fixture SHA-256 `cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38`. Five balanced B/C pairs per page and visit, 40 warm-first/revisit observations, all with `status=ok`, `correctness_pass=TRUE`, the same 1M fingerprint and WebGL2. `final-key-pages-ab-v2.tsv` is the raw timing file; schedule and manifests are adjacent. Values below are seconds, median [min–max]. Paired delta is candidate minus baseline in milliseconds.

| Page / visit | PR6 first usable | Clean first usable | Paired delta median [range] ms | Complete | Conclusion |
| --- | ---: | ---: | ---: | --- | --- |
| Projection / warm first | 1.813 [1.792–2.070] | 1.785 [1.752–2.096] | −40 [−291, +303] | Within 1 ms of usable | INCONCLUSIVE: no stable page-speed gain |
| Projection / revisit | 0.059 [0.034–0.066] | 0.063 [0.060–0.070] | +9 [−3, +28] | Within 1 ms of usable | INCONCLUSIVE: small absolute difference |
| Gene entry / warm first, no gene operation | 2.106 [2.057–2.328] | 2.111 [2.043–2.228] | −14 [−152, +122] | Within 1 ms of usable | INCONCLUSIVE: same page latency |
| Gene entry / revisit | 0.544 [0.524–0.582] | 0.539 [0.504–0.559] | −20 [−33, +10] | Within 1 ms of usable | INCONCLUSIVE: no stable meaningful gain |

The existing harness budget of 2.0 s for first entry and 0.5 s for Gene revisit marked 22 of 40 rows `pass=FALSE` across both arms; the process exited nonzero **after** writing complete raw results. All 40 rows passed content correctness. No budget was loosened. The user goal of about 3 s for Projection/Gene first usability is met for median page entry, but this is not a single-gene operation measurement. Primary payload median was PR6→clean: Projection 2,756→2,764 B and Gene 1,684→1,720 B; median full websocket receive 32,382→32,394 B and 22,224,796→22,224,868 B. These small increases are recorded, with no claimed transport improvement.

Separate one-round memory A/B (`final-key-pages-memory-ab-v2.tsv`) used the same CRB/harness and first-page state; its latency is excluded from the table above. Projection R peak was 1,573.7→1,583.4 MiB and browser peak 1,899.8→1,904.8 MiB. Gene R peak was 1,684.8→1,684.8 MiB and browser peak 2,067.1→2,083.9 MiB. Four memory rows passed correctness. This rules out an obvious peak-RSS regression in those two pages; one pair cannot prove memory equivalence or long-session cleanup. The candidate's `git_dirty=TRUE` flag reflects only untracked benchmark outputs/logs created during the run; there was no tracked source diff from its recorded SHA. **Conclusion: ACCEPTED** for these key pages' correctness and absence of a large latency/RSS regression; **INCONCLUSIVE** for an overall page-speed improvement.

## V1 Level-1 preflight, excluded from v2 acceptance

All rows below use the fixed 1M page fixture SHA-256 `cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38` and PR6 baseline `cbd65fe0`. The candidate was dirty at `73f746ac` while small changes were being screened. WSL Chrome selected **WebGL2** in every observation; the harness correctly flags `performance_applicable=FALSE` for its WebGPU target. Timing mode does not sample peak RSS. These are single-pair direction checks, not medians. Raw rows and manifests are saved beside this document.

| Page, warm first | PR6 first usable / complete | Candidate first usable / complete | First draw | Primary payload | Asset / websocket received | Correctness | Conclusion |
| --- | ---: | ---: | ---: | ---: | ---: | --- | --- |
| 1M Projection | 1.741 / 1.741 s | 1.763 / 1.763 s | 125.1 / 118.7 ms | 2,756 / 2,756 B | 8.0 MB projection both arms | 1,000,000 points and same fingerprint | INCONCLUSIVE |
| 1M Gene entry, no gene operation | 2.114 / 2.114 s | 2.159 / 2.159 s | 101.9 / 88.2 ms | 1,684 / 1,684 B | 8.0 MB projection; websocket 22,224,809 / 22,224,796 B | 1,000,000 points and same fingerprint | INCONCLUSIVE |

The hover micro used the source 1M CRB SHA-256 `bd668249342e4d9258faf0c94fc1d5cb1484aa4251e0eecbb48328269a85222b`: `formatC` on all 1M rows took 40.555 s, distinct-value formatting took 8.007 s, and all 1M hover strings were `identical()` (232,049,616 B). It is a targeted CPU result, not first-usable improvement on a Viewer Pack page that already defers hover work.

Ren parser probe used CRB SHA-256 `f8abf678730715c8214be94a74a4eacf8a25f7692e6dfa6f367203387321b4d6`: 503,432 receptor rows across 194 tables, exact TRB/TRA output. In separate fresh processes, baseline maximum RSS was 1,907,080 KiB and candidate 1,679,016 KiB; parser wall times were TRB 7.092 / 5.750 s and TRA 5.157 / 2.992 s. A same-process graph-guard probe on 220,968 TRB rows and 154,641 unique CDR3s took 2.239 / 0.748 s and returned `identical()` guard objects. These Level-1 results support the HLA mechanism, but do not establish full HLA page latency or browser memory.

### Ren parser three-pair targeted A/B

Baseline source `cbd65fe0`; candidate source `000bf411`; same Ren CRB and `probe-hla-parser.R` in three fresh R processes. Each process ran baseline then candidate and asserted `identical()` complete output. This is a targeted Level-2 mechanism result, not a page timing. The scheduled B/C order is B1/C1/B2/C2/B3/C3 within each chain's three process runs.

| Chain | Baseline seconds, median [range] | Candidate seconds, median [range] | Correctness | Conclusion |
| --- | ---: | ---: | --- | --- |
| TRB | 6.165 [6.141–6.218] | 1.344 [1.289–3.234] | 220,968 rows, identical all three | ACCEPTED |
| TRA | 4.462 [4.329–4.497] | 2.823 [2.758–4.748] | 220,968 rows, identical all three | ACCEPTED |

The v2 1M HLA Pack first-entry screen returned 1.828 s for PR6 and 1.893 s for the candidate, both correct. The candidate did not show a useful 1M page gain, so the planned extra pairs were stopped per the Level-1 gate. This page already consumes a precomputed HLA Pack frame; it does not exercise the Ren parser fallback. **Conclusion: INCONCLUSIVE** for end-to-end HLA speed.

## Final 1M candidate screening, v2

Code SHA `0e2ec540961030ee4b43382b4d36942b368538e0`; fixed page fixture, renderer and harness identities are in `README.md` and the raw manifests. `final-1m-v2.tsv` contains **one** warm-first and revisit observation per page. The separate `final-1m-memory-v2.tsv` records first-page peak RSS with polling enabled; its latency numbers are deliberately excluded. A later narrow HLA Data & QC table-host fix has a passing contract test and its own `hla-data-qc-smoke-v2.tsv` HLA page-entry observation (1.851 s, correct; the Data & QC tab itself was not clicked). All first-page rows had `status=ok`, the correct dataset fingerprint and visible content. These single observations are not medians or a formal Level-3 A/B.

| Page | First usable s | Complete s | Revisit usable / complete s | R peak MiB | Browser peak MiB | Primary payload B | Total websocket receive B | Correctness / budget |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| Projection | 1.774 | 1.774 | 0.058 / 0.058 | 1,584 | 1,921 | 2,764 | 32,428 | pass / pass |
| Gene entry, no gene operation | 2.081 | 2.081 | 0.604 / 0.604 | 1,685 | 2,113 | 1,724 | 22,224,881 | pass / **old 2.0 s first and 0.5 s revisit budgets fail** |
| Groups | 1.636 | 1.636 | 0.011 / 0.011 | 1,344 | 1,770 | see raw | 621,147 | pass / pass |
| Linked | 1.706 | 6.595 | 0.037 / 3.524 | 1,494 | 2,175 | 2,608 | 16,611,716 | pass / pass |
| Immune | 1.831 | 1.831 | 0.051 / 0.051 | 1,491 | 1,858 | 1,601,288 | 1,623,518 | pass / pass |
| Trajectory | 2.663 | 2.663 | 0.084 / 0.084 | 1,440 | 1,910 | 8,838,651 | 8,846,026 | pass / pass |
| HLA | 1.972 | 1.972 | 0.035 / 0.035 | 1,669 | 1,831 | 412,484 | 441,464 | pass / pass |

Memory numbers are rounded from KiB and belong to different process runs than the timing numbers. The memory run was source-dirty only because the raw timing files were untracked; its code SHA was unchanged. The timing command returned a budget failure for Gene, though both Gene rows had `status=ok` and `correctness_pass=TRUE`. Gene entry meets the requested aspirational 3-second goal, but that does **not** establish a single-gene operation below 3 seconds. Linked's secondary completion remains a 3.5–6.6 s bottleneck. The main page harness records click→request, server prepare, transfer, decode and first draw where event clocks exist; see the raw TSV. `quiet` is unmeasured (`NA`), never substituted with global Shiny idle. First/revisit are warm-filesystem fresh-process states; true cold-first and second-gene operations were not measured here.

**Conclusion: ACCEPTED** for the clean baseline's screened correctness and page usability. The five-pair v2 A/B above finds no meaningful Projection or Gene-entry gain over PR6; a quantitative speedup versus the current `卧槽/pr5_viewer_requests` branch remains **INCONCLUSIVE**. The historical current-branch 1M Projection first usable 109.983 s and Gene single 97.192 s used a different CRB, browser/renderer, harness and readiness contract; no trustworthy speedup factor can be computed from those numbers.

## Ren expression and lifecycle checks

The Ren probe used the fixed 1,462,702-cell CRB above and `probe-ren-expression.R` (SHA-256 `3cd0abd71b7bf77ac7bcbaf72e84d665fc981afec38ccc1bc2252bdd8eb83ffc`). MALAT1/ACTB/GAPDH three-row block was 2.436 s; single-row MALAT1 was 1.311 s; maximum process RSS was 1,834,792 KiB. The exact single versus block values, duplicate-gene values, Mean length, three Separate vectors, and all dense-f64 color packet round-trips passed. This intentionally materialized only three rows, not the whole expression matrix. The probe is a **Level-1 correctness/stress check**, not a Ren page A/B or browser result. **Conclusion: ACCEPTED** for exact data transport; **INCONCLUSIVE** for Ren Viewer page latency.

`lifecycle-v2.tsv` records six alternating demo dataset switches, generations 1–7, exact cell counts/fingerprints and a reconnect sync check. After-action R RSS was 1,223.375→1,223.422 MiB; browser RSS was 1,749.211→1,759.727 MiB, fluctuating rather than rising monotonically. The browser/R peak values for 1M are in the table above. **Conclusion: ACCEPTED** for the tested selector and stale-generation behavior; long-session retention beyond these switches remains unmeasured.

No Spatial source was changed, so the separate canonical-grid Spatial benchmark was correctly skipped. Historic ~1.7 s Spatial and ~1.5/0.6 s Trajectory numbers are outside v2 and are not pooled. The current branch's generic reader/chunking, old vector-parser variant, graph formatter, quantization and reverted shared-codec experiments were not retested or reintroduced. Further work should target Linked secondary 16.6 MB websocket traffic and real Gene operation latency; optimizing Pack-backed first-page hover or duplicating existing RGB/index/geometry paths has no measured value.
# Follow-up acceptance

The measurements above retain their original source identities. The subsequent
old-branch retirement, real Gene operation measurements, independent Spatial
fixture and functional acceptance are in `FINAL_ACCEPTANCE.md`. Statements
above about skipped Spatial testing apply to those historical source revisions.
