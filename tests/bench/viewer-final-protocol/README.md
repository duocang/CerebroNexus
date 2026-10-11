# Viewer final protocol v2 — 2026-10-10

Latest scoped acceptance: [FINAL_ACCEPTANCE.md](FINAL_ACCEPTANCE.md). Earlier
results retain their original source SHA and protocol. The supplemental Gene
operation-v2/v3, separate RSS probe and new canonical Spatial fixture below do
not replace or silently amend the frozen page harness.

The clean PR6 integration uses this protocol throughout. Results may be compared only when protocol version, harness SHA-256, dataset/sidecar/pack identity, browser engine and launch mode, R runtime, BPCells layout, viewport, cell percentage, and timing boundary match. Any change starts a new A/B series. Historical PR0–PR6 and `卧槽/*` measurements guide candidate choice but are not pooled with this series.

## Frozen environment

| Field | Value |
| --- | --- |
| Host | Windows 11 Pro 10.0.26200 x64; Intel Core i9-12900K (16 cores / 24 threads); 133,929,592 KiB visible RAM |
| Timing OS and R | WSL2 Linux 6.18.33.2; `/usr/local/bin/Rscript` 4.6.1 |
| Key R packages | Shiny 1.14.0, BPCells 0.3.1, chromote 0.5.1, shinytest2 0.5.1, qs2 0.3.1; save full `sessionInfo()` with every series |
| Timing browser | WSL Google Chrome 153.0.8010.52, chromote/shinytest2 headless fresh process; WebGL2 uses observed `--use-angle=swiftshader-webgl` software rendering, not hardware GPU; launch flags `--force-color-profile=srgb --disable-extensions --mute-audio` (the chromote `--disable-gpu` default is removed) |
| Brave compatibility | Windows Brave 154.1.96.59; functional checks only, never pooled with Chrome timing |
| Browser viewport | 1619 × 950 for `benchmark_viewer_1m_pages.R` |
| Shiny launch | New `AppDriver` and R process per observation; `devtools::load_all()` then `launchCerebro(mode="closed", percentage_cells_to_show=100, projections_show_hover_info=TRUE)` |
| Base commit | `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303` (`perf/pr6-ui-accessibility-polish`) |
| 1M source CRB SHA-256 | `bd668249342e4d9258faf0c94fc1d5cb1484aa4251e0eecbb48328269a85222b` |
| 1M page fixture CRB SHA-256 | `cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38`; 1,000,000 cells |
| 1M schema and cell fingerprint | Thin CRB schema v2; `md5-cell-set-v1:fa3fbe15b4aac9ce03ebcd5abb50acb4` |
| 1M BPCells sidecar SHA-256 | `dd3f3c155d585f89cbeba0baabb7f00f2d4a1f571905cde47f967d94c602bd9b` (harness fingerprint) |
| BPCells storage order | `row` in the 1M and Ren sidecar `storage_order` files |
| 1M Viewer Pack manifest SHA-256 | `a6b6f6b1bc35aa4f2fb9c2516532fd0df618ad238e686a753cd76038b7521aee`; schema 1; cell-order fingerprint `md5-cell-order-v1:396eed9b71d09c1304ef261c07045826` |
| Ren source CRB SHA-256 | `f8abf678730715c8214be94a74a4eacf8a25f7692e6dfa6f367203387321b4d6`; thin CRB schema v2; 1,462,702 cells; fingerprint `md5-cell-set-v1:afc13fc535ed8fa46dea95cb57245516`; verify matching BPCells sidecar before use |
| 1M harness SHA-256 | `benchmark_viewer_1m_pages.R`: `541c23fbcd80f2e1bd0dc55a57df2efdc7d80ae36c880db1674c8e7db351ceba`; `viewer_1m_page_protocol.R`: `fb9dfc5cf8e75e9e166af1d2510e341da36d1d15fd5f07272e3ac43d1ff0e276` |
| Browser and app variables | Record explicit `VIEWER_*`, `CHROMOTE_*`, `R_*`, `NOT_CRAN`, `DISPLAY` and GPU flags without credentials; unset differs from empty |
| Frozen timing renderer | `VIEWER_EXPECTED_RENDERER=webgl2` in WSL Chrome; compare GPU page rows only with `renderer_backend=webgl2` and `performance_applicable=TRUE`. Groups and HLA use their recorded non-GPU renderer paths. |

The 1M page fixture is generated **once** with `prepare_viewer_1m_pages.R` beside the source CRB and its BPCells directory. Before any formal A/B, record its CRB SHA-256, generator SHA-256, Viewer Pack manifest SHA-256, cell count, projection fingerprint, CRB schema/version and BPCells storage order. Do not regenerate within the series. Record candidate commit SHA, dirty status, package identity and harness SHA-256 on each run. A different Ren CRB is a different series even if cell count and filename match.

The v1 preflight observed `webgl2` for both PR6 and candidate in WSL Chrome. The old harness marked these observations `performance_applicable=FALSE` because its expected renderer was WebGPU. V2 explicitly pins the observed WebGL2 backend; later Brave/WebGPU observations require a separate series. The page fixture generator SHA-256 is `3e071f0989a6db92c706458c4acbfaed9ff90464bdbfc6a33101746cdd618655`. The one-round seven-page screen used code SHA `0e2ec540` and the final five-pair key-page A/B used `87c616c5` after the isolated HLA Data & QC table-host fix. Both use the same frozen harness and fixture, but results from distinct source SHA values are labelled separately. Memory polling perturbs timing, so its rows cannot be used as timing repeats. The HLA table fix has a passing contract test and a post-fix HLA page-entry smoke.

Observed WSL environment: `DISPLAY=:0`, `XDG_RUNTIME_DIR=/run/user/1000`; ambient `VIEWER_*`, `CHROMOTE_*`, `R_*`, `NOT_CRAN`, `EGL_*`, `LIBGL_*`, and `MESA_*` variables were unset. The page harness sets `NOT_CRAN=true`; timing sets `VIEWER_EXPECTED_RENDERER=webgl2`, `VIEWER_BENCH_MODE=timing` (default) and `VIEWER_BENCH_PROFILE=quick` (default). The separate memory run sets `VIEWER_BENCH_MODE=memory`; its rows record `profiler_instrumented=TRUE`. The post-fix HLA smoke explicitly sets `VIEWER_PAGES_ONLY=hla` and `VIEWER_VISITS_ONLY=first`. Dataset and output paths are in each raw manifest. The five-pair key-page A/B explicitly sets `VIEWER_PAGES_ONLY=overview,gene_expression`, `VIEWER_VISITS_ONLY=first,repeat`, `VIEWER_EXPECTED_RENDERER=webgl2` and five rounds. Its separate one-round resource A/B sets `VIEWER_BENCH_MODE=memory` and `VIEWER_VISITS_ONLY=first`. The candidate's `git_dirty=TRUE` in these two manifests is caused by untracked benchmark output/log files already being written in the worktree; `git diff` of tracked source was empty and the candidate commit SHA was fixed.

## Levels

| Level | Gate | Runs | Purpose |
| --- | --- | --- | --- |
| 1 — development | Every small candidate | Demo correctness; affected page; 1 baseline + 1 candidate; targeted profile if needed | Direction filter, not a formal performance claim |
| 2 — candidate A/B | Level 1 shows clear gain and no correctness/RSS concern | 1M, three interleaved pairs B1/C1/B2/C2/B3/C3 | Normal accept/reject gate |
| 3 — final acceptance | Final candidate, risky cache/lifecycle, architecture or major gain | Five interleaved pairs on affected important pages; targeted long session | Merge gate |

Stop after Level 1 if there is no clear gain, a correctness failure, obvious memory increase, or excessive complexity. Use 1M for generic Viewer work. Ren is reserved for expression access, single/Mean/Separate/RGB, metadata joins, wide metadata, Immune/HLA/receptors and extreme memory. Spatial uses the separate fixed 1,000,000-cell 1000×1000 canonical grid, image and fingerprint, and runs only when Spatial code changes.

## Boundaries and cache states

Every formal page row has separate click→request, server prepare, serialize/transfer, browser decode, first draw, first usable, complete and quiet fields. Use request identity and `performance.now()` where available. Missing stages are `NA`, never inferred by subtracting unrelated clocks. First usable requires correct visible primary content and a working interaction path. Complete requires all required visible content and no visible errors. Quiet is a separate one-second observation after complete. Global Shiny idle is **never** a page-ready or complete boundary. The v2 page harness uses its request-bound primary event as completion where no explicit progressive completion event exists.

Report cold first (fresh R/browser and declared empty cache), warm first (static/OS cache warm but page unvisited), revisit (return to a visited page) and repeated operation (second gene/color/selection) separately. The existing 1M page harness's `fresh` is warm-filesystem, fresh-process first after dataset load; label it `warm first`, never `cold first`.

## Correctness and resources

Each candidate checks point count, cell identity/order, projection fingerprint, color mapping, NA, zero, hover, box selection, return visit and dataset switch. Expression changes also check exact values, duplicate gene, RGB, Mean, Separate and empty state. Trajectory changes check cell ID↔coordinate mapping. Lifecycle changes check stale-result rejection, selectors and reconnect.

Level 1 records R/browser peak RSS, payload bytes and message count. Level 2 adds after-operation RSS, repeated operations, dense materialization and major copies. Level 3 adds 5–10 page switches, dataset switches, cache cleanup, reconnect and retained memory. If RSS polling perturbs latency, use separate timing and memory modes and label them. Report all raw values, median and min–max; failed/incomplete runs remain in raw data but never become successful timings. Every decision is `ACCEPTED`, `REJECTED`, `INCONCLUSIVE` or `BLOCKED`.

## Supplemental operation and Spatial protocols

- `probe-gene-operations.R`, operation-v2, SHA-256
  `c52c450781b59a1ffd61f43d0c441c9f125577f45202c05fd749b257b864a8df`:
  event-time point count plus expected label/pane count. Three interleaved
  baseline/candidate pairs use `eb432a11`/`bad37fca`. RGB's intermediate-mode
  frame makes its v2 boundary invalid; exclude RGB latency/completed-traffic
  conclusions. Other operations retain their explicit boundaries.
- `probe-gene-operations-v3.R`, SHA-256
  `ea8b67058bf9a6a6069b6a247d81e4bef86f18d79d9096f06b4a02d18d54bb96`:
  RGB additionally requires the complete three-channel render key. One pair is
  a screening check only. Both operation protocols use 1 s websocket quiet as
  an observation-window end, not complete; late traffic can cross windows.
  Unavailable operation stages and complete are `NA`.
- The separate generated resource probe has its own harness hash and
  `gene-operations-v3-memory` manifest. Its 50 ms recursive process RSS polling
  starts after AppDriver startup. Exclude its latency rows from timing results.
  `probe-gene-memory.R` SHA-256 is
  `c92ea9e3106efb9ad060ccde08d77fce8f3d9f2953c1da5a43cf9d670076b5c2`.
- `check-session.R` verifies eight demo dataset switches with actual Projection
  draws, then disconnect/document reload/new-epoch rejection. Standalone Shiny
  disables resumable reconnect; reload is not advertised as session resumption.
- `prepare-fixed-spatial.R` generates the independent `spatial-canonical-grid-v1`
  fixture once: 1,000,000 cells, canonical 1000×1000 grid and fixed image. CRB
  SHA-256 `e261ef7053c9b57d8b41b84ece40e3ed492dc0bdf5ab551cff447bd0f200095f`,
  image `54659fc75addc7145b40607ff8ea6cd9ec015c708adcda080bbcc6dd6c6ad717`,
  generator `9c3e8ebe79011b5947cbceb8fbda852986b8bdd8e6598c597a0354a20a230e1a`.
  The historical fixture is unavailable; this starts a new series. Its frozen
  page-harness A/B compares original PR6 and clean PR6 on the same new fixture.
  Historical ~1.7 s observations are not pooled with it.
