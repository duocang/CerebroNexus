# Full-source expression-backend benchmark methodology

## Research question

On complete million-scale public scRNA-seq matrices, how do CerebroNexus's `bpcells` and `h5` backends compare in backend construction, stored size, memory, hydrated startup, expression access, and standalone Viewer behaviour after the PR0-PR6 performance work?

This is descriptive engineering evidence. It does not compare biological methods, test statistical significance, control the operating-system page cache, or claim cross-machine generality.

## Sources and scope

The study uses every cell in two pinned public files: 1,306,127 cells from the 10x mouse brain E18 dataset (`GSE93421`, `SRP096558`) and 1,486,324 cells from the CELLxGENE PsychAD HBCC human prefrontal-cortex dataset (dataset `d27fb144-f105-46c2-b36f-f51421f74e4e`, collection `84ce6837-548d-4a1f-919f-0bc0d9a3952f`, DOI `10.1038/s41597-025-04687-5`). Downloads are reused only after byte-size and SHA-256 verification.

Neither source is sampled or cropped. `embedded` is excluded because both complete matrices exceed the 32-bit non-zero index limit of `Matrix::dgCMatrix`; the result is labelled not representable rather than inferred from a smaller tier.

## Experimental units

For each source/backend pair, three fresh R processes independently open the complete source, stream the backend, construct the Cerebro shell, and serialize the CRB. Backend order alternates across repeats. Each artifact is opened by two fresh access processes, yielding six access observations per source/backend. Every artifact is also passed through one fresh standalone App and browser process, yielding three Viewer observations per source/backend and 12 Viewer rows overall.

Processes run sequentially with a fixed thread count on an exclusive node. Reports show medians, observed minima/maxima, and independent-process `n`; no significance test is performed.

## Production artifact path

BPCells output uses CerebroNexus's gene-major writer. H5 output is streamed as a cells-by-genes TENx matrix under the production `expression` group. The shell is saved with `saveCerebro()` and its default qs2 codec. Fresh-process access uses `readCerebro()`, so qs2 decoding, schema-v2 validation, thin-CRB hydration, and relative sidecar attachment are included rather than bypassed with `readRDS()`.

Build metrics separate lazy source opening, backend writing, shell construction, CRB serialization, stored size, R heap, and whole-process peak RSS.

## Frozen access workload

A separate untimed process selects 12 expressed genes across the source's observed density range and records reference fingerprints. Every timed process uses the same immutable plan.

The access workload measures hydrated startup, the first full-cell single-gene read, warmed full-cell single-gene latency, one 12-gene-by-all-cells block, one single-gene read over up to 100,000 deterministic reverse-ordered non-contiguous cell indices, and the corresponding 12-gene subset block. The subset specifically exercises sorting and order restoration for BPCells and DelayedArray without replacing the complete source with a sampled dataset.

The first getter call is fresh-process but not controlled cold disk; the operating-system page cache may be warm. Every result is fingerprinted outside the timed expression. Any full or subset mismatch invalidates the run.

## Standalone Viewer workload

Each artifact runs through `createShinyApp()`, `runApp()`, the Shiny WebSocket, and Chrome. The driver records bundle and launch time, verifies the exact scheduled point count, requires WebGPU, rejects renderer errors and context loss, and records JavaScript heap use.

The driver then requires visible Canvas hover feedback, a non-empty box selection with server round-trip, zoom with an active minimap, frozen-gene switching with positive expression, and complete Linked Views readiness. All 12 browser rows are mandatory publication evidence. Vitessce and cross-browser comparison remain outside this study.

## Provenance and publication gate

The wrapper requires a clean worktree, an explicit storage description, a fixed thread count, and a source cache outside Git. Each run records Git SHA, package and dependency versions, R, OS, CPU, storage, source URLs, file sizes, and hashes. Missing rows, failed processes, mismatched plans, incorrect values, Canvas fallback, incomplete point counts, GPU failures, missing figures, or dirty publication state reject the run before immutable publication.

Validated evidence is published under `result/publication-full/runs/<run-id>/`, and `CURRENT` is updated only after all checks pass.

## Interpretation limits

- Synthetic metadata and sinusoidal projection coordinates isolate backend and Viewer engineering; they are not biological results.
- The 100,000-cell access subset is a query workload, not a sampled study cohort.
- Browser timings describe the recorded host and Chrome build only.
- Historical one-million-cell fixtures remain reproducibility artifacts for earlier PR comparisons and do not contribute to the paper benchmark.
