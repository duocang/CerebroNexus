# Real-data expression-backend benchmark

This directory contains the operator-facing harness for one pinned real source. It has two deliberately separate panels:

- **Panel A (`comparison`)** compares `embedded`, `bpcells`, and `h5` after all three receive the same materialized `dgCMatrix` at 1,000, 5,000, 10,000, 25,000, 100,000, 250,000 cells, and the frozen `common_target_actual` (the largest legal nested tier at or below 500,000 cells).
- **Panel B (`full_scale`)** measures only the public BPCells streaming path at `common_target_actual`, 1,000,000 cells, 2,000,000 cells, and all 4,140,453 cells. Its common-tier job starts from the H5AD-backed `IterableMatrix`, so it is a bridge observation rather than another Panel A replicate.

This harness is included in CerebroNexus 4.2. No reviewed full-scale results
are currently committed. It produces raw evidence; interpretation and
publication are separate, manual review steps.

## Pinned source

Use the MSSM human prefrontal cortex AnnData file. Its `/X` matrix contains normalized floating-point expression (`data`), not raw counts.

| field | required value |
|---|---|
| URL | `https://datasets.cellxgene.cziscience.com/0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad` |
| bytes | `36,092,176,654` |
| SHA-256 | `c62456941372b90bcf0df38e8cb1c34dd060bc5a507270ab1d068cbe6f1dfd54` |
| cells | `4,140,453` |
| H5AD group | `/X` (CSR) |

Download the file yourself, outside the repository, then verify it before running:

```sh
curl -fL --output /data/mssm.h5ad \
  https://datasets.cellxgene.cziscience.com/0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad
wc -c /data/mssm.h5ad
shasum -a 256 /data/mssm.h5ad
```

The setup worker resolves the source to one canonical regular-file path, checks its byte size, and streams the complete SHA-256 once. The parent checks a lightweight file identity (path, size, modification/change times, and mode) before and after every measured worker, then streams the complete SHA-256 once more before final validation. Thus the 36 GB source is hashed twice per panel, not once per job. A symlink, replacement, identity change, or final digest mismatch fails closed. The runner never downloads the source, and network transfer is never timed.

## Choose output paths first

Each real run requires a new output path that does not already exist. Its nearest existing parent must be outside the Git worktree. The runner creates a marked `scratch/` child and a run-local R library on that same filesystem, so the output path is also the operator's filesystem-capacity choice.

There is no automatic RAM or free-disk gate and no tier reduction after the schedule is frozen. An out-of-memory or disk-full event is recorded as a failed job. Per-repeat batching bounds complete live artifacts at 21 in Panel A and four in Panel B: each repeat completes all exports, then all accesses in the separately frozen access order, then removes the marked artifacts after preserving outcomes and logs.

After the clean-worktree gate, the runner copies `config.R` and `helpers.R` into the marked scratch directory. The original pre-copy hashes, frozen-copy hashes, and original post-copy hashes must agree. Setup and measured subprocesses load only that frozen harness and verify both files directly before and after sourcing; the parent verifies it before and after every worker. The final Git clean state and `HEAD` must still match the initial manifest.

## Commands

Dry runs need no source or prior manifest. They print an explicitly `UNQUALIFIED` target schedule; they do not validate exact nnz, source identity, or cross-panel linkage.

```sh
Rscript --vanilla tests/bench/run_comparison.R --dry-run
Rscript --vanilla tests/bench/run_full_scale.R --dry-run
```

Run Panel A first, validate its output, and do not modify that frozen evidence before or during Panel B:

```sh
Rscript --vanilla tests/bench/run_comparison.R \
  /data/mssm.h5ad \
  /bench-output/cerebronexus-panel-a-<git-sha>

Rscript --vanilla tests/bench/run_full_scale.R \
  /data/mssm.h5ad \
  /bench-output/cerebronexus-panel-a-<git-sha> \
  /bench-output/cerebronexus-panel-b-<git-sha>
```

Panel B refuses to start measured work unless Panel A has exactly 63 unique `OK` exports, 63 unique `OK` accesses, one final `VALID` gate, and exact matching source, Git, schema/config, frozen-harness, runtime, common-tier, sampling, shell, cell-identity, and query-plan fingerprints.

## Fixed schedules

Panel A has seven tiers × three backends × three repeats: 63 technical export/access pairs and 126 measured fresh processes. The export rotations are:

| repeat | tier order | backend order within each tier |
|---:|---|---|
| 1 | 1k, 5k, 10k, 25k, 100k, 250k, common | embedded, bpcells, h5 |
| 2 | 5k, 10k, 25k, 100k, 250k, common, 1k | bpcells, h5, embedded |
| 3 | 10k, 25k, 100k, 250k, common, 1k, 5k | h5, embedded, bpcells |

Its access rotations deliberately differ:

| repeat | tier order | backend order within each tier |
|---:|---|---|
| 1 | 5k, 10k, 25k, 100k, 250k, common, 1k | h5, embedded, bpcells |
| 2 | 10k, 25k, 100k, 250k, common, 1k, 5k | embedded, bpcells, h5 |
| 3 | 25k, 100k, 250k, common, 1k, 5k, 10k | bpcells, h5, embedded |

Panel B has four tiers × four repeats: 16 technical pairs and 32 measured fresh processes. Its tier rotations are:

| repeat | export order | access order |
|---:|---|---|
| 1 | common, 1M, 2M, full | 2M, full, common, 1M |
| 2 | 1M, 2M, full, common | full, common, 1M, 2M |
| 3 | 2M, full, common, 1M | common, 1M, 2M, full |
| 4 | full, common, 1M, 2M | 1M, 2M, full, common |

The centered four-stratum source samples are deterministic, source ordered, and nested. Exact `/X/indptr` counts decide whether the target common tier must be reduced below 500,000; 1k through 250k remain fixed, and `common_target_actual <= 250000` aborts the protocol. Hardware capacity never changes a tier.

## Fixture and measurement meaning

Every tier uses the same synthetic-shell generator. The real attributes in scope are normalized expression values, dimensions, and sparsity. `sample` (eight deterministic groups), `cluster` (32 deterministic groups), zero `nUMI`/`nGene`, and sine/cosine UMAP coordinates are synthetic functions of the selected source indices. They are not MSSM metadata, biological groups, QC measurements, or a biological projection.

Each access process times CRB loading, external attachment where applicable, one fully materialized first-gene query, five warmed calls of that same median-density gene, five-gene block preparation, and explicit block materialization. Correctness fingerprints are checked outside timed expressions. The five-gene panel is selected once from 1k and frozen across both panels.

For storage, `total_bytes` is the complete synthetic-shell artifact. BPCells and H5 report an external `sidecar_bytes`; embedded storage has no separable sidecar, so `sidecar_bytes` is `NA`, `sidecar_bytes_applicable` is false, and the reason is `embedded_has_no_sidecar`. Zero never represents structural non-applicability. Panel B's CRB and total size describe real expression in a synthetic shell, not the size of a complete original MSSM application.

## Evidence files and schemas

Every CSV has a fixed typed schema even when it has no rows. Identifiers link tables through `pair_id`, `tier_label`, source/sampling/query hashes, and the run manifest. `query-plan.rds` is a canonical typed payload, not a CSV and not a copy of millions of cell names.

- `manifest.csv`: `key` and `value`. Its keyed rows record the Git SHA/clean-state gate, schema/configuration, canonical `source_path`, frozen `config.R` and `helpers.R` hashes plus their combined harness fingerprint, command and timestamps; R version/platform and host information; sorted dependency versions, runtime fingerprint, and diagnostic package paths. Paths are excluded from the cross-panel runtime hash because run-local roots differ.
- `source.csv`: `source_key`, `source_url`, `expected_bytes`, `actual_bytes`, `expected_sha256`, `actual_sha256`, `n_cells`, `n_genes`, and `exact_nnz`.
- `sampling.csv`: `tier_label`, `n_cells`, `stratum`, `start`, `end`, `n`, `exact_nnz`, `indices_sha256`, `cell_identity_sha256`, and `shell_sha256`. There are exactly four boundary rows per tier.
- `eligibility.csv`: `panel`, `tier_label`, `n_cells`, `backend`, `exact_nnz`, `status`, and `reason`.
- `queries.csv`: `schema`, `tier_label`, `source_sha256`, `sampling_sha256`, `n_genes`, `n_cells`, `gene`, `role`, `density`, `source_row`, `tie_break_rank`, `ordered_indices_sha256`, `cell_identity_sha256`, `first_row_numeric_sha256`, `block_numeric_sha256`, and `query_plan_sha256`.
- `schedule.csv`: `pair_id`, `panel`, `repeat`, `tier_label`, `n_cells`, `backend`, `export_order`, and `access_order`.
- `export.csv`: `pair_id`, `status`, `failure_stage`, `error`, `exit_status`, `log_path`, `package_path`, `peak_rss_mb`, `r_heap_peak_mb`, `elapsed_secs`, `artifact_path`, `source_open_secs`, `comparison_materialize_secs`, `seurat_shell_secs`, `export_secs`, `crb_bytes`, `sidecar_path`, `sidecar_bytes`, `total_bytes`, `sidecar_bytes_applicable`, `sidecar_bytes_reason`, and `shell_sha256`.
- `access.csv`: `pair_id`, `status`, `failure_stage`, `error`, `exit_status`, `log_path`, `package_path`, `peak_rss_mb`, `r_heap_peak_mb`, `elapsed_secs`, `artifact_path`, `crb_load_secs`, `backend_attach_secs`, `backend_attach_applicable`, `backend_attach_reason`, `first_query_secs`, `warmed_query_1_secs`, `warmed_query_2_secs`, `warmed_query_3_secs`, `warmed_query_4_secs`, `warmed_query_5_secs`, `warmed_median_secs`, `block_prepare_secs`, `block_materialize_secs`, `block_ready_secs`, `expected_row_fingerprint`, `observed_row_fingerprint`, `expected_block_fingerprint`, `observed_block_fingerprint`, and `correctness`.
- `validation.csv`: `check_id`, `panel`, `scope`, `expected`, `observed`, `status`, and `detail`. It ends with exactly one `check_id = panel_valid` row: `VALID` only when every preceding required gate is `PASS`, otherwise `INVALID`.
- `logs/`: `install.log`, `setup.log`, and one retained export or access log for every measured worker.

Once `manifest.csv`, `source.csv`, `sampling.csv`, `eligibility.csv`, `queries.csv`, `query-plan.rds`, and `schedule.csv` pass their canonical checks, the runner snapshots their on-disk hashes. It verifies all seven files before and after every worker and rereads them from disk under the same snapshot for final validation. They exclude `validation.csv`, avoiding a self-hash cycle.

Eligibility and worker outcomes use different vocabularies. Eligibility is only `SCHEDULED`, `NOT_APPLICABLE_PROTOCOL`, or `UNSUPPORTED_DGCMATRIX_INDEX`. Export/access outcomes are `OK`, `FAILED_<stage>`, or (access only) `NOT_RUN_EXPORT_FAILED`; `MISSING_RESULT` is created only by validation. Every `OK` row must also contain zero exit status, absolute newline-free evidence paths, finite nonnegative R-heap, elapsed, and all other applicable timing/storage metrics, exact applicability markers and arithmetic, and matching correctness fingerprints; export/access artifact and package paths must agree for the pair. `peak_rss_mb` alone may be `NA` when the parent process captured no finite `ps` sample for a short successful worker, and otherwise must be finite and nonnegative. A duplicate, unscheduled or missing row, failed/not-run scheduled job, malformed `OK` row, incorrect fingerprint, provenance mismatch, or incomplete full-tier set makes the panel invalid. Raw rows and logs remain even when the runner exits non-zero. A measured-runner, cleanup, or integrity exception writes a best-effort `FAIL` plus final `INVALID` validation without replacing the original error.

## Interpretation limits

These are single-host technical pairs. The OS page cache is uncontrolled, and query-plan setup necessarily opens and warms the local source; “first query” is not a cold-disk claim. Five warmed calls are repeated operations inside one process, not five replicates. Peak RSS is sampled every 500 ms, not an exact kernel high-water mark.

Report raw rows and descriptive median `[min, max]` summaries with technical-pair `n = 3` for Panel A or `n = 4` for Panel B. Do not compute inferential uncertainty, pool the two common-tier BPCells routes, extrapolate capacity, fit a scaling law, or draw biological conclusions. The five-gene fingerprint gate proves agreement only for that frozen query panel; it is not a full-matrix equality claim.

This harness has no profile system, generic resource gate or planner, plot/report pipeline, mutable `CURRENT` pointer, automatic download, automatic resume, automatic publication, or automatic result commit.
