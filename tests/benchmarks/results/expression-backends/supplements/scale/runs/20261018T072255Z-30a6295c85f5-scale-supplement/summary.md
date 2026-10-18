# Expression-backend benchmark on real public datasets

**Run:** `20261018T072255Z-30a6295c85f5-scale-supplement`  
**Profile:** `scale`  
**Git:** `30a6295c85f57b56e889c85d6aa34c362a50173b`

> **Evidence status.** Benchmark evidence: backend comparisons use independent process repeats and correctness fingerprints.

## Sources

| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |
|---|---:|---:|---:|---:|---:|:--:|
| 10x mouse brain E18 | 1,306,127 | 27,998 | 2.625e+09 | 2010 | 29.3 GB | **no** |
| human PFC cross-disorder (HBCC) | 1,486,324 | 34,176 | 6.112e+09 | 4112 | 68.3 GB | **no** |

## Export

Values are median [minimum-maximum], followed by the number of independent export processes.
Embedded export includes the Seurat exporter, aggregations and serialization; disk-backend construction is timed separately from shell construction and serialization. These are different operation boundaries, not a direct backend-writer speed comparison.

| source | cells | backend | total MiB | build/export s | shell s | CRB serialization s | peak R heap MiB | peak process RSS MiB |
|---|---:|---|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 5e+05 | embedded | 3211.1 [3211.1-3211.1], n=5 | 53.9 [49.2-54.8], n=5 | -- | -- | 99309 [99309-99309], n=5 | 99487 [99485-99488], n=5 |

## Runtime access

Access counts describe fresh processes nested within independently built artifacts (two processes per build), not independent build replicates.
Timing contract: materialized-v2. All timed results are materialized before the timer stops.
The first-query metric is the first getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MiB | peak process RSS MiB | first query s | warmed p50 s | warmed p95 s | 12-gene full block s | shuffled 100k row s | shuffled 100k block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 5e+05 | embedded | 20.15 [19.74-23.94], n=10 | 23189 [23189-23189], n=10 | 24408 [24407-24409], n=10 | 3.1782 [3.1535-4.1174], n=10 | 3.1678 [3.1617-3.1846], n=10 | 3.8070 [3.2401-5.0244], n=10 | 3.275 [3.269-3.450], n=10 | 1.7231 [1.7204-2.5897], n=10 | 1.762 [1.760-2.639], n=10 |

Correctness: 10/10 access processes matched both source-matrix fingerprints.

## Host-specific scale-limit estimate

Across 1 distinct source/tier points, the median observed peak was 52.5 bytes per non-zero (range 52.5-52.5). This is a descriptive estimate for this exporter and host, not a universal memory law.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-10-18T09:23:10+0200 |
| git_branch | bench/human500k-embedded |
| git_dirty | false |
| package_version | 5.0.2 |
| r_version | R version 4.6.1 (2026-06-24) |
| os | Linux 6.8.0-136-generic #136-Ubuntu SMP PREEMPT_DYNAMIC Wed Jul  1 21:53:05 UTC 2026 |
| cpu | Intel(R) Core(TM) i7-14700 |
| logical_cores | 28 |
| benchmark_threads | 1 |
| slurm_job_id |  |
| slurm_node_list |  |
| slurm_cpus_per_task |  |
| slurm_memory_per_node |  |
| memory_mb | 128434.2 |
| r_vector_limit_mb | Inf |
| source `mouse_brain_e18` | 4,216,018,749 bytes; SHA-256 `255a36ee92de25cb3568faa2c27d31fe6d0db30f285c5c977be8d6245de14044` |
| source `human_pfc_hbcc` | 14,150,526,668 bytes; SHA-256 `aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f` |

