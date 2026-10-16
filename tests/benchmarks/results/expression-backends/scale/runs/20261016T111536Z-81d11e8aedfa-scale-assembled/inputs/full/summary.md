# Expression-backend benchmark on real public datasets

**Run:** `20261015T213433Z-81d11e8aedfa-full`  
**Profile:** `panel_c2`  
**Git:** `81d11e8aedfae380516f16e4f95c29728fbad02a`

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
| human_pfc_hbcc | 1,486,324 | bpcells | 27692.8 [27692.8-27692.8], n=5 | 467.4 [462.2-469.5], n=5 | 2.16 [2.07-2.21], n=5 | 14.67 [14.29-15.09], n=5 | 1060 [1060-1060], n=5 | 1920 [1919-1921], n=5 |
| human_pfc_hbcc | 1,486,324 | h5 | 70662.6 [70662.6-70662.6], n=5 | 564.5 [554.8-575.9], n=5 | 1.53 [1.49-1.59], n=5 | 14.24 [13.90-14.88], n=5 | 1260 [1260-1260], n=5 | 2044 [2043-2045], n=5 |
| mouse_brain_e18 | 1,306,127 | bpcells | 2904.1 [2904.1-2904.1], n=5 | 143.0 [130.3-144.3], n=5 | 1.96 [1.91-2.38], n=5 | 11.80 [11.69-14.03], n=5 | 968 [968-968], n=5 | 1825 [1825-1826], n=5 |
| mouse_brain_e18 | 1,306,127 | h5 | 30451.4 [30451.4-30451.4], n=5 | 174.7 [169.8-199.5], n=5 | 1.69 [1.53-3.52], n=5 | 14.17 [12.49-23.06], n=5 | 1115 [1115-1115], n=5 | 1982 [1982-1982], n=5 |

## Runtime access

Access counts describe fresh processes nested within independently built artifacts (two processes per build), not independent build replicates.
Timing contract: materialized-v2. All timed results are materialized before the timer stops.
The first-query metric is the first getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MiB | peak process RSS MiB | first query s | warmed p50 s | warmed p95 s | 12-gene full block s | shuffled 100k row s | shuffled 100k block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 4.53 [4.39-5.32], n=10 | 796 [796-796], n=10 | 1803 [1803-1806], n=10 | 1.1050 [1.0367-1.3472], n=10 | 0.8581 [0.8093-0.9127], n=10 | 0.9295 [0.9109-1.1214], n=10 | 1.069 [1.017-1.175], n=10 | 0.1137 [0.0997-0.1361], n=10 | 0.144 [0.128-0.159], n=10 |
| human_pfc_hbcc | 1,486,324 | h5 | 8.22 [7.73-9.55], n=10 | 1005 [1004-1005], n=10 | 1650 [1633-1651], n=10 | 0.8521 [0.8191-0.9528], n=10 | 0.1035 [0.0989-0.1099], n=10 | 0.1494 [0.1399-0.1783], n=10 | 0.511 [0.490-0.596], n=10 | 0.0146 [0.0133-0.0266], n=10 | 0.433 [0.403-0.589], n=10 |
| mouse_brain_e18 | 1,306,127 | bpcells | 4.60 [4.47-5.48], n=10 | 772 [772-773], n=10 | 1501 [1500-1501], n=10 | 0.7631 [0.7179-0.9002], n=10 | 0.6436 [0.6146-0.7806], n=10 | 0.7564 [0.7148-1.1784], n=10 | 0.906 [0.874-1.132], n=10 | 0.1993 [0.1768-0.2466], n=10 | 0.135 [0.119-0.170], n=10 |
| mouse_brain_e18 | 1,306,127 | h5 | 6.82 [6.55-9.04], n=10 | 973 [973-973], n=10 | 1444 [1401-1545], n=10 | 0.7122 [0.6814-0.9958], n=10 | 0.0306 [0.0273-0.0517], n=10 | 0.1241 [0.1117-0.2738], n=10 | 0.396 [0.364-0.432], n=10 | 0.1047 [0.0946-0.1217], n=10 | 0.242 [0.226-0.270], n=10 |

Correctness: 40/40 access processes matched both source-matrix fingerprints.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-10-15T23:34:47+0200 |
| git_branch | paper/real-data-benchmark |
| git_dirty | false |
| package_version | 5.0.0 |
| r_version | R version 4.6.1 (2026-06-24) |
| os | Linux 6.18.54.1-microsoft-standard-WSL2 #1 SMP PREEMPT_DYNAMIC Tue Oct  6 19:13:07 UTC 2026 |
| cpu | 12th Gen Intel(R) Core(TM) i9-12900K |
| logical_cores | 24 |
| benchmark_threads | 1 |
| slurm_job_id |  |
| slurm_node_list |  |
| slurm_cpus_per_task |  |
| slurm_memory_per_node |  |
| memory_mb | 112684.9 |
| r_vector_limit_mb | Inf |
| source `mouse_brain_e18` | 4,216,018,749 bytes; SHA-256 `255a36ee92de25cb3568faa2c27d31fe6d0db30f285c5c977be8d6245de14044` |
| source `human_pfc_hbcc` | 14,150,526,668 bytes; SHA-256 `aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f` |

