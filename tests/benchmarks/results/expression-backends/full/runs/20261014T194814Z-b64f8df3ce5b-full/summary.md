# Expression-backend benchmark on real public datasets

**Run:** `20261014T194814Z-b64f8df3ce5b-full`  
**Profile:** `panel_c2`  
**Git:** `b64f8df3ce5bc553b7084dfdaca3698aa69f475c`

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
| human_pfc_hbcc | 1,486,324 | bpcells | 27692.8 [27692.8-27692.8], n=5 | 495.7 [491.2-583.5], n=5 | 3.39 [3.01-3.43], n=5 | 17.49 [16.55-18.70], n=5 | 1060 [1060-1060], n=5 | 1930 [1930-1931], n=5 |
| human_pfc_hbcc | 1,486,324 | h5 | 70662.6 [70662.6-70662.6], n=5 | 608.8 [593.5-675.9], n=5 | 1.76 [1.72-1.92], n=5 | 18.52 [17.54-20.50], n=5 | 1277 [1277-1277], n=5 | 2067 [2067-2068], n=5 |
| mouse_brain_e18 | 1,306,127 | bpcells | 2904.1 [2904.1-2904.1], n=5 | 141.2 [140.4-146.6], n=5 | 2.13 [2.08-2.23], n=5 | 13.37 [12.99-17.35], n=5 | 956 [956-956], n=5 | 1835 [1834-1836], n=5 |
| mouse_brain_e18 | 1,306,127 | h5 | 30451.4 [30451.4-30451.4], n=5 | 188.0 [184.2-200.4], n=5 | 2.02 [1.75-2.36], n=5 | 14.02 [13.24-14.62], n=5 | 1181 [1181-1181], n=5 | 1982 [1981-1982], n=5 |

## Runtime access

Access counts describe fresh processes nested within independently built artifacts (two processes per build), not independent build replicates.
Timing contract: materialized-v2. All timed results are materialized before the timer stops.
The first-query metric is the first getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MiB | peak process RSS MiB | first query s | warmed p50 s | warmed p95 s | 12-gene full block s | shuffled 100k row s | shuffled 100k block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 5.39 [4.88-5.59], n=10 | 799 [799-799], n=10 | 1755 [1749-1755], n=10 | 1.2770 [1.1750-1.4280], n=10 | 0.9845 [0.9460-1.0370], n=10 | 1.0829 [1.0014-1.6078], n=10 | 1.151 [1.072-1.202], n=10 | 0.1355 [0.1220-0.1440], n=10 | 0.166 [0.155-0.180], n=10 |
| human_pfc_hbcc | 1,486,324 | h5 | 8.27 [8.15-9.21], n=10 | 1004 [1004-1004], n=10 | 1679 [1678-1679], n=10 | 0.8115 [0.6480-1.0020], n=10 | 0.1180 [0.1110-0.1230], n=10 | 0.1753 [0.1636-0.3518], n=10 | 1.510 [1.317-1.807], n=10 | 0.0180 [0.0160-0.0310], n=10 | 0.486 [0.465-0.532], n=10 |
| mouse_brain_e18 | 1,306,127 | bpcells | 5.05 [4.70-7.51], n=10 | 762 [761-762], n=10 | 1468 [1467-1468], n=10 | 0.8720 [0.7830-1.0620], n=10 | 0.6900 [0.6700-0.8910], n=10 | 0.7955 [0.7416-1.3348], n=10 | 1.688 [1.623-1.818], n=10 | 0.1040 [0.0930-0.1210], n=10 | 0.147 [0.134-0.182], n=10 |
| mouse_brain_e18 | 1,306,127 | h5 | 7.51 [7.27-8.31], n=10 | 973 [973-973], n=10 | 1448 [1442-1468], n=10 | 0.8560 [0.7610-0.9490], n=10 | 0.0335 [0.0310-0.0440], n=10 | 0.1267 [0.1186-0.1588], n=10 | 0.522 [0.486-0.576], n=10 | 0.1015 [0.0980-0.1160], n=10 | 0.259 [-0.029-0.287], n=10 |

Correctness: 40/40 access processes matched both source-matrix fingerprints.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-10-14T21:48:28+0200 |
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

