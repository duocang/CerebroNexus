# Expression-backend benchmark on real public datasets

**Run:** `20261017T174502Z-63d5337c3647-full`  
**Profile:** `panel_c2`  
**Git:** `63d5337c3647f1343c8d40e29a279437b84be44e`

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
| human_pfc_hbcc | 1,486,324 | bpcells | 27692.8 [27692.8-27692.8], n=5 | 543.1 [539.4-553.8], n=5 | 2.12 [2.12-2.63], n=5 | 11.50 [11.43-13.27], n=5 | 1021 [1021-1021], n=5 | 1926 [1925-1926], n=5 |
| human_pfc_hbcc | 1,486,324 | h5 | 10074.1 [10074.1-10074.1], n=5 | 2093.6 [2092.3-2096.4], n=5 | 1.36 [1.34-2.02], n=5 | 12.86 [12.74-14.64], n=5 | 1252 [1252-1252], n=5 | 2066 [2065-2066], n=5 |
| human_pfc_hbcc | 1,486,324 | h5_uncompressed | 70662.6 [70662.6-70662.6], n=5 | 636.4 [613.5-665.1], n=5 | 1.40 [1.38-2.03], n=5 | 13.83 [13.47-14.53], n=5 | 1272 [1272-1272], n=5 | 2066 [2065-2066], n=5 |
| mouse_brain_e18 | 1,306,127 | bpcells | 2904.1 [2904.1-2904.1], n=5 | 153.5 [152.1-156.0], n=5 | 1.56 [1.56-1.71], n=5 | 9.60 [9.48-10.44], n=5 | 968 [968-968], n=5 | 1827 [1827-1828], n=5 |
| mouse_brain_e18 | 1,306,127 | h5 | 3700.3 [3700.3-3700.3], n=5 | 520.5 [519.1-529.1], n=5 | 1.33 [1.31-1.57], n=5 | 11.06 [11.01-11.94], n=5 | 1139 [1139-1139], n=5 | 1982 [1981-1982], n=5 |
| mouse_brain_e18 | 1,306,127 | h5_uncompressed | 30451.4 [30451.4-30451.4], n=5 | 193.9 [190.4-199.4], n=5 | 1.30 [1.30-1.33], n=5 | 10.65 [10.63-12.92], n=5 | 1115 [1115-1115], n=5 | 1981 [1979-1982], n=5 |

## Runtime access

Access counts describe fresh processes nested within independently built artifacts (two processes per build), not independent build replicates.
Timing contract: materialized-v2. All timed results are materialized before the timer stops.
The first-query metric is the first getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MiB | peak process RSS MiB | first query s | warmed p50 s | warmed p95 s | 12-gene full block s | shuffled 100k row s | shuffled 100k block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 3.88 [3.84-5.27], n=10 | 796 [796-796], n=10 | 1803 [1802-1806], n=10 | 1.0491 [1.0164-1.1251], n=10 | 0.8314 [0.8167-0.8697], n=10 | 0.9563 [0.9044-1.0165], n=10 | 1.056 [0.975-1.211], n=10 | 0.0906 [0.0826-0.1075], n=10 | 0.131 [0.119-0.161], n=10 |
| human_pfc_hbcc | 1,486,324 | h5 | 6.77 [6.64-8.66], n=10 | 1006 [1005-1071], n=10 | 1650 [1650-1658], n=10 | 0.6028 [0.5901-0.9356], n=10 | 0.0742 [0.0734-0.1091], n=10 | 0.1518 [0.1508-0.2147], n=10 | 0.545 [0.539-0.729], n=10 | 0.0112 [0.0111-0.0162], n=10 | 0.497 [0.494-0.631], n=10 |
| human_pfc_hbcc | 1,486,324 | h5_uncompressed | 6.89 [6.64-8.69], n=10 | 1006 [1006-1006], n=10 | 1650 [1650-1651], n=10 | 0.6076 [0.5895-0.9397], n=10 | 0.0738 [0.0735-0.1073], n=10 | 0.1109 [0.1096-0.1653], n=10 | 0.464 [0.456-0.618], n=10 | 0.0100 [0.0097-0.0144], n=10 | 0.416 [0.412-0.523], n=10 |
| mouse_brain_e18 | 1,306,127 | bpcells | 4.17 [3.78-5.34], n=10 | 773 [773-773], n=10 | 1500 [1499-1501], n=10 | 0.7775 [0.6813-0.8490], n=10 | 0.5819 [0.5710-0.6767], n=10 | 0.7641 [0.6448-0.8415], n=10 | 0.922 [0.809-1.137], n=10 | 0.1360 [0.1279-0.1798], n=10 | 0.108 [0.104-0.145], n=10 |
| mouse_brain_e18 | 1,306,127 | h5 | 6.21 [5.74-7.79], n=10 | 973 [973-973], n=10 | 1509 [1443-1512], n=10 | 0.5116 [0.4819-0.7719], n=10 | 0.0344 [0.0336-0.0479], n=10 | 0.1139 [0.1131-0.1373], n=10 | 0.392 [0.377-0.506], n=10 | 0.0693 [0.0685-0.0733], n=10 | 0.258 [0.255-0.268], n=10 |
| mouse_brain_e18 | 1,306,127 | h5_uncompressed | 5.79 [5.72-7.27], n=10 | 973 [973-973], n=10 | 1443 [1400-1469], n=10 | 0.4967 [0.4830-0.7748], n=10 | 0.0267 [0.0248-0.0358], n=10 | 0.0866 [0.0850-0.1246], n=10 | 0.345 [0.331-0.441], n=10 | 0.0689 [0.0684-0.0959], n=10 | 0.224 [0.220-0.280], n=10 |

Correctness: 60/60 access processes matched both source-matrix fingerprints.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-10-17T19:45:14+0200 |
| git_branch | paper/real-data-benchmark |
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

