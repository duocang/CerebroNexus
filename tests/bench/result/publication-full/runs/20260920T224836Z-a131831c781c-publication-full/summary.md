# Expression-backend benchmark on real public datasets

**Run:** `20260920T224836Z-a131831c781c-publication-full`  
**Profile:** `panel_c2`  
**Git:** `a131831c781cbb9f33a22ecfe07f8115d4c47978`

> **Evidence status.** Publication-profile evidence: backend comparisons use independent process repeats and correctness fingerprints.

## Sources

| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |
|---|---:|---:|---:|---:|---:|:--:|
| 10x mouse brain E18 | 1,306,127 | 27,998 | 2.625e+09 | 2010 | 29.3 GB | **no** |
| human PFC cross-disorder (HBCC) | 1,486,324 | 34,176 | 6.112e+09 | 4112 | 68.3 GB | **no** |

## Export

Values are median [minimum-maximum], followed by the number of independent export processes.

| source | cells | backend | total MB | backend build s | shell s | CRB serialization s | peak R heap MB | peak process RSS MB |
|---|---:|---|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 27701.6 [27701.6-27701.6], n=3 | 460.6 [459.6-488.9], n=3 | 1.49 [1.49-1.51], n=3 | 3.51 [3.47-3.52], n=3 | 736 [736-736], n=3 | 1788 [1788-1789], n=3 |
| human_pfc_hbcc | 1,486,324 | h5 | 70662.6 [70662.6-70662.6], n=3 | 542.0 [541.5-546.7], n=3 | 1.30 [1.27-1.30], n=3 | 0.64 [0.64-0.65], n=3 | 804 [804-804], n=3 | 1929 [1929-1930], n=3 |
| mouse_brain_e18 | 1,306,127 | bpcells | 2909.1 [2909.1-2909.1], n=3 | 125.4 [125.1-125.5], n=3 | 1.34 [1.32-1.34], n=3 | 2.34 [2.31-2.35], n=3 | 708 [708-708], n=3 | 1694 [1693-1694], n=3 |
| mouse_brain_e18 | 1,306,127 | h5 | 30451.4 [30451.4-30451.4], n=3 | 156.4 [155.8-156.4], n=3 | 1.15 [1.15-1.16], n=3 | 0.45 [0.45-0.46], n=3 | 757 [757-757], n=3 | 1866 [1865-1866], n=3 |

## Runtime access

The first-query metric is the first backend getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MB | peak process RSS MB | first query s | warmed p50 s | warmed p95 s | 12-gene full block s | shuffled 100k row s | shuffled 100k block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 3.75 [3.72-3.80], n=6 | 760 [760-760], n=6 | 1668 [1668-1669], n=6 | 0.7885 [0.7690-0.8060], n=6 | 0.6315 [0.6030-0.6370], n=6 | 0.7150 [0.6838-0.7228], n=6 | 0.001 [0.001-0.002], n=6 | 0.0935 [0.0820-0.0970], n=6 | 0.037 [0.037-0.039], n=6 |
| human_pfc_hbcc | 1,486,324 | h5 | 5.55 [5.49-5.56], n=6 | 843 [843-843], n=6 | 1474 [1473-1474], n=6 | 0.0850 [0.0840-0.0900], n=6 | 0.0650 [0.0630-0.0670], n=6 | 0.0827 [0.0824-0.0838], n=6 | 0.062 [0.062-0.063], n=6 | 0.0090 [0.0090-0.0100], n=6 | 0.054 [0.053-0.055], n=6 |
| mouse_brain_e18 | 1,306,127 | bpcells | 3.56 [3.56-3.57], n=6 | 688 [687-688], n=6 | 1398 [1397-1398], n=6 | 0.5735 [0.5660-0.5980], n=6 | 0.4610 [0.4530-0.4620], n=6 | 0.5243 [0.5202-0.5384], n=6 | 0.001 [0.001-0.002], n=6 | 0.0700 [0.0690-0.0740], n=6 | 0.039 [0.038-0.040], n=6 |
| mouse_brain_e18 | 1,306,127 | h5 | 4.74 [4.72-4.74], n=6 | 850 [850-850], n=6 | 1373 [1373-1398], n=6 | 0.3930 [0.3840-0.4040], n=6 | 0.0580 [0.0570-0.0580], n=6 | 0.0695 [0.0690-0.0700], n=6 | 0.058 [0.057-0.059], n=6 | 0.0085 [0.0080-0.0090], n=6 | 0.106 [0.102-0.106], n=6 |

Correctness: 24/24 access processes matched both source-matrix fingerprints.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-09-21T00:48:46+0200 |
| git_branch | paper/real-data-benchmark |
| git_dirty | false |
| package_version | 4.6.4 |
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

