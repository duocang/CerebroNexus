# Expression-backend benchmark on real public datasets

**Run:** `20260805T214336Z-02ff3f0a971e-publication`  
**Profile:** `publication`  
**Git:** `02ff3f0a971e1c835ca0ee15294284375e1961f3`

> **Evidence status.** Publication-profile evidence: backend comparisons use independent process repeats and correctness fingerprints.

## Sources

| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |
|---|---:|---:|---:|---:|---:|:--:|
| 10x mouse brain E18 | 1,306,127 | 27,998 | 2.625e+09 | 2010 | 29.3 GB | **no** |
| human PFC cross-disorder (HBCC) | 1,486,324 | 34,176 | 6.112e+09 | 4112 | 68.3 GB | **no** |
| human PFC cross-disorder (MSSM) | 4,140,453 | 34,176 | 1.560e+10 | 3767 | 174.3 GB | **no** |

## Export

Values are median [minimum-maximum], followed by the number of independent export processes.

| source | cells | backend | total MB | export seconds | peak R heap MB |
|---|---:|---|---:|---:|---:|
| human_pfc_hbcc | 50,000 | bpcells | 334.5 [334.5-334.5], n=3 | 6.5 [6.5-6.5], n=3 | 7029 [7029-7029], n=3 |
| human_pfc_hbcc | 50,000 | embedded | 527.9 [527.9-527.9], n=3 | 97.0 [97.0-97.0], n=3 | 5899 [5899-5899], n=3 |
| human_pfc_hbcc | 50,000 | h5 | 268.7 [268.7-268.7], n=3 | 88.0 [87.9-88.0], n=3 | 11195 [11195-11195], n=3 |
| human_pfc_mssm | 50,000 | bpcells | 291.7 [291.7-291.7], n=3 | 6.0 [6.0-6.0], n=3 | 6200 [6200-6200], n=3 |
| human_pfc_mssm | 50,000 | embedded | 459.1 [459.1-459.1], n=3 | 84.2 [84.2-84.2], n=3 | 5274 [5274-5274], n=3 |
| human_pfc_mssm | 50,000 | h5 | 240.5 [240.5-240.5], n=3 | 78.5 [78.4-78.6], n=3 | 9948 [9948-9948], n=3 |
| mouse_brain_e18 | 50,000 | bpcells | 176.6 [176.6-176.6], n=3 | 4.1 [4.1-4.1], n=3 | 3951 [3951-3951], n=3 |
| mouse_brain_e18 | 50,000 | embedded | 210.0 [210.0-210.0], n=3 | 38.5 [38.5-38.5], n=3 | 3806 [3806-3806], n=3 |
| mouse_brain_e18 | 50,000 | h5 | 117.3 [117.3-117.3], n=3 | 38.0 [38.0-38.0], n=3 | 5074 [5074-5074], n=3 |
| mouse_brain_e18 | 150,000 | bpcells | 535.2 [535.2-535.2], n=3 | 11.1 [11.1-11.1], n=3 | 11027 [11027-11027], n=3 |
| mouse_brain_e18 | 150,000 | embedded | 637.4 [637.4-637.4], n=3 | 118.4 [118.3-118.5], n=3 | 10730 [10730-10730], n=3 |
| mouse_brain_e18 | 150,000 | h5 | 350.3 [350.3-350.3], n=3 | 156.2 [156.1-156.3], n=3 | 12401 [12401-12401], n=3 |

## Runtime access

Backend ready is CRB load plus backend attachment. First gene ready adds the first backend getter call in that fresh R process. This server-side readiness proxy excludes the Shiny handshake and browser rendering. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | backend ready s | first gene ready s | RSS MB | first query s | warmed p50 s | warmed p95 s | block prepare s | materialized block ready s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 50,000 | bpcells | 1.25 [1.24-1.26], n=6 | 1.88 [1.84-1.93], n=6 | 420 [420-421], n=6 | 0.6230 [0.5990-0.6690], n=6 | 0.3685 [0.3660-0.3780], n=6 | 0.3787 [0.3758-0.3872], n=6 | 0.005 [0.005-0.006], n=6 | 0.383 [0.379-0.389], n=6 |
| human_pfc_hbcc | 50,000 | embedded | 7.09 [7.08-7.10], n=6 | 7.44 [7.42-7.49], n=6 | 2725 [2725-2726], n=6 | 0.3465 [0.3330-0.3930], n=6 | 0.3290 [0.3290-0.3300], n=6 | 0.3334 [0.3324-0.3340], n=6 | 0.341 [0.340-0.343], n=6 | 0.343 [0.343-0.345], n=6 |
| human_pfc_hbcc | 50,000 | h5 | 2.40 [2.39-2.41], n=6 | 2.40 [2.40-2.41], n=6 | 527 [527-527], n=6 | 0.0080 [0.0070-0.0090], n=6 | 0.0050 [0.0050-0.0050], n=6 | 0.0090 [0.0084-0.0094], n=6 | 0.025 [0.025-0.026], n=6 | 0.047 [0.046-0.049], n=6 |
| human_pfc_mssm | 50,000 | bpcells | 1.25 [1.25-1.32], n=6 | 1.84 [1.82-1.93], n=6 | 419 [419-419], n=6 | 0.5880 [0.5700-0.6050], n=6 | 0.3260 [0.3250-0.3300], n=6 | 0.3335 [0.3314-0.3364], n=6 | 0.005 [0.005-0.006], n=6 | 0.339 [0.336-0.342], n=6 |
| human_pfc_mssm | 50,000 | embedded | 6.27 [6.20-6.30], n=6 | 6.56 [6.49-6.64], n=6 | 2429 [2429-2430], n=6 | 0.2890 [0.2850-0.3400], n=6 | 0.2830 [0.2830-0.2840], n=6 | 0.2885 [0.2870-0.2928], n=6 | 0.297 [0.296-0.317], n=6 | 0.299 [0.298-0.319], n=6 |
| human_pfc_mssm | 50,000 | h5 | 2.40 [2.40-2.43], n=6 | 2.41 [2.40-2.43], n=6 | 527 [527-527], n=6 | 0.0080 [0.0070-0.0110], n=6 | 0.0050 [0.0050-0.0050], n=6 | 0.0090 [0.0084-0.0090], n=6 | 0.026 [0.025-0.027], n=6 | 0.047 [0.046-0.048], n=6 |
| mouse_brain_e18 | 50,000 | bpcells | 1.24 [1.23-1.24], n=6 | 1.70 [1.67-1.72], n=6 | 416 [416-416], n=6 | 0.4610 [0.4290-0.4850], n=6 | 0.1920 [0.1900-0.1930], n=6 | 0.2001 [0.1980-0.2034], n=6 | 0.005 [0.005-0.006], n=6 | 0.208 [0.201-0.210], n=6 |
| mouse_brain_e18 | 50,000 | embedded | 3.36 [3.36-3.37], n=6 | 3.54 [3.52-3.55], n=6 | 1444 [1444-1444], n=6 | 0.1795 [0.1590-0.1880], n=6 | 0.1550 [0.1550-0.1560], n=6 | 0.1597 [0.1590-0.1600], n=6 | 0.168 [0.167-0.168], n=6 | 0.170 [0.170-0.171], n=6 |
| mouse_brain_e18 | 50,000 | h5 | 2.38 [2.37-2.40], n=6 | 2.39 [2.38-2.41], n=6 | 529 [529-529], n=6 | 0.0090 [0.0090-0.0100], n=6 | 0.0050 [0.0050-0.0050], n=6 | 0.0094 [0.0084-0.0094], n=6 | 0.029 [0.028-0.030], n=6 | 0.050 [0.049-0.053], n=6 |
| mouse_brain_e18 | 150,000 | bpcells | 1.56 [1.54-1.67], n=6 | 2.15 [2.14-2.28], n=6 | 470 [470-470], n=6 | 0.5970 [0.5930-0.6050], n=6 | 0.5705 [0.5660-0.5750], n=6 | 0.5836 [0.5778-0.5880], n=6 | 0.014 [0.013-0.015], n=6 | 0.626 [0.620-0.635], n=6 |
| mouse_brain_e18 | 150,000 | embedded | 9.63 [9.62-9.65], n=6 | 10.12 [10.09-10.19], n=6 | 3755 [3755-3755], n=6 | 0.4885 [0.4720-0.5620], n=6 | 0.4700 [0.4690-0.4700], n=6 | 0.4832 [0.4830-0.4840], n=6 | 0.503 [0.502-0.504], n=6 | 0.509 [0.508-0.509], n=6 |
| mouse_brain_e18 | 150,000 | h5 | 2.42 [2.41-2.44], n=6 | 2.43 [2.42-2.45], n=6 | 550 [550-550], n=6 | 0.0135 [0.0120-0.0150], n=6 | 0.0130 [0.0120-0.0140], n=6 | 0.0158 [0.0154-0.0168], n=6 | 0.299 [0.290-0.354], n=6 | 0.332 [0.320-0.386], n=6 |

Correctness: 72/72 access processes matched both source-matrix fingerprints.

## Observed export memory scaling

Across 4 distinct source/tier points, the median observed peak was 36.9 bytes per non-zero (range 35.0-42.0). This is a descriptive relationship for this exporter and host, not a universal memory law.
No maximum capacity is inferred from successful runs. A stopping boundary requires a separately recorded stress profile with observed failures; physical RAM, the R vector limit, and the `dgCMatrix` 32-bit index limit remain separate constraints.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-08-06T03:12:11+0200 |
| git_branch | perf/real-data-benchmark |
| git_dirty | false |
| repository_version | 3.2.0 |
| package_CerebroNexus | 3.2.0 |
| r_version | R version 4.6.1 (2026-06-24) |
| os | Linux 6.8.0-117-generic #117-Ubuntu SMP PREEMPT_DYNAMIC Tue May  5 19:26:24 UTC 2026 |
| cpu | Intel(R) Core(TM) i7-14700 |
| logical_cores | 28 |
| memory_mb | 128434.2 |
| r_vector_limit_mb | Inf |
| source `mouse_brain_e18` | 4,216,018,749 bytes; SHA-256 `255a36ee92de25cb3568faa2c27d31fe6d0db30f285c5c977be8d6245de14044` |
| source `human_pfc_hbcc` | 14,150,526,668 bytes; SHA-256 `aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f` |
| source `human_pfc_mssm` | 36,092,176,654 bytes; SHA-256 `c62456941372b90bcf0df38e8cb1c34dd060bc5a507270ab1d068cbe6f1dfd54` |

