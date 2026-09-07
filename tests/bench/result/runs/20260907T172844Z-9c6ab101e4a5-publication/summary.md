# Expression-backend benchmark on real public datasets

**Run:** `20260907T172844Z-9c6ab101e4a5-publication`
**Profile:** `publication`
**Git:** `9c6ab101e4a5a2ae79ada5d5fab99bf05952a0f4`

> **Evidence status.** Publication-profile evidence: backend comparisons use independent process repeats and correctness fingerprints.

## Sources

| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |
|---|---:|---:|---:|---:|---:|:--:|
| 10x mouse brain E18 | 1,306,127 | 27,998 | 2.625e+09 | 2010 | 29.3 GB | **no** |
| human PFC cross-disorder (HBCC) | 1,486,324 | 34,176 | 6.112e+09 | 4112 | 68.3 GB | **no** |

## Export

Values are median [minimum-maximum], followed by the number of independent export processes.

| source | cells | backend | total MB | export seconds | peak R heap MB | peak process RSS MB |
|---|---:|---|---:|---:|---:|---:|
| human_pfc_hbcc | 50,000 | bpcells | 334.7 [334.7-334.7], n=3 | 5.7 [5.6-5.7], n=3 | 10880 [10880-10880], n=3 | 10985 [10984-10985], n=3 |
| human_pfc_hbcc | 50,000 | embedded | 528.2 [528.2-528.2], n=3 | 96.5 [96.5-96.5], n=3 | 10880 [10880-10880], n=3 | 10984 [10983-10985], n=3 |
| human_pfc_hbcc | 50,000 | h5 | 268.9 [268.9-268.9], n=3 | 86.9 [86.8-87.0], n=3 | 11151 [11151-11151], n=3 | 11104 [11104-11104], n=3 |
| human_pfc_hbcc | 150,000 | bpcells | 977.9 [977.9-977.9], n=3 | 13.9 [13.8-14.2], n=3 | 31071 [31071-31071], n=3 | 31187 [31186-31189], n=3 |
| human_pfc_hbcc | 150,000 | embedded | 1562.1 [1562.1-1562.1], n=3 | 284.3 [284.3-284.4], n=3 | 31071 [31071-31071], n=3 | 31187 [31186-31189], n=3 |
| human_pfc_hbcc | 150,000 | h5 | 788.7 [788.7-788.7], n=3 | 359.2 [358.9-359.3], n=3 | 31355 [31355-31355], n=3 | 31186 [31186-31189], n=3 |
| mouse_brain_e18 | 50,000 | bpcells | 176.6 [176.6-176.6], n=3 | 3.5 [3.5-3.6], n=3 | 5326 [5326-5326], n=3 | 5426 [5426-5427], n=3 |
| mouse_brain_e18 | 50,000 | embedded | 210.0 [210.0-210.0], n=3 | 38.0 [37.9-38.0], n=3 | 5326 [5326-5326], n=3 | 5426 [5426-5427], n=3 |
| mouse_brain_e18 | 50,000 | h5 | 117.3 [117.3-117.3], n=3 | 37.4 [37.3-37.4], n=3 | 5610 [5610-5610], n=3 | 5431 [5431-5431], n=3 |
| mouse_brain_e18 | 150,000 | bpcells | 535.2 [535.2-535.2], n=3 | 7.8 [7.8-7.9], n=3 | 15348 [15348-15348], n=3 | 15461 [15460-15461], n=3 |
| mouse_brain_e18 | 150,000 | embedded | 637.4 [637.4-637.4], n=3 | 115.1 [115.0-115.1], n=3 | 15348 [15348-15348], n=3 | 15461 [15459-15461], n=3 |
| mouse_brain_e18 | 150,000 | h5 | 350.3 [350.3-350.3], n=3 | 152.8 [152.8-152.8], n=3 | 15633 [15633-15633], n=3 | 15461 [15459-15462], n=3 |

## Runtime access

The first-query metric is the first backend getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MB | peak process RSS MB | first query s | warmed p50 s | warmed p95 s | 12-gene block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 50,000 | bpcells | 1.29 [1.28-1.36], n=6 | 417 [417-417], n=6 | 473 [473-474], n=6 | 0.3885 [0.3810-0.4300], n=6 | 0.3690 [0.3670-0.3750], n=6 | 0.3917 [0.3772-0.4088], n=6 | 0.005 [0.004-0.006], n=6 |
| human_pfc_hbcc | 50,000 | embedded | 7.07 [7.06-7.09], n=6 | 2679 [2679-2679], n=6 | 2861 [2861-2862], n=6 | 0.3615 [0.3220-0.3870], n=6 | 0.3200 [0.3190-0.3200], n=6 | 0.3242 [0.3234-0.3488], n=6 | 0.332 [0.332-0.333], n=6 |
| human_pfc_hbcc | 50,000 | h5 | 2.47 [2.47-2.48], n=6 | 566 [566-566], n=6 | 616 [616-617], n=6 | 0.0070 [0.0060-0.0080], n=6 | 0.0040 [0.0040-0.0050], n=6 | 0.0080 [0.0074-0.0080], n=6 | 0.029 [0.028-0.030], n=6 |
| human_pfc_hbcc | 150,000 | bpcells | 1.59 [1.58-1.60], n=6 | 459 [459-459], n=6 | 580 [568-581], n=6 | 1.0800 [1.0770-1.1270], n=6 | 1.0605 [1.0590-1.0610], n=6 | 1.0721 [1.0674-1.0764], n=6 | 0.014 [0.013-0.016], n=6 |
| human_pfc_hbcc | 150,000 | embedded | 20.20 [20.17-20.22], n=6 | 7339 [7339-7339], n=6 | 7854 [7854-7854], n=6 | 0.9390 [0.9360-1.0170], n=6 | 0.9330 [0.9330-0.9340], n=6 | 0.9474 [0.9464-0.9484], n=6 | 0.968 [0.968-0.969], n=6 |
| human_pfc_hbcc | 150,000 | h5 | 2.43 [2.42-2.44], n=6 | 570 [570-570], n=6 | 666 [653-666], n=6 | 0.0265 [0.0250-0.0270], n=6 | 0.0130 [0.0120-0.0130], n=6 | 0.0180 [0.0178-0.0184], n=6 | 0.038 [0.038-0.040], n=6 |
| mouse_brain_e18 | 50,000 | bpcells | 1.28 [1.28-1.33], n=6 | 414 [414-414], n=6 | 471 [471-472], n=6 | 0.2000 [0.1980-0.2100], n=6 | 0.1900 [0.1880-0.1910], n=6 | 0.1982 [0.1962-0.2002], n=6 | 0.005 [0.004-0.005], n=6 |
| mouse_brain_e18 | 50,000 | embedded | 3.33 [3.33-3.33], n=6 | 1397 [1397-1397], n=6 | 1575 [1575-1575], n=6 | 0.1735 [0.1570-0.1910], n=6 | 0.1550 [0.1550-0.1550], n=6 | 0.1590 [0.1590-0.1744], n=6 | 0.167 [0.166-0.168], n=6 |
| mouse_brain_e18 | 50,000 | h5 | 2.45 [2.44-2.46], n=6 | 565 [565-565], n=6 | 603 [603-604], n=6 | 0.0070 [0.0070-0.0080], n=6 | 0.0040 [0.0040-0.0040], n=6 | 0.0080 [0.0074-0.0080], n=6 | 0.025 [0.024-0.026], n=6 |
| mouse_brain_e18 | 150,000 | bpcells | 1.58 [1.57-1.59], n=6 | 459 [459-459], n=6 | 583 [583-586], n=6 | 0.5845 [0.5790-0.5890], n=6 | 0.5660 [0.5650-0.5710], n=6 | 0.5776 [0.5716-0.5862], n=6 | 0.014 [0.014-0.017], n=6 |
| mouse_brain_e18 | 150,000 | embedded | 9.58 [9.57-9.59], n=6 | 3710 [3710-3711], n=6 | 4220 [4219-4220], n=6 | 0.4760 [0.4710-0.5630], n=6 | 0.4695 [0.4690-0.4700], n=6 | 0.4830 [0.4820-0.4934], n=6 | 0.502 [0.502-0.503], n=6 |
| mouse_brain_e18 | 150,000 | h5 | 2.43 [2.42-2.59], n=6 | 570 [570-571], n=6 | 643 [643-643], n=6 | 0.0260 [0.0240-0.0260], n=6 | 0.0130 [0.0120-0.0140], n=6 | 0.0158 [0.0154-0.0168], n=6 | 0.031 [0.030-0.032], n=6 |

Correctness: 72/72 access processes matched both source-matrix fingerprints.

## Host-specific scale-limit estimate

Across 4 distinct source/tier points, the median observed peak was 53.9 bytes per non-zero (range 52.8-56.6). This is a descriptive estimate for this exporter and host, not a universal memory law.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-09-07T19:28:45+0200 |
| git_branch | paper/real-data-benchmark |
| git_dirty | false |
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
