# Expression backend sweep on real remote datasets

## Sources

| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |
|---|---:|---:|---:|---:|---:|:--:|
| 10x mouse brain E18 | 1,306,127 | 27,998 | 2.625e+09 | 2,010 | 29.3 GB | **no** |
| human PFC cross-disorder (HBCC) | 1,486,324 | 34,176 | 6.112e+09 | 4,112 | 68.3 GB | **no** |

## Export: disk and wall clock

| source | cells | backend | status | nnz | crb MB | sibling MB | total MB | export s | peak R heap MB |
|---|---:|---|---|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 50,000 | bpcells | ok | 2.107e+08 | 1.8 | 332.9 | 334.7 | 4.9 | 10,881 |
| human_pfc_hbcc | 50,000 | embedded | ok | 2.107e+08 | 528.2 | -- | 528.2 | 111.2 | 10,881 |
| human_pfc_hbcc | 50,000 | h5 | ok | 2.107e+08 | 1.6 | 267.4 | 268.9 | 93.1 | 11,151 |
| human_pfc_hbcc | 150,000 | bpcells | ok | 6.166e+08 | 5.1 | 972.7 | 977.9 | 20.2 | 31,078 |
| human_pfc_hbcc | 150,000 | embedded | ok | 6.166e+08 | 1,562.1 | -- | 1,562.1 | 337.0 | 31,072 |
| human_pfc_hbcc | 150,000 | h5 | ok | 6.166e+08 | 4.6 | 784.0 | 788.7 | 419.6 | 31,301 |
| human_pfc_hbcc | 3e+05 | bpcells | **FAILED(seurat): vector memory limit of 32.0 Gb reached, see mem.maxVSi** | 1.243e+09 | -- | -- | -- | -- | -- |
| human_pfc_hbcc | 3e+05 | embedded | **FAILED(seurat): vector memory limit of 32.0 Gb reached, see mem.maxVSi** | 1.243e+09 | -- | -- | -- | -- | -- |
| human_pfc_hbcc | 3e+05 | h5 | **FAILED(seurat): vector memory limit of 32.0 Gb reached, see mem.maxVSi** | 1.243e+09 | -- | -- | -- | -- | -- |
| mouse_brain_e18 | 50,000 | bpcells | ok | 9.867e+07 | 1.8 | 174.8 | 176.6 | 3.1 | -- |
| mouse_brain_e18 | 50,000 | embedded | ok | 9.867e+07 | 210.0 | -- | 210.0 | 43.4 | -- |
| mouse_brain_e18 | 50,000 | h5 | ok | 9.867e+07 | 1.5 | 115.8 | 117.3 | 37.7 | -- |
| mouse_brain_e18 | 150,000 | bpcells | ok | 2.995e+08 | 4.9 | 530.2 | 535.2 | 8.2 | -- |
| mouse_brain_e18 | 150,000 | embedded | ok | 2.995e+08 | 637.4 | -- | 637.4 | 137.0 | -- |
| mouse_brain_e18 | 150,000 | h5 | ok | 2.995e+08 | 4.5 | 345.8 | 350.3 | 169.8 | -- |
| mouse_brain_e18 | 250,000 | embedded | ok | 4.998e+08 | 1,063.4 | -- | 1,063.4 | 215.2 | 25,325 |
| mouse_brain_e18 | 4e+05 | bpcells | **FAILED(seurat): error in evaluating the argument 'x' in selecting a me** | 7.968e+08 | -- | -- | -- | -- | -- |
| mouse_brain_e18 | 4e+05 | embedded | **FAILED(seurat): error in evaluating the argument 'x' in selecting a me** | 7.968e+08 | -- | -- | -- | -- | -- |
| mouse_brain_e18 | 4e+05 | h5 | **FAILED(seurat): error in evaluating the argument 'x' in selecting a me** | 7.968e+08 | -- | -- | -- | -- | -- |
| mouse_brain_e18 | 8e+05 | bpcells | **FAILED(read): vector memory limit of 32.0 Gb reached, see mem.maxVSize** | -- | -- | -- | -- | -- | -- |
| mouse_brain_e18 | 8e+05 | embedded | **FAILED(read): vector memory limit of 32.0 Gb reached, see mem.maxVSize** | -- | -- | -- | -- | -- | -- |
| mouse_brain_e18 | 8e+05 | h5 | **FAILED(read): vector memory limit of 32.0 Gb reached, see mem.maxVSize** | -- | -- | -- | -- | -- | -- |

## Access: startup, memory, query latency

| source | cells | backend | status | load s | attach s | startup s | RSS MB | cold s | hot p50 s | hot p95 s | bulk s |
|---|---:|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 50,000 | bpcells | ok | 0.05 | 1.26 | 1.31 | 437 | 0.4085 | 0.4189 | 0.4470 | 0.008 |
| human_pfc_hbcc | 50,000 | embedded | ok | 4.73 | 0.02 | 4.75 | 2,759 | 0.3099 | 0.3087 | 0.3167 | 0.316 |
| human_pfc_hbcc | 50,000 | h5 | ok | 0.03 | 2.23 | 2.26 | 571 | 0.0048 | 0.0057 | 0.0090 | 0.036 |
| human_pfc_hbcc | 150,000 | bpcells | ok | 0.15 | 1.29 | 1.44 | 491 | 1.1945 | 1.2026 | 1.2401 | 0.014 |
| human_pfc_hbcc | 150,000 | embedded | ok | 13.37 | 0.01 | 13.38 | 7,410 | 0.9166 | 0.9250 | 0.9763 | 0.955 |
| human_pfc_hbcc | 150,000 | h5 | ok | 0.09 | 2.10 | 2.19 | 585 | 0.0079 | 0.0088 | 0.0127 | 0.044 |
| mouse_brain_e18 | 50,000 | bpcells | ok | 0.05 | 1.28 | 1.33 | 432 | 0.1995 | 0.1994 | 0.2063 | 0.004 |
| mouse_brain_e18 | 50,000 | embedded | ok | 2.21 | 0.01 | 2.23 | 1,476 | 0.1416 | 0.1426 | 0.1544 | 0.149 |
| mouse_brain_e18 | 50,000 | h5 | ok | 0.03 | 2.12 | 2.15 | 569 | 0.0037 | 0.0038 | 0.0066 | 0.027 |
| mouse_brain_e18 | 150,000 | bpcells | ok | 0.15 | 1.32 | 1.46 | 487 | 0.6123 | 0.6073 | 0.6186 | 0.014 |
| mouse_brain_e18 | 150,000 | embedded | ok | 6.68 | 0.01 | 6.69 | 3,784 | 0.4428 | 0.4409 | 0.4645 | 0.480 |
| mouse_brain_e18 | 150,000 | h5 | ok | 0.09 | 2.08 | 2.17 | 585 | 0.0120 | 0.0099 | 0.0159 | 0.027 |

---

Host: Darwin 27.0.0 arm64, R 4.6.1. Generated 2026-07-30 02:37.
