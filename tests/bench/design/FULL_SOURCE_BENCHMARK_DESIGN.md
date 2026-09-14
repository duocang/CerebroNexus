# Full-source publication benchmark design

The paper benchmark uses the complete 1,306,127-cell 10x mouse source and complete 1,486,324-cell PsychAD HBCC human source. It compares `bpcells` and `h5`; `embedded` is declared not representable and is not approximated with a smaller matrix.

Each source/backend pair has three independent builds, two fresh access processes per build, and one fresh Viewer process per build. The access protocol covers all-cell row/block reads and a deterministic reverse-ordered non-contiguous workload over up to 100,000 cells. The Viewer protocol requires exact full-cell rendering through WebGPU plus hover, selection, zoom, gene expression, and Linked Views.

The artifact path must use production defaults: gene-major BPCells storage, H5 group `expression`, `saveCerebro()` with qs2, and `readCerebro()` hydration. The publication wrapper runs only this full-source profile and publishes one immutable result directory.

Sampled profiles and one-million-cell scripts remain development or historical reproducibility tools and cannot satisfy the publication gate.
