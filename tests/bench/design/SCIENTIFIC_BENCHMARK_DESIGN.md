# Scientific expression-backend benchmark design

The benchmark remains a correctness-checked, resource-gated comparison of
`embedded`, `bpcells`, and `h5` on public single-cell matrices. Its publication
contract is now defined by one complete A/B + C1 + C2 acquisition rather than
separate incremental runs.

See [PUBLICATION_FULL_BENCHMARK_DESIGN.md](PUBLICATION_FULL_BENCHMARK_DESIGN.md)
for the active scientific design. Exploratory `quick`, `standard`, and `stress`
profiles remain available through `run_sweep.sh`, but they cannot replace the
complete publication study.
