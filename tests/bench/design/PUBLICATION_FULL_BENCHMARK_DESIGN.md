# Publication-full Expression Backend Benchmark Design

## Decision

Acquire A/B, C1, and C2 as one study rather than extending historical evidence.
The final result is publishable only when every phase was produced from the same
clean commit, verified source bytes, software environment, thread allocation,
host, and storage configuration.

## Study grid

| phase | source and cells | backends | build repeats | access repeats per build |
|---|---|---|---:|---:|
| A/B | mouse 50k/150k; human 50k/150k | embedded, bpcells, h5 | 3 | 2 |
| C1 | mouse 400k; human 300k | embedded, bpcells, h5 | 3 | 2 |
| C2 | complete mouse and human | bpcells, h5 | 3 | 2 |

The backend order rotates over three builds. Every build and access repeat runs
in its own process. No large tier or backend is silently omitted.

## Measurement boundary

One deterministic 12-gene query plan is prepared in a separate process for each
source/tier before timed backend work. Preparation reads the source, selects the
panel, computes reference fingerprints, records time and peak RSS, and
atomically publishes the plan. The exact ordered genes, roles, densities, and
reference fingerprints are retained in `query_panel.csv`. Builds and access
processes may only read that plan. Their fingerprints must agree exactly.

For sampled tiers, build rows separately record source read, Seurat shell, and
backend export time. For C2, `export_secs` measures the streaming backend writer;
the build-process peak RSS also includes lazy source opening and portable shell
serialization. Neither includes query-plan preparation.

The first access is a fresh-process first query under uncontrolled OS-cache
state. It is never described as cold-disk latency.

## C2 artifact

The full source remains lazy. BPCells writes a directory backend; H5 writes a
TENx-compatible HDF5 sibling. A small CRB shell stores full cell identifiers,
deterministic synthetic sample/cluster metadata and projection, and a relative
backend reference. Only the requested 12 rows may be materialised for checking
and block access.

`embedded` is structurally unavailable for both complete sources because their
non-zero counts exceed `dgCMatrix`'s 32-bit index limit. The final figure labels
that cell `not representable`.

## Validity gates

Publication stops on any of the following:

- dirty acquisition worktree or malformed Git SHA;
- incomplete or duplicate schedule coverage;
- unsafe resource assessment;
- missing, failed, or incorrect build/access rows;
- query-plan fingerprint drift between preparation, build, and access;
- source byte-size or SHA-256 drift;
- study ID, acquisition Git SHA, CPU, OS, R platform, key package version,
  thread count, or storage-description drift across phases;
- incomplete final tables or figure.

## Result transaction

```text
study-work/<study-id>/
├── phases/
│   ├── ab/{CURRENT,runs/<phase-run-id>/}
│   ├── c1/{CURRENT,runs/<phase-run-id>/}
│   └── c2/{CURRENT,runs/<phase-run-id>/}
└── output/
    ├── phases/{ab,c1,c2}/
    ├── study_manifest.csv
    ├── environment_comparison.csv
    ├── source_provenance.csv
    ├── query_plan_metrics.csv
    ├── query_panel.csv
    ├── combined_metrics.csv
    ├── backend_ratios.csv
    ├── correctness.csv
    ├── summary.md
    └── figures/expression_backend_benchmark_publication_full.png
```

Each phase is immutable and resumable under the same explicit study ID. Derived
output is rebuilt from the three frozen phases. Only a complete validated output
tree is atomically copied to `result/publication-full/runs/<study-id>/`, and
`result/publication-full/CURRENT` is replaced last.

## Reporting

Report raw process rows, medians, observed ranges, and independent-process `n`.
Compute backend ratios only within matched source/tier/phase/metric cells. Do not
perform significance tests. The combined figure shows raw points plus median and
range, includes C1 build peak RSS, and makes the missing C2 embedded condition
explicit.

Supported claims are limited to host-specific backend feasibility and trade-offs
on the two recorded public sources. Cross-host claims require another host;
end-to-end interaction claims require a browser experiment.
