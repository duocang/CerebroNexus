# Engineering harnesses

These scripts reproduce focused PR0-PR5 million-cell measurements. They are not
the current real-data benchmark; run that through
[`../benchmark/run.sh`](../benchmark/run.sh).

## Contents

- [Acceptance entry points](#acceptance-entry-points)
- [Workflow entry point](#workflow-entry-point)
- [Focused diagnostics](#focused-diagnostics)
- [Internal support](#internal-support)

## Acceptance entry points

| Script | Direct caller | Measures |
|---|---|---|
| `crb/benchmark.R` | `acceptance/policy.R` | CRB write, decode, hydration, and size |
| `crb/verify.R` | `acceptance/STANDARD.md` correctness gate | legacy/thin CRB correctness |
| `backend/hot_paths.R` | policy and `backend/compare.sh` | expression backend operations |
| `renderer/benchmark.R` | policy | isolated million-point renderer |
| `viewer/startup.R` | policy | installed Viewer startup phases |
| `viewer/page_readiness.R` | policy | per-page latency, correctness, and resources |

## Workflow entry point

`backend/compare.sh` prepares isolated revisions and runs both
`backend/hot_paths.R` and `backend/bundle.R`. With no revision arguments it
compares the pinned pr0 and pr1 tips (`99d305c0` and `35128c51`); explicit
revision arguments reproduce older archived studies.

## Focused diagnostics

These remain because the million-cell reproducibility vignettes invoke them:

| Script | Direct caller | Purpose |
|---|---|---|
| `viewer/interactions.R` | Viewer benchmark vignette | interactive Viewer hot paths |
| `viewer/prepare_page_fixture.R` | Viewer benchmark vignette | add page-specific fixture data |
| `backend/bpcells_order.R` | Viewer benchmark vignette | BPCells cell-major vs gene-major layout |

## Internal support

Do not invoke these directly unless developing a harness:

| Path | Used by |
|---|---|
| `crb/fixture.R` | CRB harnesses and backend workflow |
| `viewer/page_readiness_protocol.R` | page-readiness harness and unit tests |
| `renderer/app/` | renderer harness test application |
