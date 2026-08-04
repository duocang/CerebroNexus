# Reading benchmark results

This guide explains what the generated files mean. It does not contain a fixed performance conclusion; conclusions belong to a named immutable run.

## Start with the evidence label

Open `result/CURRENT`, then read `result/runs/<run-id>/summary.md`. Only a `publication` run may support the user-facing article. `quick` is exploratory, `standard` is local review evidence, and `stress` describes a host boundary.

## How to read the tables

Values such as `2.4 [2.2-2.9], n=6` mean median 2.4, observed range 2.2 to 2.9, from six independent processes. The range is host-specific variation, not a cross-platform confidence interval.

| field | meaning | safe comparison |
|---|---|---|
| `export_secs` | backend export only | within the same source and cell tier |
| `total_mb` | CRB plus external sibling bytes | within the same source/tier |
| `r_peak_mb` | maximum R heap during preparation/export | host-specific |
| `load_secs` | deserialize the CRB | within the same source/tier |
| `attach_secs` | attach an external backend | within the same source/tier |
| `rss_mb` | resident memory after load and attach | same host only |
| `first_query_secs` | first getter call in a fresh process | not cold disk |
| `hot_p50_secs` / `hot_p95_secs` | warmed single-gene query distribution | same query plan |
| `block_secs` | deterministic 12-gene block request | same query plan |

Do not rank backends from one metric alone. Export time, stored size, startup, resident memory, single-gene access, and block access describe different user costs.

## Correctness and exclusions

Every successful access row must say `correctness = OK` and match both source fingerprints. A fast row with a mismatch is rejected, not averaged.

`resource_check.csv` records why each planned tier was safe or unsafe. A normal publication does not silently remove a tier. Boundary failures belong to the explicit `stress` profile and must be described as applying to the recorded host and memory limit.

## Files in one run

| file | purpose |
|---|---|
| `00_probe.csv` | source dimensions and sparsity |
| `05_schedule.csv` | complete requested run grid and backend order |
| `resource_check.csv` | machine-fit decision for each source/tier |
| `10_export.csv` | raw export-process rows |
| `20_access.csv` | raw access-process rows and fingerprints |
| `run_manifest.csv` | Git, R, dependency, CPU, RAM, and OS provenance |
| `source_manifest.csv` | downloaded bytes and SHA-256 |
| `crashes.csv` | child-process failures |
| `summary.md` | validated aggregate report |
| `figures/` | publication-profile plots |

## Claims that are not supported

- “cold disk” latency: the operating-system cache is not controlled;
- universal bytes per non-zero: the estimate is descriptive for one host;
- full-source support: both default complete sources exceed `dgCMatrix`'s 32-bit non-zero index limit;
- biological performance: grouping and embeddings are synthetic fixtures;
- academic peer review: `publication` is an internal evidence gate.
