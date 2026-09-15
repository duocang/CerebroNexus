# Preliminary full-source backend results

These rounded observations were recovered from the console log of run
`20260915T192120Z-9f65e8eb4efc-publication-full`. All 12 backend builds and all
24 fresh-process access measurements completed successfully. The former
publication workflow rejected the run later because its unrelated browser
interaction gate failed.

The original staged CSV files were deliberately removed with the failed-run
scratch directory. These tables are therefore an engineering summary, not an
immutable publication result. A new backend-only run remains required for the
paper evidence package.

## Backend construction

Values are approximate medians across three independent builds.

| source | cells | backend | build seconds | stored GB |
|---|---:|---|---:|---:|
| 10x mouse brain E18 | 1,306,127 | BPCells | 124.7 | 2.9 |
| 10x mouse brain E18 | 1,306,127 | H5 | 155.1 | 30.5 |
| human PFC HBCC | 1,486,324 | BPCells | 442.5 | 27.7 |
| human PFC HBCC | 1,486,324 | H5 | 565.4 | 70.7 |

## Hydrated startup and warmed single-gene access

Values are approximate medians across six fresh access processes.

| source | backend | startup seconds | RSS MB | warmed row p50 seconds |
|---|---|---:|---:|---:|
| 10x mouse brain E18 | BPCells | 3.54 | 688 | 0.465 |
| 10x mouse brain E18 | H5 | 4.70 | 850 | 0.058 |
| human PFC HBCC | BPCells | 3.75 | 760 | 0.622 |
| human PFC HBCC | H5 | 5.50 | 843 | 0.066 |

## Working interpretation

BPCells is the preferred default backend: it builds faster, starts faster, uses
less resident memory, and has a substantially smaller on-disk footprint. H5 has
lower warmed full-cell single-gene latency, but at a large storage cost.
