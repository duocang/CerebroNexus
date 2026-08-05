# Real-data expression-backend benchmark

This directory compares the `embedded`, `bpcells`, and `h5` backends on public single-cell matrices. This page explains how to run it. Read [METHODOLOGY.md](METHODOLOGY.md) for the experimental design and [RESULTS.md](RESULTS.md) before interpreting any number.

The harness is repository-only research infrastructure. It stays in the CerebroNexus repository because it measures CerebroNexus export and runtime behaviour, but `.Rbuildignore` keeps its remote readers, large-data workflow, dedicated dependencies, tests, and archived evidence out of the installed package.

> **Current status:** the harness is ready for code review, but no complete `publication` run has been performed on this branch. The archived pilot is retained for provenance only and must not be cited as final evidence.

## Quick start

```bash
# Smallest correctness and harness check
BENCH_PROFILE=quick tools/bench/run_sweep.sh

# Repeated local review
BENCH_PROFILE=standard tools/bench/run_sweep.sh

# Repeated evidence plus staged figures
BENCH_PROFILE=publication tools/bench/run_sweep.sh

# Explicit memory-boundary experiment; normally rejected on a 32 GiB host
BENCH_PROFILE=stress tools/bench/run_sweep.sh
```

Limit a run to one source when developing the harness:

```bash
BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tools/bench/run_sweep.sh
```

Run the repository-only contract suite without downloading source data:

```bash
Rscript tools/bench/run_contract_tests.R
```

The benchmark uses the dependency environment in the repository's `default.nix`; a run records both the exact CerebroNexus commit and the Git blob identity of that environment. `rhdf5` is a benchmark dependency, not a package `Suggests` dependency.

`BENCH_ALLOW_UNSAFE=1` bypasses the resource gate only when `BENCH_PROFILE=stress`. The quick, standard, and publication profiles reject the override as a configuration error. Normal runs must not silently skip unsafe tiers.

## What happens before a download

The command first:

1. inspects source dimensions;
2. records the machine and Git revision;
3. creates the requested run plan; and
4. checks estimated memory, sparse-index, and free-disk limits.

If the plan is unsafe, it stops with the source, cell tier, estimated memory, safe budget, and reason. No complete source file or backend export has started at that point.

## Profiles

| profile | purpose | large boundary tiers |
|---|---|:---:|
| `quick` | verify the harness and correctness gate | no |
| `standard` | repeated local comparison | no |
| `publication` | repeated article evidence and figures | no |
| `stress` | opt-in host memory-boundary experiment | yes |

## Outputs

Validated runs are immutable under `result/runs/<run-id>/`. `result/CURRENT` contains the run used by report and plotting tools. A failed or interrupted run leaves the previous pointer unchanged.

The 2026-07-30 single-run pilot is retained under `result/archive/pilot-2026-07-30/`; it is superseded and cannot support current performance claims.

## Plain-language script map

| script | meaning |
|---|---|
| `01_inspect_data.R` | find out how large the sources are |
| `02_record_environment.R` | record the code and machine under test |
| `03_plan_runs.R` | list the requested backend runs |
| `04_check_resources.R` | stop before running a plan that will not fit |
| `10_export_backend.R` | export one backend in a fresh process |
| `20_measure_backend.R` | measure and correctness-check one backend |
| `30_check_measurements.R` | reject incomplete or incorrect measurements |
| `40_write_report.R` | generate the Markdown result report |
| `41_draw_figures.R` | generate publication figures |
| `50_check_outputs.R` | ensure the report package is complete |
| `60_publish_results.R` | publish immutably and update `CURRENT` last |

The two default public sources are 10x mouse brain E18 (4.2 GB) and the HBCC human prefrontal-cortex atlas (14.2 GB). The MSSM cohort is opt-in through `BENCH_SOURCES_EXTRA=human_pfc_mssm`.
