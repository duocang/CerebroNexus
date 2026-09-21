# Preserve Published Benchmark Runs

## Goal

Keep every successfully published `publication-full` run under `tests/bench/result/publication-full/runs/<run-id>/` when the remote launcher starts another benchmark.

## Design

`update_and_run_publication_full.sh run` will continue to clean transient `tests/bench/study-work` and `tests/bench/scratch` directories before launch, but it will no longer delete `tests/bench/result/publication-full`. Each successful run already receives a unique UTC timestamp, commit SHA, and profile identifier, while `60_publish_results.R` publishes into an immutable run directory and updates `CURRENT` only after validation.

No reset flag or archive layer will be added. Existing collision checks in `60_publish_results.R` remain authoritative.

## Failure Behavior

An interrupted or failed run leaves all previously published runs and `CURRENT` unchanged. A successful run adds one immutable run directory and then advances `CURRENT`.

## Verification

Update the launcher contract test to require preservation of `RESULT_ROOT` while retaining cleanup coverage for `study-work` and `scratch`. Run all benchmark tests and require zero failures and zero warnings.
