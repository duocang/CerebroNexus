# Benchmark result contract

`tests/bench/results/` is the only result root inside the repository.

- `benchmark/{full,scale}/runs/<UTC>-<sha>-<profile>/` contains immutable
  paper benchmark runs; each profile's `CURRENT` pointer is updated last.
- `acceptance/<branch>-<candidate8>-<platform>/` contains one acceptance record
  for a pinned candidate and platform.
- `engineering/runs/<UTC>-<study>/` is the default destination for new
  engineering harness runs.
- `engineering/archive/` contains imported PR0-PR5 evidence referenced by the
  million-cell vignettes.

Raw datasets, caches, temporary artifacts, and failed-run scratch directories
stay outside the Git checkout. Low-level commands may accept explicit temporary
paths, but repository evidence must be published into one of the locations
above.
