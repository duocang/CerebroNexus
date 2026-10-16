# Validation of the assembled Scale update

- Nine `test-bench-*.R` files were exercised. Six passed on the first run.
  Three files exposed missing-field compatibility errors in the new optional
  manifest fields. Those were fixed; publish, reporting and resource tests were
  rerun together and all passed (tests-targeted.log). The resource file passed
  again after normalizing fixture paths across platforms (resource-final.log).
  Initial failures are retained in tests.log, not hidden or labelled as passes.
- The default and Scale dispatchers were mocked and confirmed to invoke Scale
  only. A tiny shell-only systemd scope confirmed memory.max=67108864 bytes and
  memory.swap.max=0. No benchmark ran in that scope.
- All four original acquisitions were checked, including timing, fingerprints,
  schedules, raw-value conservation, exact 300k/full coordinates, the n=1 exception,
  and original five resource skips. Altering a measurement was rejected both
  before and after recomputing the outer SHA256 checksum.
- A standalone replot read only copied inputs in this collection, with no Full
  or derived-directory dependency. Its PNG is byte-identical to the approved
  derived PNG. Eight panels were visually reviewed; alignment passed all six
  groups at 1.5 pt; collision audit passed with zero failures/warnings. The low
  level Tf font audit limitation is retained in QA_NOTES.md; effective PDF text
  sizes are >=8 pt. No invented values or repeat counts.
- The entire pre-update results backup was rechecked (859 files); original run
  and derived files remain untouched. Scale CURRENT and latest figure copies
  are the only pre-existing result files authorized to change during activation.
- No new benchmark measurements, downloads, package upgrades, cache cleanup,
  commits or pushes. The 95% memory policy is prospective only.
