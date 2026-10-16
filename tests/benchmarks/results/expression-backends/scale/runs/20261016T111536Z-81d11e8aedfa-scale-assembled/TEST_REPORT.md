# Validation of the assembled Scale update

- Nine `test-bench-*.R` files were exercised against the current scripts. The
  initial run and the targeted publish/reporting/resources rerun both completed
  with all selected files passing (tests.log, tests-targeted.log). The resource
  file passed again after the prospective schedule was regenerated
  (resource-final.log).
- The default and Scale dispatchers were mocked and confirmed to invoke Scale
  only. A tiny shell-only systemd scope confirmed memory.max=67108864 bytes and
  memory.swap.max=0. No benchmark ran in that scope.
- All five original acquisitions plus the four Human 500k repeats were checked,
  including timing, fingerprints, schedules, raw-value conservation, the exact
  300k/full coordinates, the completed five-build Human embedded 500k series and
  the original five resource skips. Assembly tests verified that altering a
  measurement is rejected both before and after recomputing the outer SHA256.
- A standalone replot read only copied inputs in this collection, with no Full
  or derived-directory dependency. Its PNG is byte-identical to the delivered
  derived PNG. Eight panels were visually reviewed; alignment passed all six
  groups at 1.5 pt; collision audit passed with zero failures and warnings. The
  low-level Tf font audit limitation is retained in QA_NOTES.md; effective PDF
  text sizes are >=8 pt. No invented values or repeat counts.
- The entire pre-update results backup was rechecked (1,829 files); original run
  files remain byte-identical. Only the derived directory's regenerated outputs
  changed during the update, and only Scale CURRENT and its latest figure copies
  are authorized to change during activation.
- No new benchmark measurements, downloads, package upgrades, cache cleanup,
  commits or pushes. The 99%-of-available fixed cap applies to the Human 500k
  repeats (107,379,944,939 bytes, swap disabled).
