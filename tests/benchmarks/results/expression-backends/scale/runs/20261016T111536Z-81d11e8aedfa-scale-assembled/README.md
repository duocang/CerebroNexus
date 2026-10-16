# Scale: assembled existing measurements

Collection `20261016T111536Z-81d11e8aedfa-scale-assembled`. **No new measurements.** This is a complete grid assembled
from five real acquisitions, not one rerun of the revised Scale protocol.

- Common sizes: 1k, 10k, 50k, 100k, 300k, 500k, 1m.
- Mouse endpoint: 1,306,127 cells; Human endpoint: 1,486,324 cells.
- 44 dataset/size/backend combinations, each with five builds; every build has
  two independent access processes.
- Total: 220 successful builds, 440 access processes, 16,720 raw timings.
- Human embedded 500k is complete: one feasibility build under a 99 GiB cap plus
  four prospective repeats under a fixed 100.0 GiB (99% of launch-time available
  memory) cgroup cap with swap disabled. This collection supersedes the earlier
  assembly that recorded only the feasibility build.
- Five original 500k resource skips remain in `resource_skips.csv` and the
  unchanged original input run. They are not successful repeats and are not pooled.
- Embedded above 500k was not measured. No embedded full endpoint is invented.

`run_id`, `profile`, and every original numerical measurement remain acquisition
values. `collection_id`, `source_run_id`, `source_profile`, and `tier_type` identify
the assembly and origin. `05_schedule.csv` describes actual included successful
builds; it is NOT the five-repeat prospective schedule. Original schedules,
resource checks, manifests, query plans, logs and skip rows live under `inputs/`.
`resource_checks_by_acquisition.csv` retains historical checks without inventing
a single shared resource budget. The 99%-of-available fixed cap applies to the
Human 500k repeats; 300k used 90 GiB and the feasibility build 99 GiB.

The collection includes all evidence needed from Full and derived directories.
Those external directories are no longer needed for validation or replotting.
R packages and the nature-figure QA skill remain environment dependencies.

## Figure and statistics

`figures/expression_backend_benchmark_overview.png` and `.pdf` are the derived
figure regenerated for this collection, byte-identical to the delivered derived
figure. Rings/dashes identify reused Full endpoints; 300k points and the Human
500k completion are supplemental acquisitions. Human embedded 500k is a filled
five-build summary (median and min-max range), no longer a single hollow diamond.
Access values are medians of two processes within each build, then median/min-max
over builds. Different tiers can use different genes selected by the same rule;
these are descriptive measurements, not fitted scaling laws. Original and
supplemental memory limits, priorities and host/cache states differ. The five
500k builds provide a descriptive range, not a controlled replication study.

The old ceiling image is intentionally not activated: it lacks the new evidence.
Historical runs, derived files and Full CURRENT are unchanged. Only Scale CURRENT
and its latest figure copies are switched after validation and archival.

## Offline verification and reproduction

In the existing R environment, from this collection directory:

```sh
Rscript validate.R .
FIGURE_SKILL=/path/to/nature-figure Rscript replot.R .
```

Replot writes `replot-output/`, reads only bundled acquisition evidence and does
not measure, download, alter raw inputs, or update CURRENT. It preserves the
existing figure contract and runs the rendered panel-alignment check.
`assembly_inventory.csv` records SHA256 for all delivered inputs and artifacts.
The archived assembly recipe documents the one-time source-copy operation.
