# Reproduce the 1M-cell Viewer benchmark

## Scope

This vignette compares Viewer performance across three stacked Git
revisions. It uses the official 10x Genomics E18 mouse brain matrix,
retains exactly 1,000,000 cells, and creates a BPCells-backed Seurat
object and Cerebro file.

The benchmark has three layers:

1.  `tests/bench/viewer_1m_hot_paths.R` measures filtering, sampling,
    hover preparation and expression access between the release and
    backend revision.
2.  `tests/bench/viewer_interaction_hot_paths.R` and
    `tests/bench/viewer_interaction_hit_test.js` measure the Viewer
    interaction algorithms added by the final revision.
3.  `tests/bench/viewer_1m_browser.R` launches the real Shiny Viewer in
    Chrome, waits for the 1M dataset and Overview Canvas, checks that
    the Canvas contains plotted pixels, rejects browser errors, and
    records Shiny process memory.

The scripts are the executable source of truth. This vignette explains
their arguments and supplies complete shell commands rather than copying
their implementation into a second place.

## Compared revisions

Choose release, backend and Viewer revisions. Each revision must be an
ancestor of the next so the three comparisons isolate backend work,
Viewer interaction work and their cumulative effect. Tags and
pull-request refs are preferable to short commit IDs in published
commands.

``` bash
export BASELINE_REV="v4.4.2"
export BACKEND_REV="perf/pr1-backend-hot-paths"
export VIEWER_REV="perf/pr2-viewer-interactions"

git rev-parse --verify "$BASELINE_REV^{commit}"
git rev-parse --verify "$BACKEND_REV^{commit}"
git rev-parse --verify "$VIEWER_REV^{commit}"
git merge-base --is-ancestor "$BASELINE_REV" "$BACKEND_REV"
git merge-base --is-ancestor "$BACKEND_REV" "$VIEWER_REV"
```

The defaults above run directly in the development repository. For a
published comparison, replace either branch ref with a fetched
pull-request ref; no other command changes. All validation commands exit
with status zero when the revisions form the required stack.

## Requirements

The reported run used:

- Apple M1 Pro with 32 GiB RAM;
- macOS 27.0;
- R 4.6.1 on `aarch64`;
- BPCells 0.3.1, Seurat 5.5.1, Shiny 1.14.0 and shinytest2 0.5.1;
- a Chrome or Chromium browser discoverable by shinytest2;
- at least 15 GB of free disk space for the source and reusable outputs.

Run this preflight from the repository root. It only reports missing
packages; it does not install or change anything.

``` bash
Rscript - <<'RS'
required <- c(
  "BPCells", "devtools", "dplyr", "glue", "magrittr", "Matrix",
  "ps", "Seurat", "SeuratObject", "shiny", "shinytest2"
)
missing <- required[
  !vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "))
}
cat("Benchmark packages: OK\n")
RS
```

## Prepare isolated comparison trees

Do not switch the working checkout back and forth during a benchmark.
Create three detached Git worktrees so all revisions coexist while
reading the same CRB.

Copy and run this block from the repository root:

``` bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
BENCH_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/cerebronexus-viewer-bench.XXXXXX")"
BASELINE_ROOT="$BENCH_ROOT/base"
BACKEND_ROOT="$BENCH_ROOT/backend"
VIEWER_ROOT="$BENCH_ROOT/viewer"

git worktree add --detach "$BASELINE_ROOT" "$BASELINE_REV"
git worktree add --detach "$BACKEND_ROOT" "$BACKEND_REV"
git worktree add --detach "$VIEWER_ROOT" "$VIEWER_REV"

printf 'repository: %s\nbenchmark trees: %s\n' "$REPO_ROOT" "$BENCH_ROOT"
```

The browser benchmark passes a named dataset path to
[`launchCerebro()`](https://mihem.github.io/CerebroNexus/reference/launchCerebro.md),
so all worktrees remain clean and require no compatibility patch.

## Download and prepare the 1M example

Choose a persistent cache location. Reusing it is important because
preparation takes tens of minutes and the source H5 is about 4.2 GB.

``` bash
export CEREBRO_LARGE_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/CerebroNexus-benchmark"
mkdir -p "$CEREBRO_LARGE_CACHE"
df -h "$CEREBRO_LARGE_CACHE"
```

Run the complete download, Seurat sketch workflow and CRB conversion
from the source checkout:

``` bash
cd "$REPO_ROOT"

/usr/bin/time -l Rscript - <<'RS'
devtools::load_all(".", quiet = TRUE)
result <- .prepareLargeExample(
  size = "1m",
  cache_dir = Sys.getenv("CEREBRO_LARGE_CACHE")
)
print(result)
RS
```

The preparation is resumable at artifact boundaries. A later call reuses
a complete source, Seurat object or CRB instead of downloading and
converting it again. Partial downloads are not published as complete
cache files.

Define and validate the resulting paths:

``` bash
CRB="$CEREBRO_LARGE_CACHE/1m/cerebro/cerebro_mouse_brain_1m.crb"
SIDECAR="${CRB%.crb}.bpcells"

test -f "$CRB"
test -d "$SIDECAR"
du -sh "$CEREBRO_LARGE_CACHE/1m/source" \
  "$CEREBRO_LARGE_CACHE/1m/seurat" \
  "$CEREBRO_LARGE_CACHE/1m/cerebro"
```

The benchmarked artifact contained 1,000,000 cells, 27,998 genes, 33
clusters and one full 1,000,000-row UMAP. Preparation took 1,342.12
seconds and reached 8.47 GiB maximum resident memory.

| Artifact                                   |   Exact bytes |
|--------------------------------------------|--------------:|
| Official source H5                         | 4,216,018,749 |
| Reusable Seurat output and BPCells sidecar | 3,778,378,648 |
| CRB and BPCells sidecar                    | 3,765,141,671 |
| CRB metadata file alone                    |    28,132,278 |

All of these files remain in the selected cache. The large data are
never added to Git.

## Run the R hot-path benchmark

The command-line interface is:

``` text
viewer_1m_hot_paths.R BEFORE_ROOT AFTER_ROOT CRB [REPEATS]
```

Run three repetitions and save the tab-separated result:

``` bash
Rscript "$BACKEND_ROOT/tests/bench/viewer_1m_hot_paths.R" \
  "$BASELINE_ROOT" \
  "$BACKEND_ROOT" \
  "$CRB" \
  3 \
  > "$BENCH_ROOT/backend_hot_paths.tsv"

column -t -s $'\t' "$BENCH_ROOT/backend_hot_paths.tsv"
```

For each operation the script:

1.  reads the same 1M-cell CRB and BPCells sidecar;
2.  sources the Viewer helpers from the before and after worktrees;
3.  warms each operation once;
4.  runs garbage collection before each timed repetition;
5.  reports the median elapsed time;
6.  records one R allocation profile with
    [`Rprofmem()`](https://rdrr.io/r/utils/Rprofmem.html); and
7.  stops immediately if the before and after results violate the
    operation’s equality or invariant check.

The measured three-repetition medians were:

| Operation | Scale | Before | After | Time | Before allocation | After allocation | Allocation |
|----|----|---:|---:|---:|---:|---:|---:|
| Full projection selection | 1M cells, all groups, 100% | 228 ms | 5 ms | -97.8% | 206.7 MiB | 11.4 MiB | -94.5% |
| Filtered projection selection | 1M cells, 17/33 clusters, 25% | 351 ms | 30 ms | -91.5% | 170.6 MiB | 43.3 MiB | -74.6% |
| Hover preparation | 1M loaded, 100K displayed, 3 groups | 33,877 ms | 2,438 ms | -92.8% | 1,328.7 MiB | 56.3 MiB | -95.8% |
| Single-gene expression | 1 gene x 1M cells, BPCells | 4,060 ms | 4,227 ms | +4.1% | 154.1 MiB | 169.3 MiB | +9.9% |
| RGB expression | 3 genes x 1M cells | 12,963 ms | 4,170 ms | -67.8% | 461.4 MiB | 238.3 MiB | -48.4% |
| Multi-panel expression | 9 genes x 1M cells | 4,336 ms | 4,311 ms | -0.6% | 562.3 MiB | 463.2 MiB | -17.6% |
| Mean expression | 100 genes x 1M cells | 5,622 ms | 3,988 ms | -29.1% | 1,916.8 MiB | 112.8 MiB | -94.1% |

[`Rprofmem()`](https://rdrr.io/r/utils/Rprofmem.html) measures
allocations known to R, not all native memory owned by BPCells or the
operating system. The browser benchmark below therefore also records
process RSS. The single-gene result is a small regression in this run
and must not be reported as an improvement. The main expression gains
come from batching RGB reads and keeping mean expression backend-native.

## Run the Viewer interaction microbenchmarks

These commands measure the interaction algorithms introduced between the
backend and Viewer revisions. They use deterministic synthetic inputs
and do not require the 1M-cell CRB.

``` bash
Rscript "$VIEWER_ROOT/tests/bench/viewer_interaction_hot_paths.R" "$VIEWER_ROOT" \
  > "$BENCH_ROOT/viewer_interaction_hot_paths.tsv"

node "$VIEWER_ROOT/tests/bench/viewer_interaction_hit_test.js" "$VIEWER_ROOT" \
  > "$BENCH_ROOT/viewer_interaction_browser.json"

column -t -s $'\t' "$BENCH_ROOT/viewer_interaction_hot_paths.tsv"
cat "$BENCH_ROOT/viewer_interaction_browser.json"
```

The measured medians were:

| Operation | Before | After | Change |
|----|---:|---:|---:|
| Configuration fingerprint, 500K cells | 139 ms | 128 ms | -7.9% |
| Hover payload and JSON, 50K cells | 1,224 ms | 37 ms | -97.0% |
| Spatial image, five cached renders | 281 ms | below 1 ms | timer resolution |
| Spatial hulls, five style changes | 413 ms | 69 ms | -83.3% |
| Hit testing, 500K cells | 1.229 ms/event | 0.0042 ms/event | -99.7% |

The hover payload fell from 8.65 MiB to 1.44 MiB. The hit-test index
took 13.34 ms to build and broke even after approximately 11 pointer
events.

## Run the browser loading benchmark

The browser command-line interface is:

``` text
viewer_1m_browser.R BEFORE_ROOT AFTER_ROOT CRB [REPEATS] [PERCENT]
```

`PERCENT` is the percentage of cells drawn in Overview and must be
between 10 and 100. The reported comparisons use 10, meaning that the
real 1M dataset is loaded while 100,000 points are drawn. Run all three
pairs to isolate backend, Viewer-interaction and cumulative changes:

``` bash
run_browser_pair() {
  comparison="$1"
  before_root="$2"
  after_root="$3"
  Rscript "$VIEWER_ROOT/tests/bench/viewer_1m_browser.R" \
    "$before_root" "$after_root" "$CRB" 3 10 \
    > "$BENCH_ROOT/browser_${comparison}.txt"
}

run_browser_pair release_to_backend "$BASELINE_ROOT" "$BACKEND_ROOT"
run_browser_pair backend_to_viewer "$BACKEND_ROOT" "$VIEWER_ROOT"
run_browser_pair release_to_viewer "$BASELINE_ROOT" "$VIEWER_ROOT"

for report in "$BENCH_ROOT"/browser_*.txt; do
  printf '\n== %s ==\n' "$(basename "$report")"
  cat "$report"
done
```

Each repetition performs the following checks and measurements:

1.  generates a temporary app that loads the selected Git revision;
2.  launches it through shinytest2 and Chrome;
3.  opens Data Info and waits until it reports exactly 1,000,000 cells;
4.  records `data_ready_ms`;
5.  opens Overview and waits for a non-zero Canvas;
6.  samples Canvas pixels and requires at least 1,000 non-white samples;
7.  rejects unexpected browser `error`, `fatal` or `unhandled` log
    messages;
8.  records the Shiny R process RSS; and
9.  alternates before/after execution order between rounds.

Each output has a `RAW` table with every run and a `SUMMARY` table
containing the medians. The three-round results were:

| Comparison | Data ready | Overview Canvas | Total | Shiny RSS |
|----|---:|---:|---:|---:|
| Release to backend | 16.71 to 15.59 s (-6.7%) | 45.38 to 18.28 s (-59.7%) | 62.08 to 33.87 s (-45.4%) | 3,464.7 to 2,435.3 MiB (-29.7%) |
| Backend to Viewer | 15.38 to 14.72 s (-4.3%) | 18.25 to 1.52 s (-91.7%) | 33.68 to 16.30 s (-51.6%) | 2,154.6 to 1,913.9 MiB (-11.2%) |
| Release to Viewer | 15.49 to 15.10 s (-2.5%) | 44.58 to 1.59 s (-96.4%) | 59.70 to 16.69 s (-72.0%) | 3,513.1 to 1,904.7 MiB (-45.8%) |

Every run loaded 1,000,000 cells, painted the same 1301-by-594 Canvas
and passed the browser-log check. Absolute medians vary slightly between
pairwise runs; use each alternating pair only for its stated comparison.

## Validate full 1M-point Canvas rendering

The 10% comparison exercises the intended large-data default. To
validate the chunked Canvas path, run the Viewer revision as both inputs
and request 100% rendering:

``` bash
Rscript "$VIEWER_ROOT/tests/bench/viewer_1m_browser.R" \
  "$VIEWER_ROOT" \
  "$VIEWER_ROOT" \
  "$CRB" \
  2 \
  100 \
  > "$BENCH_ROOT/full_1m_canvas.txt"

cat "$BENCH_ROOT/full_1m_canvas.txt"
```

This is a rendering acceptance check, not a revision comparison. The
measured runs painted all 1M UMAP points in 72.4 and 73.5 seconds, using
4,616.7 and 4,368.5 MiB RSS.

## Use the prepared example normally

Launch it directly from the source checkout:

``` r
devtools::load_all(".")

launchCerebro(
  example_data_size = "1m",
  example_data_dir = Sys.getenv("CEREBRO_LARGE_CACHE"),
  percentage_cells_to_show = 10
)
```

Or package it together with other Cerebro datasets:

``` r
devtools::load_all(".")

createShinyApp(
  cerebro_data = c("Small dataset" = "/absolute/path/small.crb"),
  example_data_size = "1m",
  example_data_dir = Sys.getenv("CEREBRO_LARGE_CACHE"),
  result_dir = "/absolute/path/cerebro-large-app",
  launch_browser = FALSE
)
```

## Interpret and compare runs

Use the medians, not a single fastest run. Keep the following fixed
between versions:

- CRB and BPCells sidecar;
- display percentage and browser viewport;
- R, package and browser versions;
- machine power mode and competing workloads; and
- number of repetitions.

The first read may populate operating-system disk caches. Alternating
browser order reduces, but does not eliminate, thermal and cache bias.
Report the raw runs alongside medians, and do not describe a sub-percent
difference as a speedup without additional repetitions.

## Clean up comparison worktrees

The cache is intentionally retained. Remove only the temporary Git
worktrees after collecting the result files you need:

``` bash
git -C "$REPO_ROOT" worktree remove "$BASELINE_ROOT"
git -C "$REPO_ROOT" worktree remove "$BACKEND_ROOT"
git -C "$REPO_ROOT" worktree remove "$VIEWER_ROOT"
```

The benchmark result files remain under `$BENCH_ROOT`. Delete that
temporary directory separately only after copying any results you want
to keep.
