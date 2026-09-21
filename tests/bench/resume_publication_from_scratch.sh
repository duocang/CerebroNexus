#!/usr/bin/env bash
# Finalize a measurement-complete publication run retained after a reporting
# or validation failure. This never repeats builds or access measurements.

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
SCRIPT="$REPO/tests/bench/resume_publication_from_scratch.sh"

if [ "${1:-}" != "--inside" ]; then
  [ "$#" -eq 1 ] || {
    echo "usage: $0 <retained-cerebro-bench-scratch>" >&2
    exit 2
  }
  [ -f "$1/.cerebro-benchmark-scratch" ] || {
    echo "not a retained CerebroNexus benchmark scratch directory: $1" >&2
    exit 1
  }
  export BENCH_RECOVERY_SCRATCH="$(cd "$1" && pwd -P)"
  command -v nix-shell >/dev/null 2>&1 || {
    echo "nix-shell is required" >&2
    exit 1
  }
  exec nix-shell "$REPO/default.nix" -A shell --run \
    "bash '$SCRIPT' --inside"
fi

SCRATCH="${BENCH_RECOVERY_SCRATCH:-}"
[ -n "$SCRATCH" ] && [ -f "$SCRATCH/.cerebro-benchmark-scratch" ] || {
  echo "BENCH_RECOVERY_SCRATCH is missing or unsafe" >&2
  exit 1
}
STAGE="$SCRATCH/result"
MANIFEST="$STAGE/run_manifest.csv"
[ -f "$MANIFEST" ] || {
  echo "retained run has no run_manifest.csv" >&2
  exit 1
}

read_manifest_value() {
  Rscript -e 'm <- read.csv(commandArgs(TRUE)[1], stringsAsFactors=FALSE); key <- commandArgs(TRUE)[2]; value <- m$value[m$key == key]; if (length(value) != 1L || is.na(value) || !nzchar(value)) quit(status=1L); cat(value)' "$MANIFEST" "$1"
}

export BENCH_ROOT="$REPO/tests/bench"
export BENCH_SCRATCH="$SCRATCH"
export BENCH_LIB="$SCRATCH/rlib"
export R_LIBS_USER="$SCRATCH/r-user-library"
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
export NOT_CRAN=true
export BENCH_RUN_ID="$(read_manifest_value run_id)"
export BENCH_STUDY_ID="$(read_manifest_value study_id)"
export BENCH_PROFILE="$(read_manifest_value profile)"
RESULT_ROOT="${BENCH_RESULT_ROOT:-$BENCH_ROOT/result/publication-full}"
STATE_DIR="${BENCH_STATE_DIR:-/home/xuesong/.cache/cerebronexus-benchmark/runner}"

echo "==> resuming completed measurements: $BENCH_RUN_ID"
Rscript -e '
  path <- commandArgs(TRUE)[1]
  m <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  old <- m$key == "package_version.Version"
  if (!any(m$key == "package_version") && sum(old) == 1L) {
    m$key[old] <- "package_version"
    write.csv(m, path, row.names = FALSE, na = "")
  }
' "$MANIFEST"
Rscript "$BENCH_ROOT/src/30_check_measurements.R" "$STAGE"
Rscript "$BENCH_ROOT/src/40_write_report.R" "$STAGE"
mkdir -p "$STAGE/logs"
Rscript "$BENCH_ROOT/src/41_draw_figures.R" "$STAGE" "$STAGE/figures" \
  > "$STAGE/logs/figures-recovery.log" 2>&1
Rscript "$BENCH_ROOT/src/49_write_evidence_manifest.R" "$STAGE"
Rscript "$BENCH_ROOT/src/50_check_outputs.R" "$STAGE"
Rscript "$BENCH_ROOT/src/60_publish_results.R" \
  "$STAGE" "$RESULT_ROOT" "$BENCH_RUN_ID"
mkdir -p "$STATE_DIR"
printf '0\n' > "$STATE_DIR/publication-full.exit.tmp"
mv -f -- "$STATE_DIR/publication-full.exit.tmp" \
  "$STATE_DIR/publication-full.exit"
echo "==> recovered and published: $RESULT_ROOT/runs/$BENCH_RUN_ID"
