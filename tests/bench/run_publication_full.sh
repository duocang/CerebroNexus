#!/usr/bin/env bash

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
export BENCH_ROOT="$REPO/tests/bench"
export BENCH_THREADS="${BENCH_THREADS:-1}"
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
RESULT_ROOT="${BENCH_RESULT_ROOT:-$BENCH_ROOT/result/publication-full}"

if [ -z "${BENCH_SOURCE_CACHE:-}" ]; then
  echo "BENCH_SOURCE_CACHE is required for the publication-full study" >&2
  exit 1
fi
if [ -z "${BENCH_STORAGE_DESCRIPTION:-}" ]; then
  echo "BENCH_STORAGE_DESCRIPTION is required for the publication-full study" >&2
  exit 1
fi
if [ -n "${BENCH_SOURCES_ONLY:-}${BENCH_SOURCES_EXTRA:-}" ]; then
  echo "publication-full owns the exact two-source design; source overrides are not allowed" >&2
  exit 1
fi
mkdir -p "$BENCH_SOURCE_CACHE"
source_cache_path=$(cd "$BENCH_SOURCE_CACHE" && pwd -P)
case "$source_cache_path/" in
  "$REPO/"*)
    echo "BENCH_SOURCE_CACHE must be outside the Git checkout" >&2
    exit 1
    ;;
esac

head_sha=$(git -C "$REPO" rev-parse HEAD)
export BENCH_STUDY_ID="${BENCH_STUDY_ID:-$(date -u +%Y%m%dT%H%M%SZ)-${head_sha:0:12}-publication-full}"
if [[ ! "$BENCH_STUDY_ID" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]]; then
  echo "unsafe BENCH_STUDY_ID: $BENCH_STUDY_ID" >&2
  exit 1
fi

tracked=$(git -C "$REPO" status --porcelain --untracked-files=no)
untracked=$(git -C "$REPO" ls-files --others --exclude-standard -- \
  . ":(exclude,glob)tests/bench/result/**" \
  ":(exclude,glob)tests/bench/study-work/**")
if [ -n "$tracked$untracked" ]; then
  echo "publication-full requires a clean Git worktree" >&2
  exit 1
fi

BENCH_PROFILE=panel_c2 \
  BENCH_RUN_ID="$BENCH_STUDY_ID" \
  BENCH_RESULT_ROOT="$RESULT_ROOT" \
  "$BENCH_ROOT/run_sweep.sh"
