#!/usr/bin/env bash
set -eu
case "$(basename "$0")" in
  R) printf 'install\n' >> "$BENCH_TEST_LOG" ;;
  curl)
    while [ "$#" -gt 0 ]; do
      if [ "$1" = '-o' ]; then printf 'tiny data\n' > "$2"; break; fi
      shift
    done ;;
  rm)
    for argument in "$@"; do
      case "$argument" in
        -*) ;;
        "$BENCH_TEST_ROOT"/*) printf 'remove %s\n' "$argument" >> "$BENCH_TEST_LOG" ;;
        *) echo "unsafe test cleanup: $argument" >&2; exit 1 ;;
      esac
    done
    exec /bin/rm "$@" ;;
  Rscript)
    if [ "$1" = '-e' ]; then
      case "$2" in *BENCH_SOURCES*) echo 'https://example.invalid/tiny.h5ad' ;; *) echo 'tiny' ;; esac
      exit 0
    fi
    stage=$(basename "$1")
    printf '%s\n' "$stage" >> "$BENCH_TEST_LOG"
    if [ "${BENCH_TEST_STOP:-}" = "$stage" ]; then exit 7; fi
    if [ "${BENCH_TEST_SIGNAL:-}" = "$stage" ]; then kill -TERM "$PPID"; exit 0; fi
    case "$stage" in
      03_plan_runs.R) printf 'publication\ttiny\t2\tpaired\t1\t1\trds\t1\n' > "$3" ;;
      10_export_backend.R)
        mkdir -p "$7/export/${2}_${3}_${4}_r${5}"
        touch "$7/export/${2}_${3}_${4}_r${5}/bench.crb" ;;
    esac ;;
esac
