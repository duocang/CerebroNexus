#!/usr/bin/env bash
# Shared Linux/WSL replacement for the page and release comparison wrappers.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
repo="$(cd "$here/../../../.." && pwd -P)"
workflow="${1:-pages}"
if (( $# )); then shift; fi
fail() { printf '%s\n' "$*" >&2; exit 2; }
case "$workflow" in pages|release) ;; *) fail 'Expected pages or release';; esac
artifact= sidecar= output= run_id= pages= rounds=0
baseline=892097a10d013fe9db0c35a78b4ced0dd09a3fdd
reference=6c9718d78ef5e8f895281aad95d0c6594a8026ba
candidate=HEAD profile=quick mode=timing visits=first,repeat
if [[ "$workflow" == release ]]; then rounds=3; visits=first; fi
while (( $# )); do
  case "$1" in
    -h|--help)
      cat <<'HELP'
Usage: bash tests/benchmarks/viewer/run.sh {pages|release} --artifact FILE [OPTIONS]
  --reference REF   Reference revision (default: fixed PR5, 6c9718d7)
  --candidate REF   Candidate revision (default: committed HEAD)
  --baseline REF    Third revision for viewer (default: pre-PR0, 892097a1)
  --profile NAME    quick (default) or publication (viewer only)
  --mode NAME       timing (default) or memory (viewer only)
  --rounds N        1..100; viewer defaults to 1 quick / 5 publication;
                    release defaults to 3
  --pages LIST      Comma-separated pages (default: all available)
  --visits LIST     first,repeat (viewer default), first, or repeat
  --sidecar DIR     Release BPCells sidecar (auto-detect exactly one *.bpcells)
  --output DIR      Output root (default: sibling CerebroNexus-benchmarks)
  --run-id NAME     Unique result directory name (default: UTC timestamp + SHAs)
  --repo DIR       Repository containing candidate revisions

Publication requires >=5 rounds. Release uses quick/timing/first, with a
separate CRB and Viewer Pack per revision. Failed measurements remain on disk
and produce a nonzero exit status. Run inside the project's Linux R environment.
HELP
      exit 0 ;;
    --artifact|--sidecar|--output|--run-id|--pages|--rounds|--baseline|--reference|--candidate|--profile|--mode|--visits|--repo)
      (( $# >= 2 )) && [[ -n "$2" ]] || fail "Missing value for $1"
      key="${1#--}"; key="${key//-/_}"
      printf -v "$key" '%s' "$2"
      shift 2 ;;
    *) fail "Unknown option: $1" ;;
  esac
done
[[ "$profile" == quick || "$profile" == publication ]] || fail 'Invalid profile'
[[ "$mode" == timing || "$mode" == memory ]] || fail 'Invalid mode'
case "$visits" in first|repeat|first,repeat|repeat,first) ;; *) fail 'Invalid visits';; esac
[[ "$rounds" =~ ^[0-9]{1,3}$ ]] || fail 'Rounds must be an integer from 1 to 100'
rounds=$((10#$rounds))
if (( rounds == 0 )); then
  [[ "$workflow" == pages ]] || fail 'Release requires at least one round'
  if [[ "$profile" == publication ]]; then rounds=5; else rounds=1; fi
fi
(( rounds >= 1 && rounds <= 100 )) || fail 'Rounds must be between 1 and 100'
[[ "$profile" != publication ]] || (( rounds >= 5 )) || fail 'Publication requires at least five rounds'
if [[ "$workflow" == release ]]; then
  [[ "$profile/$mode/$visits" == quick/timing/first ]] || fail 'Release uses quick/timing/first'
fi
[[ -n "$artifact" && -f "$artifact" ]] || fail 'Specify --artifact with an existing CRB file'
command -v Rscript >/dev/null || fail 'Rscript is required'
repo="$(realpath -e -- "$repo")"
artifact="$(realpath -e -- "$artifact")"
git -C "$repo" rev-parse --is-inside-work-tree >/dev/null
output="$(realpath -m -- "${output:-$(dirname "$repo")/CerebroNexus-benchmarks}")"
if [[ "$workflow" == release ]]; then
  if [[ -z "$sidecar" ]]; then
    shopt -s nullglob
    sidecars=("$(dirname "$artifact")"/*.bpcells)
    (( ${#sidecars[@]} == 1 )) || fail 'Specify --sidecar: expected exactly one *.bpcells directory'
    sidecar="${sidecars[0]}"
  fi
  [[ -d "$sidecar" ]] || fail 'BPCells sidecar must be a directory'
  sidecar="$(realpath -e -- "$sidecar")"
fi
labels=(pr5 latest); refs=("$reference" "$candidate")
if [[ "$workflow" == pages ]]; then labels=(before_pr0 "${labels[@]}"); refs=("$baseline" "${refs[@]}"); fi
commits=() worktrees=() shorts=()
for ref in "${refs[@]}"; do
  sha="$(git -C "$repo" rev-parse --verify --end-of-options "$ref^{commit}")"
  commits+=("$sha"); shorts+=("${sha:0:8}")
done
run_id="${run_id:-$(date -u +%Y%m%dT%H%M%SZ)-$workflow-$profile-$(IFS=-; echo "${shorts[*]}")}"
[[ "$run_id" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] || fail 'Invalid run ID: use letters, digits, dot, underscore, hyphen'
result="$output/results/$run_id"
fixtures="$output/fixtures/$run_id"
[[ ! -e "$result" && ! -e "$fixtures" ]] || fail "Run already exists: $run_id"
mkdir -p "$output/results" "$output/worktrees"
mkdir "$result"
config="$result/run-config.tsv"
{
  printf 'key\tvalue\n'
  printf '%s\t%s\n' run_id "$run_id" workflow "$workflow" profile "$profile" \
    benchmark_mode "$mode" rounds "$rounds" visits "$visits" pages "$pages" \
    artifact "$artifact" sidecar "$sidecar" harness_root "$here" \
    harness_git_sha "$(git -C "$here" rev-parse HEAD)" \
    harness_git_dirty "$(if [[ -n "$(git -C "$here" status --porcelain)" ]]; then echo true; else echo false; fi)"
} > "$config"
for i in "${!labels[@]}"; do
  label="${labels[$i]}"; sha="${commits[$i]}"
  tree="$output/worktrees/$label-${sha:0:8}"
  if [[ ! -e "$tree" ]]; then git -C "$repo" worktree add --detach "$tree" "$sha"; fi
  [[ "$(git -C "$tree" rev-parse HEAD)" == "$sha" ]] || fail "Wrong revision in $tree"
  [[ -z "$(git -C "$tree" status --porcelain)" ]] || fail "Dirty benchmark worktree: $tree"
  worktrees+=("$tree")
  printf '%s\t%s\n' "$label.ref" "${refs[$i]}" "$label.sha" "$sha" "$label.worktree" "$tree" >> "$config"
done
export VIEWER_BENCH_PROFILE="$profile" VIEWER_BENCH_MODE="$mode"
export VIEWER_VISITS_ONLY="$visits" VIEWER_PAGES_ONLY="$pages"
benchmark="$here/benchmark_viewer_1m_pages.R"
summarizer="$here/summarize_viewer_page_comparison.R"
raw="$result/raw.tsv"
status=0
# Keep the first failure, while still reporting any measurements already written.
measure() {
  local log=$1 code=0
  shift
  Rscript "$benchmark" "$@" 2>&1 | tee "$log" || code=$?
  if (( status == 0 && code != 0 )); then status=$code; fi
}
printf 'Results: %s\n' "$result"
if [[ "$workflow" == pages ]]; then
  candidates=()
  for i in "${!labels[@]}"; do candidates+=("${labels[$i]}=${worktrees[$i]}"); done
  measure "$result/run.log" "${candidates[0]}" "$artifact" "$raw" "$rounds" "${candidates[@]:1}"
else
  for i in "${!labels[@]}"; do
    label="${labels[$i]}"; fixture="$fixtures/$label"
    mkdir -p "$fixture"
    cp --reflink=auto -- "$artifact" "$fixture/$(basename "$artifact")"
    ln -s -- "$sidecar" "$fixture/$(basename "$sidecar")"
    crb="$fixture/$(basename "$artifact")"
    printf '%s\t%s\n' "$label.artifact" "$crb" >> "$config"
    code=0
    Rscript -e 'a <- commandArgs(TRUE); pkgload::load_all(a[1], quiet=TRUE); buildViewerPack(a[2], viewer_binary="always", overwrite=TRUE)' \
      "${worktrees[$i]}" "$crb" > "$result/$label-build.log" 2>&1 || code=$?
    if (( code != 0 )); then
      printf 'Pack build failed for %s; see %s\n' "$label" "$result/$label-build.log" >&2
      if (( status == 0 )); then status=$code; fi
      continue
    fi
    measure "$result/$label.log" "$label=${worktrees[$i]}" "$crb" "$result/$label.tsv" "$rounds"
  done
  code=0
  Rscript -e 'a <- commandArgs(TRUE); paths <- a[-1]; paths <- paths[file.exists(paths)]; if (!length(paths)) stop("No candidate results"); x <- do.call(rbind, lapply(paths, read.delim)); write.table(x, a[1], sep="\t", row.names=FALSE, quote=FALSE, na="NA")' \
    "$raw" "$result/pr5.tsv" "$result/latest.tsv" || code=$?
  if (( status == 0 && code != 0 )); then status=$code; fi
fi
if [[ -f "$raw" ]]; then
  code=0
  Rscript "$summarizer" "$raw" "$result" || code=$?
  if (( status == 0 && code != 0 )); then status=$code; fi
else
  printf 'No raw results; inspect logs in %s\n' "$result" >&2
  if (( status == 0 )); then status=1; fi
fi
exit "$status"
