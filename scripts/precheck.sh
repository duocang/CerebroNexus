#!/usr/bin/env bash
# Local checks mirroring the CI test groups without changing the worktree.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

run_group() {
  Rscript scripts/run-test-shard.R --group "$1" --shard 1 --shards 1
}

case "${1:-fast}" in
  air)
    command -v air >/dev/null || { echo "air not found (brew install air)"; exit 2; }
    air format --check .
    ;;
  fast)
    R CMD INSTALL .
    run_group logic
    run_group process-sensitive
    ;;
  full)
    R CMD INSTALL .
    run_group logic
    run_group process-sensitive
    CEREBRO_RUN_BROWSER_TESTS=true run_group browser
    Rscript -e "devtools::check(args = c('--no-tests'), vignettes = TRUE, error_on = 'warning')"
    Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE, dest_dir = 'pkgdown-site')"
    ;;
  *)
    echo "Usage: scripts/precheck.sh [air|fast|full]" >&2
    exit 2
    ;;
esac
