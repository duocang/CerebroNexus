#!/usr/bin/env bash
# precheck.sh — local mirror of the GitHub Actions checks, run in the order CI runs them.
#
# CI pipeline this reproduces (.github/workflows/):
#   style.yaml       -> air format        (reformats code; THIS is what breaks format-sensitive regex tests)
#   R-tests.yaml     -> testthat::test_dir
#   R-cmd-check.yaml -> devtools::check
#   pkgdown.yaml     -> pkgdown::build_site
#
# Why air runs FIRST: on GitHub, code is air-formatted on push BEFORE tests run, so tests
# always see formatted code. Running air first locally keeps local == CI.
#
# Usage:
#   scripts/precheck.sh           # full: air + tests + R CMD check + pkgdown
#   scripts/precheck.sh fast      # air + tests only (quick, catches the common regex/format breakage)
#   scripts/precheck.sh air       # air format only
set -uo pipefail

cd "$(git rev-parse --show-toplevel)" || { echo "not in a git repo"; exit 2; }

MODE="${1:-full}"
RED=$'\033[31m'; GRN=$'\033[32m'; YEL=$'\033[33m'; RST=$'\033[0m'
fail=0
step() { echo; echo "${YEL}==== $* ====${RST}"; }
ok()   { echo "${GRN}PASS:${RST} $*"; }
bad()  { echo "${RED}FAIL:${RST} $*"; fail=1; }

command -v air >/dev/null    || { echo "${RED}air not found${RST} (brew install air)"; exit 2; }
command -v Rscript >/dev/null || { echo "${RED}Rscript not found${RST}"; exit 2; }

# ---- 0. kill stray apps ----
# A hand-started app left listening makes the shinytest2 tests fail at random
# -- a few assertions short, or a data.frame error raised inside AppDriver --
# because the driver can attach to the wrong process.
#
# Both entry points, not just runApp: `launchCerebroBuilder()` and
# `launchCerebro()` start a server whose command line never says "runApp", so a
# runApp-only pattern walks straight past them. That is not hypothetical --
# a launchCerebroBuilder() left on a port swallowed a restart here, and the
# stale process kept serving while the new one died with "Failed to create
# server", which reads exactly like a fix that did not work.
STRAY='runApp|launchCerebro'
if [ "$MODE" != "air" ] && pgrep -f "$STRAY" >/dev/null 2>&1; then
  echo "${YEL}killing stray Shiny process(es) before testing${RST}"
  pkill -f "$STRAY" || true
  sleep 1
fi

# ---- 1. air format (mirrors style.yaml) ----
step "1/4 air format ."
air format .
if air format --check . >/dev/null 2>&1; then
  ok "code is air-clean"
else
  ok "code reformatted by air (see 'git diff' for what changed)"
fi
[ "$MODE" = "air" ] && { echo; echo "${GRN}air-only done.${RST}"; exit 0; }

# ---- 2. testthat (mirrors R-tests.yaml) ----
step "2/4 testthat::test_dir"
Rscript -e "
  suppressMessages(devtools::load_all('.'))
  out <- testthat::test_dir('tests/testthat',
    reporter = testthat::default_reporter(), stop_on_failure = FALSE)
  df <- as.data.frame(out)
  if (sum(df\$failed) > 0 || any(df\$error)) quit(status = 1)
" && ok "all tests passed" || bad "testthat failures (see above)"

if [ "$MODE" = "fast" ]; then
  echo; [ "$fail" -eq 0 ] && echo "${GRN}fast precheck PASSED.${RST}" || echo "${RED}fast precheck FAILED.${RST}"
  exit "$fail"
fi

# ---- 3. R CMD check (mirrors R-cmd-check.yaml) ----
step "3/4 devtools::check"
Rscript -e "devtools::check(vignettes = TRUE, error_on = 'error')" \
  && ok "R CMD check clean" || bad "R CMD check errors (see above)"

# ---- 4. pkgdown (mirrors pkgdown.yaml) ----
step "4/4 pkgdown::build_site"
Rscript -e "pkgdown::build_site(preview = FALSE)" \
  && ok "pkgdown built" || bad "pkgdown build failed (see above)"

echo
if [ "$fail" -eq 0 ]; then
  echo "${GRN}full precheck PASSED — local mirrors CI, safe to commit/push.${RST}"
else
  echo "${RED}full precheck FAILED — fix above before pushing.${RST}"
fi
exit "$fail"
