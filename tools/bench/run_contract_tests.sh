#!/usr/bin/env bash
# Run repository-only benchmark contracts in the pinned environment without
# allowing personal R libraries or startup files to override it.

set -uo pipefail

unset R_LIBS R_LIBS_USER
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null

BENCH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec Rscript "$BENCH_ROOT/run_contract_tests.R"
