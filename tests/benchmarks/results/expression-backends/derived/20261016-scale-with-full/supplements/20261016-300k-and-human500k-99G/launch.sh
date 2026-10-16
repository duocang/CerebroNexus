#!/usr/bin/env bash
set -uo pipefail
source /mnt/c/Users/wangx/AppData/Local/Temp/cerebro-supplement-20261016-081232/env.sh
trap 'printf "%s\n" "$?" > "$EXTRA/runner.exit"' EXIT
exec >> "$EXTRA/runner.log" 2>&1
date -Is
echo 'Verifying cached source SHA256 before starting any measurements'
sha256sum "$BENCH_SOURCE_CACHE/1M_neurons_filtered_gene_bc_matrices_h5.h5" "$BENCH_SOURCE_CACHE/d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad" > "$EXTRA/source-sha256.txt"
grep -q '^255a36ee92de25cb3568faa2c27d31fe6d0db30f285c5c977be8d6245de14044 ' "$EXTRA/source-sha256.txt" || exit 11
grep -q '^aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f ' "$EXTRA/source-sha256.txt" || exit 12
systemd-run --user --wait --pipe --unit=cerebro-scale300-20261016-081232 \
  -p MemoryMax=90G -p MemorySwapMax=0 -p OOMPolicy=kill -p RuntimeMaxSec=21600 \
  -p Nice=10 -p IOWeight=10 \
  /nix/var/nix/profiles/default/bin/nix-shell "$REPO/default.nix" -A shell \
  --run "bash $EXTRA/run300.sh" > "$EXTRA/scale300.log" 2>&1
code300=$?
printf '%s\n' "$code300" > "$EXTRA/scale300.exit"
systemctl --user show cerebro-scale300-20261016-081232.service -p Result -p ExecMainStatus -p MemoryPeak -p MemorySwapPeak -p MemoryMax -p MemorySwapMax > "$EXTRA/scale300.service.txt"
echo "300K_FINISHED exit=$code300 $(date -Is)"
available=$(awk '/^MemAvailable:/ {print $2}' /proc/meminfo)
if [ "$available" -lt 104857600 ]; then
  echo "500K_NOT_STARTED: less than 100 GiB available ($available KiB)"
  echo 125 > "$EXTRA/human500.exit"
  exit 125
fi
systemd-run --user --wait --pipe --unit=cerebro-human500-99g-20261016-081232 \
  -p MemoryMax=99G -p MemorySwapMax=0 -p OOMPolicy=kill -p RuntimeMaxSec=7200 \
  -p Nice=10 -p IOWeight=10 \
  /nix/var/nix/profiles/default/bin/nix-shell "$REPO/default.nix" -A shell \
  --run "bash $EXTRA/run500.sh" > "$EXTRA/human500-99G/run.log" 2>&1
code500=$?
printf '%s\n' "$code500" > "$EXTRA/human500.exit"
systemctl --user show cerebro-human500-99g-20261016-081232.service -p Result -p ExecMainStatus -p MemoryPeak -p MemorySwapPeak -p MemoryMax -p MemorySwapMax > "$EXTRA/human500-99G/service-result.txt"
echo "500K_FINISHED exit=$code500 $(date -Is)"
printf '300k=%s\n500k=%s\n' "$code300" "$code500" > "$EXTRA/runner.complete"
