#!/usr/bin/env bash

bench_sha256_file() {
  local path=$1
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$path" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$path" | awk '{print $1}'
  elif command -v openssl >/dev/null 2>&1; then
    openssl dgst -sha256 "$path" | awk '{print $NF}'
  else
    echo "no SHA-256 implementation found" >&2
    return 1
  fi
}

bench_fetch_source() {
  local url=$1
  local expected_bytes=$2
  local scratch_dir=$3
  local name
  local file
  local part
  local bytes
  local sha
  local recorded_sha
  local cache_root
  name=$(basename "${url%%\?*}")
  mkdir -p "$scratch_dir"

  if [ -n "${BENCH_SOURCE_CACHE:-}" ]; then
    mkdir -p "$BENCH_SOURCE_CACHE"
    cache_root=$(cd "$BENCH_SOURCE_CACHE" && pwd) || return 1
    file="$cache_root/$name"
    if [ -f "$file" ]; then
      if [ ! -f "$file.sha256" ]; then
        echo "cached source has no SHA-256 sidecar: $file" >&2
        return 1
      fi
      recorded_sha=$(tr -d '[:space:]' < "$file.sha256")
      sha=$(bench_sha256_file "$file") || return 1
      bytes=$(wc -c < "$file" | tr -d '[:space:]')
      if [ "$sha" != "$recorded_sha" ] || [ "$bytes" != "$expected_bytes" ]; then
        echo "cached source failed checksum or size validation: $file" >&2
        return 1
      fi
    else
      part="$file.part"
      curl -fL --retry 3 --retry-delay 5 --continue-at - \
        -o "$part" "$url" || return 1
      bytes=$(wc -c < "$part" | tr -d '[:space:]')
      if [ "$bytes" != "$expected_bytes" ]; then
        echo "downloaded source has unexpected size: $bytes != $expected_bytes" >&2
        return 1
      fi
      sha=$(bench_sha256_file "$part") || return 1
      mv "$part" "$file"
      printf '%s\n' "$sha" > "$file.sha256.tmp"
      mv "$file.sha256.tmp" "$file.sha256"
    fi
    BENCH_FETCHED_FILE="$scratch_dir/$name"
    ln -sfn "$file" "$BENCH_FETCHED_FILE"
  else
    BENCH_FETCHED_FILE="$scratch_dir/$name"
    part="$BENCH_FETCHED_FILE.part"
    curl -fL --retry 3 --retry-delay 5 --continue-at - \
      -o "$part" "$url" || return 1
    bytes=$(wc -c < "$part" | tr -d '[:space:]')
    if [ "$bytes" != "$expected_bytes" ]; then
      echo "downloaded source has unexpected size: $bytes != $expected_bytes" >&2
      return 1
    fi
    sha=$(bench_sha256_file "$part") || return 1
    mv "$part" "$BENCH_FETCHED_FILE"
  fi

  BENCH_FETCHED_BYTES=$bytes
  BENCH_FETCHED_SHA256=$sha
}
