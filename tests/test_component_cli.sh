#!/usr/bin/env bash
set -euo pipefail

CLI="./bin/ebusta-cli"
test -x "$CLI" || { echo "FAIL: $CLI not found/executable (build via make build-cli or make test-stack)"; exit 1; }

out="$("$CLI" "author:Кинг" 2>&1 || true)"

# CLI currently does NOT exit non-zero on gRPC errors -> detect by output
echo "$out" | grep -q "❌ Error:" && { echo "FAIL: cli reported error"; echo "$out"; exit 1; }

# Accept either results or explicit empty message
echo "$out" | grep -Eq '(No results found\.|Found [0-9]+ books:|ID\s*\|\s*Title\s*\|\s*Authors)' || {
  echo "FAIL: unexpected cli output"
  echo "$out"
  exit 1
}

echo "PASS: cli contract ok"
