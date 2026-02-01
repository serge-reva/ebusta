#!/usr/bin/env bash
set -euo pipefail

CLI="./bin/ebusta-cli"
test -x "$CLI" || { echo "FAIL: $CLI not found/executable (run make build-cli or make test-stack)"; exit 1; }

run_case() {
  local title="$1"
  local q="$2"

  local out
  out="$("$CLI" "$q" 2>&1 || true)"

  # Any ❌ => fail
  if grep -q '❌' <<<"$out"; then
    echo "FAIL: $title (cli error)"
    echo "$out"
    exit 1
  fi

  # Must NOT be empty results
  if grep -q 'No results found\.' <<<"$out"; then
    echo "FAIL: $title (no results)"
    echo "$out"
    exit 1
  fi

  # Must have header line with ID | Title | Authors
  if ! echo "$out" | head -n 1 | grep -qE '^ID[[:space:]]+\|[[:space:]]+Title[[:space:]]+\|[[:space:]]+Authors'; then
    echo "FAIL: $title (no header)"
    echo "$out"
    exit 1
  fi

  # Must have at least one data row (line with two | separators, not header, not separator)
  local data_rows
  data_rows=$(echo "$out" | grep -E '\|.*\|' | grep -v '^ID[[:space:]]' | grep -v '^-' | wc -l)
  
  if [ "$data_rows" -lt 1 ]; then
    echo "FAIL: $title (no book rows)"
    echo "$out"
    exit 1
  fi

  echo "PASS: $title"
}

echo "=== Functional RESULTS via CLI argv ==="

run_case "1) simple keyword" "Кинг"
run_case "2) field author"   "author:Кинг"
run_case "3) explicit id:"   "id:0ef3a71859cce7a48d27493cdeabd6901c6d1e43"
run_case "4) implicit SHA"   "0ef3a71859cce7a48d27493cdeabd6901c6d1e43"

echo "ALL PASS: cli functional results"
