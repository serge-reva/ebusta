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

  # Must have header + at least one row
  echo "$out" | head -n 1 | grep -Eq '^ID[[:space:]]+\|[[:space:]]+Title[[:space:]]+\|[[:space:]]+Authors'

  # Any row line (exclude header and separator): "<something> | <something> | <something>"
  echo "$out" | grep -E '^[^|]{2,40}[[:space:]]\|[[:space:]].+\|[[:space:]].+' | grep -vq '^ID[[:space:]]\|' || {
    echo "FAIL: $title (no book rows)"
    echo "$out"
    exit 1
  }

  echo "PASS: $title"
}

echo "=== Functional RESULTS via CLI argv ==="

run_case "1) simple keyword" "Кинг"
run_case "2) field author"   "author:Кинг"

echo "ALL PASS: cli functional results"
