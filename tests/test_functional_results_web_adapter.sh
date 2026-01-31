#!/usr/bin/env bash
set -euo pipefail

URL="http://localhost:50080/input"

run_case() {
  local title="$1"
  local q="$2"

  local out
  out="$(
    curl -fsS \
      --retry 30 --retry-connrefused --retry-delay 0 --max-time 15 \
      --get --data-urlencode "msg=$q" \
      "$URL"
  )"

  # Must return a non-empty list (not "No books found...")
  echo "$out" | head -n 1 | grep -Eq '^Found [1-9][0-9]* books:'

  # Must contain at least one book line: [id] ...
  echo "$out" | grep -Eq '^\[[^]]+\] '

  echo "PASS: $title"
}

echo "=== Functional RESULTS via web-adapter ==="

run_case "1) simple keyword" "Кинг"
run_case "2) field author"   "author:Кинг"

echo "ALL PASS: web-adapter functional results"
