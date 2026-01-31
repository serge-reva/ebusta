#!/usr/bin/env bash
set -euo pipefail

out="$(
  curl -fsS \
    --retry 20 --retry-connrefused --retry-delay 0 --max-time 10 \
    --get --data-urlencode "msg=author:Кинг" \
    "http://localhost:50080/input"
)"

# Contract: must be either "Found N books:" or "No books found for:"
echo "$out" | grep -Eq '^(Found [0-9]+ books:|No books found for:)'

echo "PASS: web-adapter /input contract ok"
