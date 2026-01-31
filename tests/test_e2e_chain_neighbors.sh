#!/usr/bin/env bash
set -euo pipefail

get_counter() {
  curl -fsS \
    --retry 20 --retry-connrefused --retry-delay 0 --max-time 10 \
    "http://localhost:50090/metrics" \
  | awk '/^orchestrator_requests_total[[:space:]]+/ {print $2; exit}'
}

before="$(get_counter || true)"
before="${before:-0}"

out="$(
  curl -fsS \
    --retry 20 --retry-connrefused --retry-delay 0 --max-time 10 \
    --get --data-urlencode "msg=author:Кинг" \
    "http://localhost:50080/input"
)"

echo "$out" | grep -Eq '^(Found [0-9]+ books:|No books found for:)'

after="$(get_counter || true)"
after="${after:-0}"

# numeric compare
case "$before$after" in
  (*[!0-9]*)
    echo "FAIL: metrics counter not numeric (before='$before', after='$after')"
    exit 1
    ;;
esac

if [ "$after" -le "$before" ]; then
  echo "FAIL: orchestrator_requests_total did not increase (before=$before, after=$after)"
  exit 1
fi

echo "PASS: e2e chain ok (orchestrator_requests_total $before -> $after)"
