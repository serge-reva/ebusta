#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

GATEWAY_URL="${GATEWAY_URL:-http://localhost:8443}"

resp_file="$(mktemp)"
trap 'rm -f "$resp_file"' EXIT

code="$(curl -sS -o "$resp_file" -w '%{http_code}' \
  -X POST "$GATEWAY_URL/search" \
  -H 'Content-Type: application/json' \
  -d '{"query":"author:king","page":1,"limit":5}')"

if [[ "$code" != "200" ]]; then
  echo "FAIL: expected HTTP 200 from gateway /search, got $code"
  cat "$resp_file"
  exit 1
fi

if command -v jq >/dev/null 2>&1; then
  jq -e '.trace_id and (.total|type=="number") and (.books|type=="array")' "$resp_file" >/dev/null || {
    echo "FAIL: invalid gateway response structure"
    cat "$resp_file"
    exit 1
  }
else
  grep -q '"trace_id"' "$resp_file" || { echo "FAIL: trace_id missing"; cat "$resp_file"; exit 1; }
  grep -q '"total"' "$resp_file" || { echo "FAIL: total missing"; cat "$resp_file"; exit 1; }
  grep -q '"books"' "$resp_file" || { echo "FAIL: books missing"; cat "$resp_file"; exit 1; }
fi

echo "PASS: gateway search returned valid response (datamanager path via orchestrator)"
