#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

echo "[1/3] Running DSL parser self-tests"
(
  cd dsl-scala
  sbt "runMain ebusta.dsl.ParserSelfTest"
)

echo "[2/3] Building dsl-scala and query-builder jars"
make build-scala

echo "[3/3] Live smoke via gateway"
curl -fsS "http://localhost:8443/health" >/dev/null 2>&1 || {
  echo "gateway is not available on :8443"
  exit 1
}

RESP=$(curl -sS -X POST http://localhost:8443/search \
  -H 'Content-Type: application/json' \
  -d '{"query":"stephen king","page":1,"limit":5}')

echo "$RESP" | jq . >/dev/null 2>&1 || {
  echo "gateway response is not valid json: $RESP"
  exit 1
}

if echo "$RESP" | jq -e '.error.message | test("expected but")' >/dev/null 2>&1; then
  echo "multi-word parse still failing: $RESP"
  exit 1
fi

echo "$RESP" | jq -e '.trace_id and (.books|type=="array")' >/dev/null 2>&1 || {
  echo "gateway response missing required fields: $RESP"
  exit 1
}

echo "gateway smoke response: $RESP"

echo "test_dsl_multiword.sh PASSED"
