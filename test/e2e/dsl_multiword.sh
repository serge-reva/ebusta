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

echo "[3/3] Optional live smoke via gateway (if running)"
if curl -fsS "http://localhost:8443/health" >/dev/null 2>&1; then
  RESP=$(curl -sS -X POST http://localhost:8443/search \
    -H 'Content-Type: application/json' \
    -d '{"query":"стивен кинг","page":1,"limit":5}')

  echo "$RESP" | jq . >/dev/null 2>&1 || {
    echo "gateway response is not valid json: $RESP"
    exit 1
  }

  if echo "$RESP" | jq -e '.error.message | test("expected but")' >/dev/null 2>&1; then
    echo "multi-word parse still failing: $RESP"
    exit 1
  fi

  echo "gateway smoke response: $RESP"
else
  echo "gateway is not running; skipped live smoke"
fi

echo "test_dsl_multiword.sh PASSED"
