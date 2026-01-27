#!/usr/bin/env bash
set -euo pipefail

echo "[A] Chain neighbors: start-all -> DSL -> orchestrator -> web"
make stop-all || true
./tests/a_grpc_wrapper.sh
./tests/a_dsl_go_stubs.sh
make build-all
make start-all

# Give services a moment
sleep 1

./tests/a_dsl_smoke.sh
./tests/a_orchestrator_smoke.sh
./tests/a_web_adapter_smoke.sh

# E2E HTTP call (web -> orchestrator)
curl -fsS "http://localhost:50080/input?msg=author:%D1%82%D0%BE%D0%BB%D1%81%D1%82%D0%BE%D0%B9" >/tmp/ebusta_e2e_http.out
test -s /tmp/ebusta_e2e_http.out

echo "Chain neighbors OK"

exit 0
