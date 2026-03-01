#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

go run ./test/utils/grpc_check.go \
  --target orchestrator \
  --addr localhost:50054 \
  --query "author:Кинг" \
  --limit 1 \
  --timeout 10s
