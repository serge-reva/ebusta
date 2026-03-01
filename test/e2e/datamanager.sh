#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

go run ./test/utils/grpc_check.go \
  --target datamanager \
  --addr localhost:50051 \
  --query "Кинг" \
  --limit 1 \
  --timeout 8s
