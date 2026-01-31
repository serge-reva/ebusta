#!/usr/bin/env bash
set -euo pipefail

go run ./tests/grpc_check/main.go \
  --target orchestrator \
  --addr localhost:50053 \
  --query "author:Кинг" \
  --limit 1 \
  --timeout 10s
