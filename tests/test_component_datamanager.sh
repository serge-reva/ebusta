#!/usr/bin/env bash
set -euo pipefail

go run ./tests/grpc_check/main.go \
  --target datamanager \
  --addr localhost:50051 \
  --query "Кинг" \
  --limit 1 \
  --timeout 8s
