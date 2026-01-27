#!/usr/bin/env bash
set -euo pipefail

echo "[A] grpc wrapper: ensure grpc/grpc.so exists"

if [[ -f grpc/grpc.so ]]; then
  echo "OK: grpc/grpc.so exists"
else
  echo "grpc/grpc.so missing -> building in ./grpc"
  (cd grpc && make)
fi

test -f grpc/grpc.so
test -s grpc/grpc.so
file grpc/grpc.so | grep -qi "shared object"

echo "grpc wrapper OK"
