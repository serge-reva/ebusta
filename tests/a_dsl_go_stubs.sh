#!/usr/bin/env bash
set -euo pipefail

echo "[A] DSL Go stubs: generate api/gen/dsl from lisp-converter/search.proto"
mkdir -p api/gen/dsl

protoc \
  --proto_path=lisp-converter \
  --go_out=. --go_opt=module=ebusta \
  --go-grpc_out=. --go-grpc_opt=module=ebusta \
  lisp-converter/search.proto

test -f api/gen/dsl/search.pb.go
ls -la api/gen/dsl/*.pb.go >/dev/null
echo "DSL Go stubs OK"
