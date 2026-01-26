#!/bin/bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/projects/ebusta/grpc

# Чистим порт 50052 перед запуском
fuser -k 50052/tcp >/dev/null 2>&1 || true

echo "🚀 Запускаю EBusta DSL Server..."

sbcl --noinform \
     --eval '(push (truename "~/projects/ebusta/grpc/") asdf:*central-registry*)' \
     --eval '(push (truename "~/projects/ebusta/lisp-converter/") asdf:*central-registry*)' \
     --eval '(ql:quickload :cl-protobufs :silent t)' \
     --eval '(ql:quickload :grpc :silent t)' \
     --eval '(ql:quickload :ebusta-search :silent t)' \
     --load "$HOME/projects/ebusta/lisp-converter/dsl-service.lisp" \
     --eval "(loop (sleep 1))"
