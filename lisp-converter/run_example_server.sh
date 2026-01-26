#!/bin/bash
# Чистим порт перед запуском
fuser -k 50051/tcp 2>/dev/null || true

echo "🚀 Запускаю Example Server..."

sbcl --noinform \
     --eval '(push (truename "~/projects/ebusta/lisp-converter/") asdf:*central-registry*)' \
     --eval '(ql:quickload :helloworld :silent t)' \
     --load "$HOME/projects/ebusta/lisp-converter/example-server.lisp" \
     --eval "(loop (sleep 1))"
