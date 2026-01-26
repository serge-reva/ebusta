#!/bin/bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/projects/ebusta/grpc

# Очищаем порт
fuser -k 50052/tcp >/dev/null 2>&1 || true

echo "🚀 Запускаю EBusta DSL Server (Fix Runtime Options)..."

# ПРАВИЛО: Рантайм-опции (память) ДО опций выполнения (eval)
sbcl --dynamic-space-size 1024 --noinform --non-interactive \
     --eval "(push (truename \"~/projects/ebusta/grpc/\") asdf:*central-registry*)" \
     --eval "(push (truename \"~/projects/ebusta/lisp-converter/\") asdf:*central-registry*)" \
     --eval "(ql:quickload '(:cl-ppcre :ebusta-search) :silent t)" \
     --load "$HOME/projects/ebusta/lisp-converter/dsl-service.lisp" \
     --eval "(ebusta-service:start :port 50052)"
