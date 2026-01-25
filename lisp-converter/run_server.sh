#!/bin/bash
# Говорим системе, где искать нашу свежесобранную библиотеку grpc.so
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/projects/grpc

echo "🚀 Запускаю Lisp gRPC сервер..."
sbcl --noinform \
     --load "$HOME/quicklisp/setup.lisp" \
     --load "$HOME/projects/ebusta/lisp-converter/server.lisp"
