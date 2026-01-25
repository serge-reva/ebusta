#!/bin/bash
# Путь к библиотеке внутри проекта
GRPC_PATH="$HOME/projects/ebusta/grpc"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GRPC_PATH

echo "🚀 Запускаю DSL Client..."

sbcl --noinform \
     --eval "(push (truename \"$GRPC_PATH/\") asdf:*central-registry*)" \
     --eval '(push (truename "~/projects/ebusta/lisp-converter/") asdf:*central-registry*)' \
     --eval '(ql:quickload :cl-protobufs :silent t)' \
     --eval '(ql:quickload :grpc :silent t)' \
     --eval '(ql:quickload :ebusta-search :silent t)' \
     --load "$HOME/projects/ebusta/lisp-converter/dsl-client.lisp"
