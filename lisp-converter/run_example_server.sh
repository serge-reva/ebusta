#!/bin/bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/projects/grpc

echo "🚀 Запускаю Example Server (через Load File)..."

sbcl --noinform \
     --eval '(push (truename "~/projects/grpc/") asdf:*central-registry*)' \
     --eval '(push (truename "~/projects/ebusta/lisp-converter/") asdf:*central-registry*)' \
     --eval '(ql:quickload :cl-protobufs :silent t)' \
     --eval '(ql:quickload :grpc :silent t)' \
     --eval '(ql:quickload :helloworld :silent t)' \
     --load "$HOME/projects/ebusta/lisp-converter/example-server.lisp"
