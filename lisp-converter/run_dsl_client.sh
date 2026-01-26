#!/bin/bash
# Скрипт запуска тестового Lisp-клиента

PROJ_DIR="$HOME/projects/ebusta"

sbcl --noinform --non-interactive \
     --eval '(push (truename "~/projects/ebusta/grpc/") asdf:*central-registry*)' \
     --eval '(push (truename "~/projects/ebusta/lisp-converter/") asdf:*central-registry*)' \
     --eval "(ql:quickload '(:cl-protobufs :grpc :uiop :cl-json) :silent t)" \
     --eval "(ql:quickload :ebusta-search :silent t)" \
     --load "$PROJ_DIR/lisp-converter/dsl-client.lisp" \
     --eval "(ebusta-service::run)"
