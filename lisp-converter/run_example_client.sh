#!/bin/bash
echo "🚀 Запускаю Client..."

sbcl --noinform --non-interactive \
     --eval '(push (truename "~/projects/ebusta/lisp-converter/") asdf:*central-registry*)' \
     --eval '(ql:quickload :helloworld :silent t)' \
     --load "$HOME/projects/ebusta/lisp-converter/example-client.lisp"
