#!/bin/bash
echo "=== 1. Список пакетов cl-protobufs ==="
sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
     --eval '(ql:quickload :cl-protobufs :silent t)' \
     --eval '(dolist (p (list-all-packages))
              (let ((name (package-name p)))
                (when (search "PROTOBUF" name)
                  (format t "Package: ~A | Nicknames: ~A~%" name (package-nicknames p)))))' \
     --quit

echo ""
echo "=== 2. Заголовок search.lisp (первые 20 строк) ==="
head -n 20 ~/projects/ebusta/lisp-converter/search.lisp
