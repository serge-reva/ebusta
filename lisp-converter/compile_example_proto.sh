#!/bin/bash
set -e

# Проверяем, установлен ли компилятор protoc в системе
if ! command -v protoc &> /dev/null; then
    echo "❌ Ошибка: 'protoc' не найден. Установите его: sudo apt install protobuf-compiler"
    exit 1
fi

echo "🚀 Генерирую Lisp-код из helloworld.proto..."

sbcl --noinform \
     --eval '(ql:quickload :cl-protobufs :silent t)' \
     --eval '(defvar *out* (merge-pathnames "projects/ebusta/lisp-converter/helloworld.lisp" (user-homedir-pathname)))' \
     --eval '(defvar *proto* (merge-pathnames "projects/grpc/examples/client/helloworld.proto" (user-homedir-pathname)))' \
     --eval '(handler-case 
                (cl-protobufs:protoc-lisp *proto* :output-file *out*)
              (error (c) (format t "ERROR: ~A~%" c) (sb-ext:exit :code 1)))' \
     --quit

if [ -f ~/projects/ebusta/lisp-converter/helloworld.lisp ]; then
    echo "✅ Файл helloworld.lisp успешно создан."
else
    echo "❌ Ошибка: Файл не создан."
    exit 1
fi
