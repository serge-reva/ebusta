#!/bin/bash
set -e

WORK_DIR="$HOME/projects/ebusta/lisp-converter"
PROTO_SRC="$HOME/projects/grpc/examples/client/helloworld.proto"

mkdir -p "$WORK_DIR"
cd "$WORK_DIR"

# 1. Копируем proto-файл, чтобы он лежал рядом
echo "📂 Копирую helloworld.proto..."
cp "$PROTO_SRC" .

# 2. Создаем ASDF-систему. Это "официальный" способ сборки.
# Мы говорим Lisp'у: "у нас есть proto файл, скомпилируй его сам".
cat << 'LISPEOF' > helloworld.asd
(defsystem "helloworld"
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on (:cl-protobufs :grpc)
  :components ((:protobuf-source-file "helloworld")))
LISPEOF

echo "✅ Система helloworld.asd создана."
