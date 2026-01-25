#!/bin/bash

# Параметры
DIR="$HOME/projects/ebusta/lisp-converter"
DSL_QUERY='(:and (:field "title" "Lisp") (:or (:field "author" "Serge") (:field "author" "Reva")))'
JSON_PAYLOAD=$(jq -n --arg q "$DSL_QUERY" '{raw_query: $q}')

echo "🚀 Перехожу в $DIR и отправляю запрос..."

cd "$DIR"

# Запускаем локально, указывая файл просто по имени.
# Это решает проблему с import path.
./grpcurl -plaintext \
    -proto search.proto \
    -d "$JSON_PAYLOAD" \
    localhost:50052 \
    ebusta.library.v1.MessageConverter/Convert
