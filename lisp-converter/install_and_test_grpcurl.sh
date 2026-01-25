#!/bin/bash
set -e

# 1. Скачиваем grpcurl (последняя версия)
echo "⬇️ Скачиваю grpcurl..."
cd /tmp
wget -q https://github.com/fullstorydev/grpcurl/releases/download/v1.8.9/grpcurl_1.8.9_linux_x86_64.tar.gz
tar -xvf grpcurl_1.8.9_linux_x86_64.tar.gz
mv grpcurl ~/projects/ebusta/lisp-converter/
chmod +x ~/projects/ebusta/lisp-converter/grpcurl

echo "✅ grpcurl установлен в ~/projects/ebusta/lisp-converter/grpcurl"

# 2. Формируем запрос
DSL_QUERY='(:and (:field "title" "Lisp") (:or (:field "author" "Serge") (:field "author" "Reva")))'

# Экранируем для JSON
JSON_PAYLOAD=$(jq -n --arg q "$DSL_QUERY" '{raw_query: $q}')

echo "🚀 Отправляю запрос через grpcurl..."
echo "Payload: $JSON_PAYLOAD"

~/projects/ebusta/lisp-converter/grpcurl -plaintext \
    -proto ~/projects/ebusta/lisp-converter/search.proto \
    -d "$JSON_PAYLOAD" \
    localhost:50052 \
    ebusta.library.v1.MessageConverter/Convert
