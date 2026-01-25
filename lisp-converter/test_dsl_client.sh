#!/bin/bash
# Используем grpcurl для независимой проверки

# DSL запрос: И (поле title=Lisp) ИЛИ (автор=Serge, автор=Reva)
DSL_QUERY='(:and (:field "title" "Lisp") (:or (:field "author" "Serge") (:field "author" "Reva")))'

# Экранируем кавычки для JSON
JSON_PAYLOAD=$(jq -n --arg q "$DSL_QUERY" '{raw_query: $q}')

echo "Sending DSL: $DSL_QUERY"

grpcurl -plaintext \
    -proto ~/projects/ebusta/lisp-converter/search.proto \
    -d "$JSON_PAYLOAD" \
    localhost:50052 \
    ebusta.library.v1.MessageConverter/Convert
