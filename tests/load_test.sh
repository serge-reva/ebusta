#!/bin/bash
# Запуск нагрузки: 200 запросов в секунду в течение 30 секунд
./lisp-converter/ghz \
    --insecure \
    --proto ./lisp-converter/search.proto \
    --call ebusta.library.v1.MessageConverter/Convert \
    --data '{"raw_query": "(title:\"Lisp Machine\" OR author:Graham) AND (year:2026 OR tags:/sre/)"}' \
    --rps 200 \
    --duration 30s \
    localhost:50052
