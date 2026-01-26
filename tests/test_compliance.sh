#!/bin/bash
# EBusta Compliance Test Suite (SRE Edition)

GRPCURL="./lisp-converter/grpcurl"
PROTO="lisp-converter/search.proto"

check_query() {
    local query=$1
    local expected_pattern=$2
    local label=$3

    echo -n "[TEST] $label... "
    
    # Прямой вызов grpcurl, убираем пробелы из вывода для надежного сравнения
    RESULT=$($GRPCURL -plaintext -import-path . -proto $PROTO \
        -d "{\"raw_query\": \"$query\"}" \
        localhost:50052 ebusta.library.v1.MessageConverter/Convert 2>/dev/null | tr -d '[:space:]')

    if [[ "$RESULT" == *"$expected_pattern"* ]]; then
        echo "✅ PASSED"
    else
        echo "❌ FAILED"
        echo "Query: $query"
        echo "Expected Pattern: $expected_pattern"
        echo "Actual (Minified): $RESULT"
        exit 1
    fi
}

echo "=== Running Compliance Tests ==="
# Короткая пауза на прогрев
sleep 2

check_query "101" '"field":"id"' "UR 1.1 (Numeric ID)"
check_query "linux" '"field":"any"' "UR 1.1 (Default any)"
check_query "author:\"Стивен Кинг\"" '"value":"СтивенКинг"' "UR 2.1 (Quoted Author)"
