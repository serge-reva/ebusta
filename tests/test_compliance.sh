#!/bin/bash
# Compliance Test Suite (UR 1 - UR 4)

GRPCURL="./lisp-converter/grpcurl"
PROTO="./lisp-converter/search.proto"
ADDR="localhost:50052"
SVC="ebusta.library.v1.MessageConverter/Convert"

check_result() {
    local name=$1
    local query=$2
    local pattern=$3
    echo -n "[TEST] $name... "
    res=$($GRPCURL -plaintext -import-path ./lisp-converter -proto $PROTO -d "{\"raw_query\": \"$query\"}" $ADDR $SVC 2>/dev/null)
    if echo "$res" | grep -q "$pattern"; then
        echo "✅ PASSED"
    else
        echo "❌ FAILED"
        echo "Query: $query"
        echo "Expected: $pattern"
        echo "Got: $res"
        exit 1
    fi
}

echo "=== Running Compliance Tests from ./tests ==="

# UR 1.1
check_result "UR 1.1 (Numeric ID)" "101" "\"field\": \"id\""
check_result "UR 1.1 (Default any)" "linux" "\"field\": \"any\""

# UR 2.1 (Quoted values)
check_result "UR 2.1 (Quoted Author)" "author:\"Стивен Кинг\"" "\"value\": \"Стивен Кинг\""

# UR 2.2 (Regex)
check_result "UR 2.2 (Regex Operator)" "title:/^Unix.*/" "\"operator\": 6"

# UR 3.1 & 3.3 (Logic & Precedence)
check_result "UR 3.1 (Complex Logic)" "author:\"Стивен Кинг\" AND title:Куджо" "\"op\": 1"

echo "=== All tests passed ==="
