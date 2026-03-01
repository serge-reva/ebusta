#!/usr/bin/env bash
set -euo pipefail

CLI="./bin/ebusta-cli"
test -x "$CLI" || { echo "FAIL: $CLI not found/executable (run make build-cli or make build)"; exit 1; }

run_case() {
  local title="$1"
  local q="$2"

  local out
  out="$($CLI "$q" 2>&1 || true)"

  if echo "$out" | grep -Eq '^Error:'; then
    echo "FAIL: $title (search failed)"
    echo "$out"
    exit 1
  fi

  if ! echo "$out" | grep -Eq '(No results found\.|Found [0-9]+ books:|ID[[:space:]]*\|[[:space:]]*Title[[:space:]]*\|[[:space:]]*Authors)'; then
    echo "FAIL: $title (unexpected cli output)"
    echo "$out"
    exit 1
  fi

  echo "PASS: $title"
}

echo "=== Functional AST acceptance via CLI argv ==="

run_case "1) numeric id" "101"
run_case "2) default any" "linux"
run_case "3) quoted author" 'author:"Стивен Кинг"'
run_case "4) greedy author AND year" "author:Стивен Кинг AND year:2026"
run_case "5) title OR title" "title:Туман OR title:Оно"
run_case "6) NOT author" "NOT author:Кинг"
run_case "7) (author AND title) OR id" "author:Кинг AND title:Туман OR id:101"
run_case "8) trailing paren treated as literal" "author:Кинг)"

echo "ALL PASS: cli functional AST"
