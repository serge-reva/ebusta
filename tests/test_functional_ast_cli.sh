#!/usr/bin/env bash
set -euo pipefail

LOG="orch.log"
CLI="./bin/ebusta-cli"

test -f "$LOG" || { echo "FAIL: $LOG not found (stack not started?)"; exit 1; }
test -x "$CLI" || { echo "FAIL: $CLI not found/executable (run make build-cli or make test-stack)"; exit 1; }

send_query() {
  local q="$1"
  local out
  out="$("$CLI" "$q" 2>&1 || true)"

  # Any ❌ from CLI is treated as failure (dial, rpc, etc.)
  if grep -q '❌' <<<"$out"; then
    echo "FAIL: cli reported error"
    echo "$out"
    exit 1
  fi
}

wait_patterns_in_new_log() {
  local start_line="$1"; shift
  local title="$1"; shift
  local max_iter=50

  for _ in $(seq 1 "$max_iter"); do
    local chunk
    chunk="$(tail -n +"$((start_line+1))" "$LOG" 2>/dev/null || true)"

    local ok=1
    for pat in "$@"; do
      echo "$chunk" | grep -Eqi "$pat" || { ok=0; break; }
    done

    if [ "$ok" -eq 1 ]; then
      echo "PASS: $title"
      return 0
    fi
    sleep 0.1
  done

  echo "FAIL: $title"
  echo "--- last new log chunk ---"
  tail -n 120 "$LOG" || true
  return 1
}

run_case_ok() {
  local title="$1"
  local q="$2"
  shift 2
  local start_line
  start_line="$(wc -l < "$LOG" 2>/dev/null || echo 0)"

  send_query "$q"

  wait_patterns_in_new_log "$start_line" "$title" \
    'DSL CanonicalForm:' \
    "$@"
}

echo "=== Functional AST tests via CLI argv ==="

run_case_ok  "1) numeric id" \
  '101' \
  ':FIELD[[:space:]]+"id"[[:space:]]+"101"'

run_case_ok  "2) default any" \
  'linux' \
  ':FIELD[[:space:]]+"any"[[:space:]]+"linux"'

run_case_ok  "3) quoted author" \
  'author:"Стивен Кинг"' \
  ':FIELD[[:space:]]+"author"[[:space:]]+"Стивен[[:space:]]+Кинг"'

run_case_ok  "4) greedy author AND year" \
  'author:Стивен Кинг AND year:2026' \
  ':AND' \
  ':FIELD[[:space:]]+"author"[[:space:]]+"Стивен[[:space:]]+Кинг"' \
  ':FIELD[[:space:]]+"year"[[:space:]]+"2026"'

run_case_ok  "5) title OR title" \
  'title:Туман OR title:Оно' \
  ':OR' \
  ':FIELD[[:space:]]+"title"[[:space:]]+"Туман"' \
  ':FIELD[[:space:]]+"title"[[:space:]]+"Оно"'

run_case_ok  "6) NOT author" \
  'NOT author:Кинг' \
  ':NOT' \
  ':FIELD[[:space:]]+"author"[[:space:]]+"Кинг"'

run_case_ok  "7) (author AND title) OR id" \
  'author:Кинг AND title:Туман OR id:101' \
  ':OR' \
  ':AND' \
  ':FIELD[[:space:]]+"author"[[:space:]]+"Кинг"' \
  ':FIELD[[:space:]]+"title"[[:space:]]+"Туман"' \
  ':FIELD[[:space:]]+"id"[[:space:]]+"101"'

run_case_ok  "8) trailing paren treated as literal" \
  'author:Кинг)' \
  ':FIELD[[:space:]]+"author"[[:space:]]+"Кинг\)"'

echo "ALL PASS: cli functional AST"
