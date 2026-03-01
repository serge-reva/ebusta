#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

GATEWAY_URL="http://localhost:8443/search"
QUERY_PAYLOAD='{"query":"author:king","limit":1,"page":1}'

SERVICES=(
  datamanager
  orchestrator
  dsl-scala
  query-builder
  downloader
)

cleanup() {
  make docker-down >/dev/null 2>&1 || true
}
trap cleanup EXIT

log() {
  printf '[resilience] %s\n' "$*"
}

wait_for_gateway() {
  local timeout="${1:-180}"
  local deadline=$((SECONDS + timeout))
  local body
  body="$(mktemp)"

  while (( SECONDS < deadline )); do
    local code
    code="$(curl -sS -o "$body" -w '%{http_code}' -H 'Content-Type: application/json' -d "$QUERY_PAYLOAD" "$GATEWAY_URL" || true)"
    if [[ "$code" == "200" ]]; then
      rm -f "$body"
      return 0
    fi
    sleep 2
  done

  log "gateway did not become ready in ${timeout}s"
  cat "$body" || true
  rm -f "$body"
  return 1
}

wait_service_healthy() {
  local service="$1"
  local timeout="${2:-180}"
  local deadline=$((SECONDS + timeout))

  while (( SECONDS < deadline )); do
    local line
    line="$(docker compose ps "$service" 2>/dev/null | awk 'NR==3{print $0}')"
    if [[ -n "$line" ]] && grep -qi 'healthy' <<<"$line"; then
      return 0
    fi
    sleep 2
  done

  log "service ${service} did not become healthy in ${timeout}s"
  docker compose ps "$service" || true
  return 1
}

run_search() {
  local body_file="$1"
  curl -sS -o "$body_file" -w '%{http_code}' \
    -H 'Content-Type: application/json' \
    -d "$QUERY_PAYLOAD" \
    "$GATEWAY_URL" || true
}

assert_search_ok() {
  local body
  body="$(mktemp)"
  local code
  code="$(run_search "$body")"

  if [[ "$code" != "200" ]]; then
    log "expected HTTP 200, got HTTP ${code}"
    cat "$body" || true
    rm -f "$body"
    return 1
  fi

  rm -f "$body"
}

assert_search_failed() {
  local body
  body="$(mktemp)"
  local code
  code="$(run_search "$body")"

  if [[ "$code" == "500" || "$code" == "503" ]]; then
    rm -f "$body"
    return 0
  fi

  log "expected HTTP 500/503, got HTTP ${code}"
  cat "$body" || true
  rm -f "$body"
  return 1
}

run_failure_cycle() {
  local service="$1"

  log "stopping ${service}"
  docker compose stop "$service" >/dev/null

  local fail_seen=0
  for _ in {1..10}; do
    if assert_search_failed; then
      fail_seen=1
      break
    fi
    sleep 2
  done

  if [[ "$fail_seen" -ne 1 ]]; then
    log "search did not fail after stopping ${service}"
    return 1
  fi

  log "starting ${service}"
  docker compose start "$service" >/dev/null
  wait_service_healthy "$service" 180
  wait_for_gateway 180
  assert_search_ok
  log "recovery verified for ${service}"
}

log "starting stack"
make docker-up

log "waiting for gateway baseline"
wait_for_gateway 240
assert_search_ok

for svc in "${SERVICES[@]}"; do
  run_failure_cycle "$svc"
done

log "all resilience checks passed"
