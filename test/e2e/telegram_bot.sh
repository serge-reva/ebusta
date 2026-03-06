#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPOSE_FILE="$ROOT_DIR/test/e2e/docker-compose.telegram-bot.yml"
COMPOSE_CMD=(docker compose -f "$COMPOSE_FILE")

cleanup() {
  "${COMPOSE_CMD[@]}" down >/dev/null 2>&1 || true
}
trap cleanup EXIT

cd "$ROOT_DIR"
make build-telegram-bot >/dev/null

echo "▶ Starting telegram-bot e2e stack"
"${COMPOSE_CMD[@]}" up -d --build

echo "▶ Waiting for bot health"
for _ in $(seq 1 30); do
  if curl -fsS http://localhost:8088/health >/dev/null 2>&1; then
    break
  fi
  sleep 1
done
curl -fsS http://localhost:8088/health >/dev/null

echo "▶ Sending /help webhook update"
curl -fsS -X POST \
  -H 'Content-Type: application/json' \
  -H 'X-Telegram-Bot-Api-Secret-Token: e2e-secret' \
  http://localhost:8088/webhook \
  -d '{"update_id":1,"message":{"message_id":10,"from":{"id":42,"is_bot":false,"first_name":"Test"},"chat":{"id":42,"type":"private"},"date":1700000000,"text":"/help"}}' >/dev/null
sleep 1
RESP="$(curl -fsS http://localhost:8088/_mock/messages)"
echo "$RESP" | grep -q 'Available commands:'

echo "▶ Sending /search webhook update"
curl -fsS -X POST \
  -H 'Content-Type: application/json' \
  -H 'X-Telegram-Bot-Api-Secret-Token: e2e-secret' \
  http://localhost:8088/webhook \
  -d '{"update_id":2,"message":{"message_id":11,"from":{"id":42,"is_bot":false,"first_name":"Test"},"chat":{"id":42,"type":"private"},"date":1700000001,"text":"/search tolstoy"}}' >/dev/null
sleep 1
RESP="$(curl -fsS http://localhost:8088/_mock/messages)"
echo "$RESP" | grep -q 'Mock result for tolstoy'

echo "✅ telegram_bot e2e passed"
