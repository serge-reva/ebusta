#!/bin/bash
HOST="cloud-1"
PORT=9200

echo "🔍 Checking port $PORT on $HOST..."
# Используем код выхода nc вместо парсинга текста
if nc -zv -w 5 "$HOST" "$PORT" > /dev/null 2>&1; then
    echo "✅ Connection SUCCESSFUL! Port is open."
    curl -s -I "http://$HOST:$PORT" | head -n 1
else
    echo "❌ Connection FAILED."
fi
