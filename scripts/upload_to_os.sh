#!/bin/bash
BASE_DIR=$(cd "$(dirname "$0")/.." && pwd)
CONFIG="$BASE_DIR/config.yaml"

# Настройки подключения
OS_HOST="192.168.1.179"
OS_PORT="9200"
INDEX_NAME=$(grep "index_name" "$CONFIG" | awk -F': ' '{print $2}' | tr -d '"' | tr -d "'" | tr -d ' ')

# Прямой путь к данным балкера
OUT_DIR="$BASE_DIR/f2bulker/data/out"

if [ ! -d "$OUT_DIR" ]; then
    echo "❌ Error: Directory $OUT_DIR not found!"
    exit 1
fi

echo "📤 Found data in: $OUT_DIR"
echo "📤 Uploading to http://$OS_HOST:$OS_PORT [$INDEX_NAME]"

shopt -s nullglob
for jsonl in "$OUT_DIR"/*.jsonl; do
    # Пропускаем тестовые файлы
    [[ "$jsonl" == *"test"* ]] && continue
    [[ "$jsonl" == *"smoke"* ]] && continue

    echo -n "--- $(basename "$jsonl"): "
    
    # Отправка через Bulk API
    curl -s -H "Content-Type: application/x-ndjson" \
         -XPOST "http://$OS_HOST:$OS_PORT/_bulk" \
         --data-binary "@$jsonl" | jq -c '{took, errors, count: (.items | length)}'
done

echo -n "🏁 Final document count in OS: "
curl -s "http://$OS_HOST:$OS_PORT/$INDEX_NAME/_count" | jq '.count'
