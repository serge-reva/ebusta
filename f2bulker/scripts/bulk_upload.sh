#!/bin/bash

# Базовые пути
BASE_DIR=$(cd "$(dirname "$0")/.." && pwd)
CONFIG="$BASE_DIR/config.yaml"

# Функция извлечения значений из YAML
get_cfg_val() {
    local section=$1
    local key=$2
    sed -n "/^$section:/,/^[a-z]/p" "$CONFIG" | grep "$key" | awk -F': ' '{print $2}' | tr -d '"' | tr -d "'" | tr -d ' '
}

# Извлечение параметров
OS_URL=$(get_cfg_val "opensearch" "url")
INDEX_NAME=$(get_cfg_val "opensearch" "index_name")
OUT_DIR=$(get_cfg_val "paths" "output_dir")
UPL_LOG=$(get_cfg_val "uploading" "log_path")
SLEEP_TIME=$(get_cfg_val "uploading" "sleep_between_uploads")

# Дефолтные значения
UPL_LOG=${UPL_LOG:-"uploader.log"}
[[ "$UPL_LOG" == ./* ]] && UPL_LOG="$BASE_DIR/${UPL_LOG#./}"
[[ "$OUT_DIR" == ./* ]] && OUT_DIR="$BASE_DIR/${OUT_DIR#./}"
SLEEP_TIME=${SLEEP_TIME:-10}

log_event() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo "$msg" >> "$UPL_LOG"
    [ -t 1 ] && echo -e "$1"
}

log_event "🚀 Starting Bulk Upload to ${OS_URL}"

# Определение списка файлов
FILES=()
if [ -n "$1" ]; then
    [ -f "$1" ] && FILES=("$1") || { [ -f "$OUT_DIR/$1" ] && FILES=("$OUT_DIR/$1"); }
    [ ${#FILES[@]} -eq 0 ] && { log_event "❌ Error: File $1 not found."; exit 1; }
else
    shopt -s nullglob
    FILES=("$OUT_DIR"/*.jsonl)
fi

[ ${#FILES[@]} -eq 0 ] && { log_event "⚠️ No files found."; exit 0; }

for jsonl in "${FILES[@]}"; do
    base=$(basename "$jsonl")
    log_event "📤 Uploading: **$base**"
    
    # Выполнение запроса
    RESPONSE=$(curl -s -H "Content-Type: application/x-ndjson" -XPOST "${OS_URL}/_bulk" --data-binary "@$jsonl")
    
    # 1. Проверка на пустой ответ (сервер лежит)
    if [ -z "$RESPONSE" ]; then
        log_event "❌ **CRITICAL**: No response from server. Is OpenSearch running?"
        exit 1
    fi

    # 2. Проверка ошибок в JSON через jq
    ERRORS=$(echo "$RESPONSE" | jq '.errors')
    if [ "$ERRORS" == "true" ]; then
        log_event "❌ **Error** in $base: Bulk API reported issues."
        echo "$RESPONSE" | jq -c '.items[] | select(.index.error != null) | .index.error' >> "$UPL_LOG"
    else
        TOOK=$(echo "$RESPONSE" | jq '.took')
        COUNT=$(echo "$RESPONSE" | jq '.items | length // 0')
        log_event "✅ **Success**: $base ($COUNT docs) uploaded in ${TOOK}ms."
    fi

    [ ${#FILES[@]} -gt 1 ] && { log_event "💤 Cooling down $SLEEP_TIME sec..."; sleep "$SLEEP_TIME"; }
done
log_event "🏁 Process finished."
