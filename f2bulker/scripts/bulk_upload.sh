#!/bin/bash

# Базовые пути
BASE_DIR=$(cd "$(dirname "$0")/.." && pwd)
CONFIG="$BASE_DIR/config.yaml"

# Функция извлечения значений из YAML (улучшенная для вложенных структур)
get_cfg_val() {
    local section=$1
    local key=$2
    sed -n "/^$section:/,/^[a-z]/p" "$CONFIG" | grep "$key" | awk -F': ' '{print $2}' | tr -d '"' | tr -d "'" | tr -d ' '
}

# Извлечение параметров [cite: 256]
OS_URL=$(get_cfg_val "opensearch" "url")
INDEX_NAME=$(get_cfg_val "opensearch" "index_name")
OUT_DIR=$(get_cfg_val "paths" "output_dir")
UPL_LOG=$(get_cfg_val "uploading" "log_path")
SLEEP_TIME=$(get_cfg_val "uploading" "sleep_between_uploads")

# Дефолтные значения и пути
UPL_LOG=${UPL_LOG:-"uploader.log"}
[[ "$UPL_LOG" == ./* ]] && UPL_LOG="$BASE_DIR/${UPL_LOG#./}"
[[ "$OUT_DIR" == ./* ]] && OUT_DIR="$BASE_DIR/${OUT_DIR#./}"
SLEEP_TIME=${SLEEP_TIME:-10}

# Функция логирования (Экран + Файл) [cite: 240, 423]
log_event() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo "$msg" >> "$UPL_LOG"
    [ -t 1 ] && echo -e "$1"
}

log_event "🚀 Starting Bulk Upload to ${OS_URL} (Index: ${INDEX_NAME})"

# Определение списка файлов для обработки
FILES=()
if [ -n "$1" ]; then
    # Если передан параметр — проверяем его существование
    if [ -f "$1" ]; then
        FILES=("$1")
    elif [ -f "$OUT_DIR/$1" ]; then
        FILES=("$OUT_DIR/$1")
    else
        log_event "❌ Error: File $1 not found."
        exit 1
    fi
else
    # Иначе берем все .jsonl из директории [cite: 257]
    shopt -s nullglob
    FILES=("$OUT_DIR"/*.jsonl)
fi

if [ ${#FILES[@]} -eq 0 ]; then
    log_event "⚠️ No files found for processing. Exiting."
    exit 0
fi

log_event "📊 Total files to process: ${#FILES[@]}"

for jsonl in "${FILES[@]}"; do
    base=$(basename "$jsonl")
    
    log_event "📤 Uploading: **$base**"
    
    # Прямая отправка в Bulk API без авторизации [cite: 452, 457]
    RESPONSE=$(curl -s -H "Content-Type: application/x-ndjson" \
         -XPOST "${OS_URL}/_bulk" \
         --data-binary "@$jsonl")
    
    # Проверка на ошибки через jq [cite: 458, 471]
    if echo "$RESPONSE" | jq -e '.errors == true' > /dev/null; then
        log_event "❌ **Error** in $base: Bulk API reported issues."
        # Детали ошибок пишем только в файл, чтобы не спамить экран
        echo "$RESPONSE" | jq -c '.items[] | select(.index.error != null) | .index.error' >> "$UPL_LOG"
    else
        TOOK=$(echo "$RESPONSE" | jq '.took')
        COUNT=$(echo "$RESPONSE" | jq '.items | length')
        log_event "✅ **Success**: $base ($COUNT docs) uploaded in ${TOOK}ms."
    fi

    # Пауза "остывания", если файлов больше одного [cite: 260]
    if [ ${#FILES[@]} -gt 1 ]; then
        log_event "💤 Cooling down for $SLEEP_TIME sec..."
        sleep "$SLEEP_TIME"
    fi
done

log_event "🏁 Process finished."
