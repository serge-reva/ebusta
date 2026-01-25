#!/bin/bash

# Читаем наш новый конфиг
CONFIG="os-setup-config.yaml"

# Улучшенная функция парсинга YAML для текущей папки
get_cfg_val() {
    local section=$1
    local key=$2
    sed -n "/^$section:/,/^[a-z]/p" "$CONFIG" | grep "$key" | awk -F': ' '{print $2}' | tr -d '"' | tr -d "'" | tr -d ' '
}

OS_URL=$(get_cfg_val "opensearch" "url")
INDEX_NAME=$(get_cfg_val "opensearch" "index_name")
INDEX_FILE=$(get_cfg_val "paths" "index_file")
T_DIR=$(get_cfg_val "paths" "templates_dir")
LOG_PATH=$(get_cfg_val "logging" "log_path")

log_msg() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo "$msg" >> "$LOG_PATH"
    [ -t 1 ] && echo -e "$1"
}

log_msg "🚀 Starting OpenSearch initialization on **$OS_URL**"

# 1. Проверка доступности сервера
if ! curl -s --head "$OS_URL" > /dev/null; then
    log_msg "❌ Error: Cannot reach $OS_URL. Check connection to cloud-1."
    exit 1
fi

# 2. Создание индекса
log_msg "🔨 Creating index: **$INDEX_NAME**"
if [ ! -f "$INDEX_FILE" ]; then
    log_msg "❌ Error: Config file $INDEX_FILE not found."
    exit 1
fi

# Удаляем индекс, если он уже есть (для чистой переустановки на VDS)
curl -s -X DELETE "$OS_URL/$INDEX_NAME" > /dev/null
RESPONSE=$(curl -s -X PUT "$OS_URL/$INDEX_NAME" -H 'Content-Type: application/json' --data-binary "@$INDEX_FILE")

if echo "$RESPONSE" | grep -q '"acknowledged":true'; then
    log_msg "   ✅ Index created successfully."
else
    log_msg "   ❌ Failed to create index: $RESPONSE"
    exit 1
fi

# 3. Загрузка поисковых шаблонов
log_msg "🧩 Uploading templates from $T_DIR..."
shopt -s nullglob
FILES=("$T_DIR"/*.json)

for f in "${FILES[@]}"; do
    t_name=$(basename "$f" .json)
    log_msg "📝 Sending template: **$t_name**"
    
    T_RESPONSE=$(curl -s -X POST "$OS_URL/_scripts/$t_name" \
         -H 'Content-Type: application/json' \
         --data-binary "@$f")
    
    if echo "$T_RESPONSE" | grep -q '"acknowledged":true'; then
        log_msg "   ✅ Success."
    else
        log_msg "   ❌ Error in $t_name: $T_RESPONSE"
    fi
done

log_msg "🏁 Setup complete. Logs saved to $LOG_PATH"
