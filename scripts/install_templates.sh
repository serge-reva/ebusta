#!/bin/bash
BASE_DIR=$(cd "$(dirname "$0")/.." && pwd)

HOST=192.168.1.179
PORT=9200
SCHEME=http
BASE="${SCHEME}://${HOST}:${PORT}"
TPL_DIR="${BASE_DIR}/opensearch/templates"

echo "🌐 Target: ${BASE}"
echo "📁 Source templates: ${TPL_DIR}"

if [ ! -d "${TPL_DIR}" ]; then
    echo "❌ Error: Directory ${TPL_DIR} not found!"
    exit 1
fi

# 1. Сначала собираем список имен шаблонов на основе файлов в папке
INSTALLED_NAMES=()
for f in "${TPL_DIR}"/*.json; do
  [[ "$(basename "$f")" == "flibusta_merged_index"* ]] && continue
  INSTALLED_NAMES+=("$(basename "$f" .json)")
done

# 2. Очистка старых скриптов перед установкой
echo "Cleaning templates..."
for name in "${INSTALLED_NAMES[@]}"; do
  curl -s -XDELETE "${BASE}/_scripts/${name}" >/dev/null || true
done

# 3. Установка
for f in "${TPL_DIR}"/*.json; do
  [[ "$(basename "$f")" == "flibusta_merged_index"* ]] && continue
  name="$(basename "$f" .json)"
  echo "Installing template: ${name}"
  
  curl -s -H "Content-Type: application/json" \
       -XPUT "${BASE}/_scripts/${name}" \
       --data-binary @"${f}" >/dev/null
done

# 4. Динамическая верификация
echo "--- Verification ---"
for name in "${INSTALLED_NAMES[@]}"; do
  echo -n "Checking ${name}: "
  RESULT=$(curl -s "${BASE}/_scripts/${name}")
  if echo "$RESULT" | grep -q '"script"'; then
      echo "✅ OK"
  else
      echo "❌ MISSING"
  fi
done
