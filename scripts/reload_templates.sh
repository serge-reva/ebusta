#!/bin/bash

# ЖЕСТКАЯ ПРИВЯЗКА К IP
OS_HOST="192.168.1.179:9200"
TEMPLATE_DIR="./opensearch/templates"

echo "🚀 Updating OpenSearch templates on $OS_HOST..."

# Проверка доступности OpenSearch перед началом
if ! curl -s "http://$OS_HOST" > /dev/null; then
    echo "❌ Ошибка: Не могу подключиться к OpenSearch на $OS_HOST"
    exit 1
fi

for file in $TEMPLATE_DIR/*.json; do
  [ -e "$file" ] || continue
  
  # Имя файла без расширения = ID шаблона
  filename=$(basename -- "$file")
  template_id="${filename%.*}"
  
  echo -n "👉 Uploading $template_id ... "
  
  # Загружаем шаблон
  response=$(curl -s -X PUT "http://$OS_HOST/_scripts/$template_id" \
       -H "Content-Type: application/json" \
       -d @"$file")
       
  if echo "$response" | grep -q '"acknowledged":true'; then
    echo "✅ OK"
  else
    echo "❌ FAIL"
    echo "   Response: $response"
  fi
done
