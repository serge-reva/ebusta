#!/bin/bash

# Настройки
OS_HOST="192.168.1.179:9200"
TEMPLATE_DIR="./opensearch/templates"

# Цвета для красоты
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "🚀 Updating OpenSearch templates on ${GREEN}$OS_HOST${NC}..."

# 1. Проверка доступности сервера
if ! curl -s --fail "http://$OS_HOST" > /dev/null; then
  echo -e "${RED}❌ Ошибка: OpenSearch недоступен по адресу $OS_HOST${NC}"
  echo "   Убедитесь, что IP верный и сервис запущен."
  exit 1
fi

# 2. Перебор файлов
for file in "$TEMPLATE_DIR"/*.json; do
  [ -e "$file" ] || continue
  
  # Получаем ID шаблона из имени файла (fl_author_exact.json -> fl_author_exact)
  filename=$(basename -- "$file")
  template_id="${filename%.*}"
  
  echo -n "👉 Uploading $template_id ... "
  
  # Загружаем шаблон
  # Используем --fail, чтобы curl возвращал ошибку при HTTP 400/500
  response=$(curl -s -X PUT "http://$OS_HOST/_scripts/$template_id" \
       -H "Content-Type: application/json" \
       -d @"$file")
       
  # Проверяем ответ
  if echo "$response" | grep -q '"acknowledged":true'; then
    echo -e "${GREEN}✅ OK${NC}"
  else
    echo -e "${RED}❌ FAIL${NC}"
    echo "   Response: $response"
  fi
done

echo "🏁 Done."
