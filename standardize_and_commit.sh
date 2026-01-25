#!/bin/bash
set -e

echo "🛠 1. Стандартизация имен параметров в JSON-шаблонах (q -> query)..."
# Меняем {{q}} на {{query}} во всех шаблонах, где он остался
sed -i 's/{{q}}/{{query}}/g' opensearch/templates/*.json

echo "🧪 2. Синхронизация тестов и бэкенда..."
# Убеждаемся, что в smoke_full.sh везде используется query
sed -i 's/"q":/"query":/g' tests/smoke_full.sh

echo "🚀 3. Обновление шаблонов в OpenSearch..."
./scripts/sync_templates.sh

echo "💾 4. Финальный Git Commit..."
git add .
git commit -m "final: standardized search parameters to 'query' and fixed backend inner_hits parsing"
git push

echo "🏁 Все системы синхронизированы, код зафиксирован в Git."
