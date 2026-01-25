#!/bin/bash
set -e

echo "🛠 1. Исправление маппинга параметров в cmd/datamanager/main.go..."
# Меняем default paramName с "q" на "query"
sed -i 's/paramName = "q"/paramName = "query"/g' cmd/datamanager/main.go

echo "🌐 2. Переключение smoke_full.sh на cloud-1..."
if [ -f tests/smoke_full.sh ]; then
    sed -i 's/192.168.1.179/cloud-1/g' tests/smoke_full.sh
fi

echo "🚀 3. Перезапуск системы..."
make stop
make run

echo "⏳ Ожидание готовности (5 сек)..."
sleep 5

echo "🧪 4. Финальный прогон тестов..."
make smoke

echo "🏁 Исправления применены. Проверь результаты выше."
