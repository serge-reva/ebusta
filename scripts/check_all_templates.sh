#!/bin/bash

OS_HOST="192.168.1.149:9200"

# Список всех твоих шаблонов
TEMPLATES=(
    "fl_mixed_search"
    "fl_authors_all"
    "fl_titles_all"
    "fl_author_exact"
    "fl_title_prefix"
    "fl_title_substring"
    "fl_names_token_prefix"
)

echo "🔎 Inspecting ALL templates on $OS_HOST..."

for tpl in "${TEMPLATES[@]}"; do
    echo "==================================================="
    echo "📂 TEMPLATE ID: $tpl"
    
    # Скачиваем тело шаблона
    CONTENT=$(curl -s "http://$OS_HOST/_scripts/$tpl")
    
    # Выводим сырой JSON (форматируем через python для читаемости, если jq нет)
    if command -v jq >/dev/null 2>&1; then
        echo "$CONTENT" | jq .
    else
        # Fallback если нет jq
        echo "$CONTENT"
    fi

    # ПРОВЕРКА НА ОШИБКУ
    # Ищем переменную {{q}}
    if echo "$CONTENT" | grep -q "{{q}}"; then
        echo -e "\n✅ STATUS: OK (Использует {{q}})"
    else
        echo -e "\n❌ STATUS: WARNING (Не вижу {{q}}! Возможно старая версия?)"
    fi
    echo ""
done
