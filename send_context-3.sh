#!/bin/bash

# Настройки
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
OUTPUT_FILE="ebusta_context_${TIMESTAMP}.txt"
REMOTE_DEST="reva@mars:/home/reva/to_chat"

echo "📂 Собираю контекст проекта в $OUTPUT_FILE..."

# Очищаем файл перед началом
echo "=== EBusta Project Context: $(date) ===" > "$OUTPUT_FILE"

# 2. Собираем исходники
find . -type f \
    \( -name "*.go" -o -name "*.yaml" -o -name "*.proto" -o -name "*.json" -o -name "*.md" -o -name "Makefile" -o -name "go.mod" \) \
    -not -path "./bin/*" \
    -not -path "*/data/*" \
    -not -path "./.git/*" \
    -not -path "*/uploader.log" \
    -not -path "*.log" | while read -r file; do
        echo "" >> "$OUTPUT_FILE"
        echo "--- START_FILE: $file ---" >> "$OUTPUT_FILE"
        cat "$file" >> "$OUTPUT_FILE"
        echo "" >> "$OUTPUT_FILE"
        echo "--- END_FILE: $file ---" >> "$OUTPUT_FILE"
done

echo "✅ Сборка завершена. Размер: $(du -h "$OUTPUT_FILE" | awk '{print $1}')"

echo "🚀 Отправляю на mars..."
scp "$OUTPUT_FILE" "$REMOTE_DEST"

if [ $? -eq 0 ]; then
    echo "🎉 Файл на mars:/home/reva/to_chat/$OUTPUT_FILE. Загружай его мне!"
else
    echo "❌ Ошибка при отправке через scp"
fi
