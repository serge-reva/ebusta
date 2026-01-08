#!/bin/bash

if [ -z "$1" ]; then
    echo "❌ Ошибка: Укажите имя файла."
    exit 1
fi

RESULT_FILE="$1"
TARGET="reva@mars:/home/reva/to_chat"

echo "📦 Сборка исходников (f2bulker/data и мусор исключены)..."

# Очищаем файл
> "$RESULT_FILE"

# Ищем файлы, исключая конкретные пути
# Мы используем -path с подстановочными знаками, это работает стабильнее
find . -type f \
    -not -path "*/f2bulker/data/*" \
    -not -path "*/.git/*" \
    -not -path "*/.idea/*" \
    -not -path "*/vendor/*" \
    -not -path "*/bin/*" \
    -not -name "$RESULT_FILE" \
    -not -name "*.log" \
    -not -name "*.jsonl" \
    -not -name "*.fb2" \
    -not -name "*.zip" \
    -not -name "*.pb.go" \
    -not -name "go.sum" \
    -not -name "source*" \
    | sort | while read -r file; do
    
    clean_file="${file#./}"

    # Проверка на текст через grep
    if grep -Iq "" "$file"; then
        echo "Adding: $clean_file"
        echo "------------------------------------------------" >> "$RESULT_FILE"
        echo "FILE: $clean_file" >> "$RESULT_FILE"
        echo "------------------------------------------------" >> "$RESULT_FILE"
        cat "$file" >> "$RESULT_FILE"
        echo -e "\n" >> "$RESULT_FILE"
    fi
done

echo "🚀 Пересылка на $TARGET..."
ls -lh "$RESULT_FILE"
scp "$RESULT_FILE" "$TARGET"
