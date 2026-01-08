#!/bin/bash

# Проверяем, передано ли имя файла
if [ -z "$1" ]; then
    echo "❌ Ошибка: Укажите имя результирующего файла."
    echo "Пример: ./scripts/pack_and_send.sh project_context.txt"
    exit 1
fi

RESULT_FILE="$1"
TARGET="reva@mars:/home/reva/to_chat"

echo "📦 Собираем исходный код (без данных и мусора) в $RESULT_FILE..."

# Очищаем файл
> "$RESULT_FILE"

# ИСПРАВЛЕНИЕ:
# 1. Исключаем технические папки (.git, .idea, .vscode)
# 2. Исключаем скомпилированные бинарники (bin)
# 3. ВАЖНО: Исключаем папку с данными fb2bulker/data
# 4. Исключаем старые дампы (source*) и go.sum (лишний шум)
find . -type f \
    -not -path '*/.git/*' \
    -not -path '*/.idea/*' \
    -not -path '*/.vscode/*' \
    -not -path '*/bin/*' \
    -not -path '*/fb2bulker/data/*' \
    -not -name "$RESULT_FILE" \
    -not -name 'source*' \
    -not -name 'go.sum' \
    | sort | while read -r file; do
    
    # Убираем префикс "./" для красоты
    clean_file="${file#./}"

    # Проверка на бинарность (grep считает файл бинарным, если там есть NULL байт)
    if grep -Iq "" "$file"; then
        echo "Processing: $clean_file"
        echo "------------------------------------------------" >> "$RESULT_FILE"
        echo "FILE: $clean_file" >> "$RESULT_FILE"
        echo "------------------------------------------------" >> "$RESULT_FILE"
        cat "$file" >> "$RESULT_FILE"
        echo -e "\n" >> "$RESULT_FILE"
    else
        echo "Skipping binary: $clean_file"
    fi
done

echo "🚀 Пересылка файла на $TARGET..."

scp "$RESULT_FILE" "$TARGET"

if [ $? -eq 0 ]; then
    echo "✅ Готово! Файл успешно отправлен."
else
    echo "❌ Ошибка при отправке scp."
fi
