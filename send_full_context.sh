#!/bin/bash

# Настройки
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
OUTPUT_FILE="ebusta_full_context_${TIMESTAMP}.txt"
REMOTE_DEST="reva@mars:/home/reva/to_chat"

echo "📂 Собираю ПОЛНЫЙ контекст (Go + Lisp + C++ + Proto) в $OUTPUT_FILE..."

# Очищаем файл перед началом
echo "=== EBusta Full Project Context: $(date) ===" > "$OUTPUT_FILE"

# 1. Добавляем Git Diff за последний час
echo "" >> "$OUTPUT_FILE"
echo "--- SECTION: GIT DIFF (LAST 1 HOUR) ---" >> "$OUTPUT_FILE"
if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    git diff --since="1 hour ago" >> "$OUTPUT_FILE"
else
    echo "Not a git repository or no changes in the last hour." >> "$OUTPUT_FILE"
fi
echo "--- END SECTION: GIT DIFF ---" >> "$OUTPUT_FILE"

# 2. Собираем исходники
# Объединены фильтры из ebusta (go, yaml, json...) и grpc (lisp, cc, asd...)
find . -type f \
    \( -name "*.go" \
       -o -name "*.yaml" \
       -o -name "*.proto" \
       -o -name "*.json" \
       -o -name "*.md" \
       -o -name "Makefile" \
       -o -name "go.mod" \
       -o -name "*.lisp" \
       -o -name "*.cc" \
       -o -name "*.asd" \
       -o -name "*.sexp" \) \
    -not -path "./bin/*" \
    -not -path "*/data/*" \
    -not -path "./.git/*" \
    -not -path "*/uploader.log" \
    -not -path "*.log" \
    -not -path "*_context_*.txt" | while read -r file; do
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
    echo "🎉 Файл успешно отправлен: $REMOTE_DEST/$OUTPUT_FILE"
    echo "Теперь загрузи этот файл мне."
else
    echo "❌ Ошибка при отправке через scp"
fi
