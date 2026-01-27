#!/bin/bash

# Настройки
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
OUTPUT_FILE="ebusta_full_context_${TIMESTAMP}.txt"

echo "📂 Собираю ПОЛНЫЙ контекст (Go + Lisp + Proto + Makefile) в $OUTPUT_FILE..."

echo "=== EBusta Full Project Context: $(date) ===" > "$OUTPUT_FILE"

# 1. Структура файлов (чтобы понимать, где что лежит)
echo "--- SECTION: FILE TREE ---" >> "$OUTPUT_FILE"
find . -maxdepth 3 -not -path '*/.*' >> "$OUTPUT_FILE"
echo "--- END SECTION: FILE TREE ---" >> "$OUTPUT_FILE"

# 2. Исходники
find . -type f \
    \( -name "*.go" \
       -o -name "*.yaml" \
       -o -name "*.proto" \
       -o -name "Makefile" \
       -o -name "go.mod" \
       -o -name "*.lisp" \
       -o -name "*.asd" \
       -o -name "*.sh" \) \
    -not -path "./bin/*" \
    -not -path "*/data/*" \
    -not -path "./.git/*" \
    -not -path "*/grpc/third_party/*" \
    -not -path "*_context_*.txt" | while read -r file; do
        echo "" >> "$OUTPUT_FILE"
        echo "--- START_FILE: $file ---" >> "$OUTPUT_FILE"
        cat "$file" >> "$OUTPUT_FILE"
        echo "" >> "$OUTPUT_FILE"
        echo "--- END_FILE: $file ---" >> "$OUTPUT_FILE"
done

echo "✅ Готово. Файл: $OUTPUT_FILE"
echo "Размер: $(du -h "$OUTPUT_FILE" | awk '{print $1}')"
