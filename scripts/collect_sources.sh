#!/bin/bash

OUTPUT_FILE="sources_clean.txt"

echo "📦 Collecting sources to $OUTPUT_FILE..."
echo "🚫 Excluding: fb2bulker/data, .git, bin, .idea, .vscode"

# Очищаем старый файл
echo "" > "$OUTPUT_FILE"

# Ищем файлы, исключая лишнее
find . -type f \
  -not -path "*/.git/*" \
  -not -path "*/.idea/*" \
  -not -path "*/.vscode/*" \
  -not -path "./fb2bulker/data/*" \
  -not -path "./bin/*" \
  -not -name "go.sum" \
  -not -name "*.log" \
  -not -name "$OUTPUT_FILE" \
  -not -name "*.pb.go" \
  -exec bash -c 'echo -e "\n------------------------------------------------\nFILE: $1\n------------------------------------------------"; cat "$1"' _ {} \; >> "$OUTPUT_FILE"

echo "✅ Done! File size:"
ls -lh "$OUTPUT_FILE"
