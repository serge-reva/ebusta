#!/bin/bash
set -euo pipefail

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
CONTEXT_FILE="ebusta_full_context_${TIMESTAMP}.txt"
REMOTE_DEST="reva@mars:/home/reva/to_chat"

echo "📄 Собираю текстовый контекст: $CONTEXT_FILE..."

# Очистка старого файла, если вдруг существует
> "$CONTEXT_FILE"

# Поиск файлов согласно твоему актуальному стеку (Go + Scala)
find . -type f \
  \( -name "*.go" \
     -o -name "*.yaml" \
     -o -name "*.proto" \
     -o -name "*.json" \
     -o -name "Makefile" \
     -o -name "go.mod" \
     -o -name "*.cc" \
     -o -name "*.sh" \
     -o -name "*.asd" \
     -o -name "*.sexp" \
     -o -name "*.scala" \
     -o -name "*.sbt" \
     -o -name "*.properties" \
     -o -name "build.sbt" \) \
  -not -path "./bin/*" \
  -not -path "./old*" \
  -not -path "./lisp*" \
  -not -path "./grpc/*" \
  -not -path "*/data/*" \
  -not -path "./.git/*" \
  -not -path "*/uploader.log" \
  -not -path "*/target/*" \
  -not -path "*/.bsp/*" \
  -not -path "*/.metals/*" \
  -not -path "*/.idea/*" \
  -not -path "*/.vscode/*" \
  -not -name "*.log" \
  -not -name "*bak*" \
  -not -name "*_context_*.txt" \
  -print0 | sort -z | while IFS= read -r -d '' file; do
    echo "--- FILE: ${file#./} ---" >> "$CONTEXT_FILE"
    cat "$file" >> "$CONTEXT_FILE"
    echo -e "\n" >> "$CONTEXT_FILE"
done

echo "✅ Текстовый контекст готов. Размер: $(du -h "$CONTEXT_FILE" | awk '{print $1}')"

echo "🚀 Отправляю на mars..."
scp "$CONTEXT_FILE" "$REMOTE_DEST"

echo "🎉 Отправлено: $REMOTE_DEST/$CONTEXT_FILE"
