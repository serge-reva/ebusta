#!/bin/bash
set -euo pipefail

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
ARCHIVE="ebusta_full_context_${TIMESTAMP}.tar.gz"
REMOTE_DEST="reva@mars:/home/reva/to_chat"

echo "📦 Собираю контекст в архив: $ARCHIVE..."

# Список файлов в архив (тот же набор расширений/файлов, те же исключения)
FILELIST="$(mktemp)"
trap 'rm -f "$FILELIST"' EXIT

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
     -o -name "*.sh" \
     -o -name "*.asd" \
     -o -name "*.sexp" \) \
  -not -path "./bin/*" \
  -not -path "./old*" \
  -not -path "./lisp*" \
  -not -path "./grpc/*" \
  -not -path "*/data/*" \
  -not -path "./.git/*" \
  -not -path "*/uploader.log" \
  -not -name "*.log" \
  -not -name "*.md" \
  -not -name "*bak*" \
  -not -name "*_context_*.txt" \
  -print0 | sort -z | tr '\0' '\n' > "$FILELIST"

# Собрать tar.gz (пути без ведущего ./)
tar -czf "$ARCHIVE" -T "$FILELIST" --transform='s|^\./||'

echo "✅ Архив готов. Размер: $(du -h "$ARCHIVE" | awk '{print $1}')"

echo "🚀 Отправляю на mars..."
scp "$ARCHIVE" "$REMOTE_DEST"

echo "🎉 Отправлено: $REMOTE_DEST/$ARCHIVE"
