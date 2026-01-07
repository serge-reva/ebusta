#!/bin/bash

# Читаем конфиг для получения пути к warn_dir
BASE_DIR=$(cd "$(dirname "$0")/.." && pwd)
CONFIG="$BASE_DIR/config.yaml"
WARN_DIR=$(grep "warn_dir" "$CONFIG" | awk -F': ' '{print $2}' | tr -d '"' | tr -d ' ' | tr -d "'")

# Превращаем относительный путь в абсолютный
[[ "$WARN_DIR" == ./* ]] && WARN_DIR="$BASE_DIR/${WARN_DIR#./}"

if [ ! -d "$WARN_DIR" ] || [ -z "$(ls -A "$WARN_DIR")" ]; then
    echo "Directory $WARN_DIR is empty or does not exist. Nothing to analyze."
    exit 0
fi

REPORT_FILE="$WARN_DIR/analysis_report.txt"
echo "--- Warn Analysis Report: $(date) ---" > "$REPORT_FILE"
echo "System Info: $(uname -a)" >> "$REPORT_FILE"

for f in "$WARN_DIR"/*.fb2; do
    [ -e "$f" ] || continue
    FILENAME=$(basename "$f")
    
    echo "File: $FILENAME" >> "$REPORT_FILE"
    echo "Size: $(stat -c%s "$f") bytes" >> "$REPORT_FILE"
    
    # Пытаемся определить кодировку через стандартную утилиту file
    FILE_INFO=$(file -b "$f")
    echo "File Type: $FILE_INFO" >> "$REPORT_FILE"
    
    # Если есть enca, используем её для более точного определения
    if command -v enca >/dev/null 2>&1; then
        echo "Enca Guess: $(enca -L russian -i "$f" 2>/dev/null)" >> "$REPORT_FILE"
    fi

    echo "Head Snippet (first 10 lines):" >> "$REPORT_FILE"
    # Читаем через cat -v чтобы видеть непечатные символы и BOM
    head -n 10 "$f" | cat -v >> "$REPORT_FILE"
    echo "--------------------------------------" >> "$REPORT_FILE"
done

echo "Report generated: $REPORT_FILE"

# Отправка на удаленный хост
echo "Transferring to reva@mars:/home/to_chat..."
scp -r "$WARN_DIR" reva@mars:/home/to_chat

if [ $? -eq 0 ]; then
    echo "✅ Success: Files and report sent."
else
    echo "❌ Error: SCP failed."
    exit 1
fi
