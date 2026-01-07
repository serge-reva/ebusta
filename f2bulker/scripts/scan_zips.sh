#!/bin/bash
BASE_DIR=$(cd "$(dirname "$0")/.." && pwd)
CONFIG="$BASE_DIR/config.yaml"

RESCAN_FLAG=""
if [ "$1" == "--rescan" ]; then RESCAN_FLAG="-rescan"; fi

get_cfg_val() {
    grep "$1" "$CONFIG" | head -n 1 | awk -F': ' '{print $2}' | tr -d '"' | tr -d "'" | tr -d ' '
}

SRC_DIR=$(get_cfg_val "source_dir")
OUT_DIR=$(get_cfg_val "output_dir")
SLEEP_TIME=$(get_cfg_val "sleep_between_zips")
SLEEP_TIME=${SLEEP_TIME:-0}

# Конвертация путей
[[ "$SRC_DIR" == ./* ]] && SRC_DIR="$BASE_DIR/${SRC_DIR#./}"
[[ "$OUT_DIR" == ./* ]] && OUT_DIR="$BASE_DIR/${OUT_DIR#./}"

mkdir -p "$OUT_DIR"

shopt -s nullglob
for zipfile in "$SRC_DIR"/*.zip; do
    base=$(basename "$zipfile")
    echo "--- [$(date +%H:%M:%S)] Container: $base ---"
    
    # Запуск
    "$BASE_DIR/bin/f2bulker" $RESCAN_FLAG -config "$CONFIG" -src "$zipfile" -out "$OUT_DIR/${base}.jsonl"
    EXIT_CODE=$?
    
    if [ $EXIT_CODE -eq 10 ]; then
        echo ">>> All files skipped. Skipping cool-down."
    elif [ $EXIT_CODE -eq 0 ] && [ "$SLEEP_TIME" -gt 0 ]; then
        echo ">>> Work done. Cooling down for $SLEEP_TIME sec..."
        sleep "$SLEEP_TIME"
    fi
done
