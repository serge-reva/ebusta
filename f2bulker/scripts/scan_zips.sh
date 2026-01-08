#!/bin/bash
BASE_DIR=$(cd "$(dirname "$0")/.." && pwd)
CONFIG="$BASE_DIR/config.yaml"

RESCAN_FLAG=""
if [ "$1" == "--rescan" ]; then RESCAN_FLAG="-rescan"; fi

get_cfg_val() {
    grep "$1" "$CONFIG" | head -n 1 | awk -F': ' '{print $2}' | tr -d '"' | tr -d "'" | tr -d ' '
}

SRC_DIR=$(get_cfg_val "source_dir")
SLEEP_TIME=$(get_cfg_val "sleep_between_zips")
SLEEP_TIME=${SLEEP_TIME:-0}

[[ "$SRC_DIR" == ./* ]] && SRC_DIR="$BASE_DIR/${SRC_DIR#./}"

shopt -s nullglob
for zipfile in "$SRC_DIR"/f.*.zip "$SRC_DIR"/d.*.zip "$SRC_DIR"/fb2-*.zip; do
    [ -e "$zipfile" ] || continue
    base=$(basename "$zipfile")
    echo "--- [$(date +%H:%M:%S)] Container: $base ---"
    
    "$BASE_DIR/bin/f2bulker" $RESCAN_FLAG -config "$CONFIG" -container "$base"
    EXIT_CODE=$?
    
    if [ $EXIT_CODE -eq 0 ] && [ "$SLEEP_TIME" -gt 0 ]; then
        echo ">>> Work done. Cooling down for $SLEEP_TIME sec..."
        sleep "$SLEEP_TIME"
    fi
done
