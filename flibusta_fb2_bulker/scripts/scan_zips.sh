#!/bin/bash
BASE_DIR=$(cd "$(dirname "$0")" && pwd)
CONFIG="$BASE_DIR/config.yaml"

get_cfg_path() {
    grep "$1" "$CONFIG" | head -n 1 | awk -F': ' '{print $2}' | tr -d '"' | tr -d "'" | tr -d ' '
}

SRC_DIR_RAW=$(get_cfg_path "source_dir")
OUT_DIR_RAW=$(get_cfg_path "output_dir")

convert_path() {
    local p=$1
    [[ "$p" == ./* ]] && echo "$BASE_DIR/${p#./}" || echo "$p"
}

SRC_DIR=$(convert_path "$SRC_DIR_RAW")
OUT_DIR=$(convert_path "$OUT_DIR_RAW")

mkdir -p "$OUT_DIR"

shopt -s nullglob
for zipfile in "$SRC_DIR"/*.zip; do
    base=$(basename "$zipfile")
    echo "Indexing: $base"
    "$BASE_DIR/bulker" -config "$CONFIG" -src "$zipfile" > "$OUT_DIR/${base}.jsonl"
done
