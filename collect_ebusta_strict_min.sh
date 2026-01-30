#!/usr/bin/env bash
set -euo pipefail

ROOT="$(pwd)"
TS="$(date +%Y%m%d_%H%M%S)"
OUT="${ROOT}/ebusta_strict_min_${TS}.txt"
DEST="${DEST:-reva@mars:/home/reva/to_chat/}"

emit () {
  local f="$1"
  [[ -f "$f" ]] || return 0
  echo
  echo "--- START_FILE: ./${f} ---"
  cat "$f"
  echo "--- END_FILE: ./${f} ---"
}

{
  echo "=== EBusta STRICT MIN Context: $(date) ==="
  echo "PWD: $ROOT"
  echo

  echo "## PORT MAP (only relevant dirs)"
  echo "# listen:"
  grep -RIn --exclude-dir=.git -E 'net\.Listen\("tcp"|ListenAndServe\(":' \
    ./cmd ./api/proto/v1 ./lisp-converter ./tests 2>/dev/null || true
  echo
  echo "# dial:"
  grep -RIn --exclude-dir=.git -E 'grpc\.Dial\(' \
    ./cmd ./tests 2>/dev/null || true
  echo
  echo "# ports literals:"
  grep -RIn --exclude-dir=.git -E ':[0-9]{4,5}\b' \
    ./cmd ./lisp-converter ./tests ./ebusta.yaml 2>/dev/null || true
  echo

  echo "## FILES INCLUDED (paths only)"
  ls -1 api/proto/v1/*.proto 2>/dev/null || true
  ls -1 cmd/*/main.go 2>/dev/null || true
  ls -1 lisp-converter/search.proto lisp-converter/*.lisp 2>/dev/null || true
  ls -1 tests/* 2>/dev/null || true
  echo

  echo "## CONTENT"
  emit "INSTRUCTIONS.txt"
  emit "Makefile"
  emit "ebusta.yaml"

  for p in api/proto/v1/*.proto; do emit "$p"; done

  emit "lisp-converter/search.proto"
  for f in lisp-converter/*.lisp; do emit "$f"; done

  for f in cmd/*/main.go; do emit "$f"; done

  for f in tests/*; do
    [[ -f "$f" ]] && emit "$f"
  done

} > "$OUT"

echo "OK: wrote $OUT"
wc -c "$OUT" || true

echo "Uploading via scp to $DEST"
scp "$OUT" "$DEST"
echo "OK: uploaded"
