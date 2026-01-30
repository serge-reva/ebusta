#!/usr/bin/env bash
set -euo pipefail

ROOT="$(pwd)"
TS="$(date +%Y%m%d_%H%M%S)"
OUT="${ROOT}/ebusta_min_context_${TS}.txt"
DEST="${DEST:-reva@mars:/home/reva/to_chat/}"

emit_file () {
  local f="$1"
  [[ -f "$f" ]] || return 0
  echo
  echo "--- START_FILE: ./${f} ---"
  sed -n '1,2000p' "$f"
  echo "--- END_FILE: ./${f} ---"
}

emit_files_list () {
  local title="$1"; shift
  echo
  echo "## ${title}"
  for f in "$@"; do
    if [[ -f "$f" ]]; then
      echo "./$f"
    fi
  done
}

{
  echo "=== EBusta MIN Context: $(date) ==="
  echo "HOST: $(hostname)"
  echo "PWD:  ${ROOT}"
  echo

  echo "## GIT"
  git rev-parse HEAD 2>/dev/null || true
  git status -sb 2>/dev/null || true
  echo
  echo "## CHANGES (summary)"
  git diff --stat 2>/dev/null || true
  echo
  echo "## UNTRACKED / MODIFIED (porcelain)"
  git status --porcelain 2>/dev/null || true
  echo

  # Список только “ключевых” файлов (без ls -lR)
  emit_files_list "KEY FILES (paths only)" \
    INSTRUCTIONS.txt Makefile go.mod go.sum ebusta.yaml README.md

  emit_files_list "ENTRYPOINTS (cmd/*/main.go)" \
    cmd/orchestrator/main.go \
    cmd/datamanager/main.go \
    cmd/processor/main.go \
    cmd/web-adapter/main.go \
    cmd/message-converter/main.go \
    cmd/diagnose/main.go \
    cmd/cli/main.go

  echo
  echo "## PROTO LIST (api/proto/v1)"
  ls -1 api/proto/v1/*.proto 2>/dev/null || true

  echo
  echo "## LISP-CONVERTER LIST (paths only)"
  ls -1 lisp-converter/* 2>/dev/null || true

  echo
  echo "## TESTS LIST (paths only)"
  ls -1 tests/* 2>/dev/null || true

  echo
  echo "## PORTS & GRPC DIAL/LISTEN (NO internal/*, NO big dirs)"
  echo "# listen:"
  grep -RIn --exclude-dir=.git -E 'net\.Listen\("tcp"|ListenAndServe\(":' \
    ./cmd ./api ./lisp-converter ./tests ./ 2>/dev/null || true
  echo
  echo "# dial:"
  grep -RIn --exclude-dir=.git -E 'grpc\.Dial\(".*:[0-9]+' \
    ./cmd ./tests ./ 2>/dev/null || true
  echo
  echo "# raw ports:"
  grep -RIn --exclude-dir=.git -E ':[0-9]{4,5}\b' \
    ./cmd ./api ./lisp-converter ./tests ./ 2>/dev/null || true

  # Содержимое только нужного для анализа
  echo
  echo "## CONTENT"
  emit_file "INSTRUCTIONS.txt"
  emit_file "Makefile"
  emit_file "go.mod"
  emit_file "go.sum"
  emit_file "ebusta.yaml"
  emit_file "README.md"

  # proto — основа контракта
  for p in api/proto/v1/*.proto; do
    [[ -f "$p" ]] && emit_file "$p"
  done

  # lisp-converter — источник истины по DSL (берём только легковесные исходники)
  for f in lisp-converter/*.proto lisp-converter/*.lisp lisp-converter/*.asd lisp-converter/Makefile lisp-converter/README*; do
    [[ -f "$f" ]] && emit_file "$f"
  done

  # entrypoints — где слушают/куда звонят
  for f in \
    cmd/orchestrator/main.go \
    cmd/datamanager/main.go \
    cmd/processor/main.go \
    cmd/web-adapter/main.go \
    cmd/message-converter/main.go \
    cmd/diagnose/main.go \
    cmd/cli/main.go
  do
    [[ -f "$f" ]] && emit_file "$f"
  done

  # тесты — чтобы понять, что должно работать end-to-end
  for f in tests/*.go tests/*.sh; do
    [[ -f "$f" ]] && emit_file "$f"
  done

} > "$OUT"

echo "OK: wrote $OUT"
echo "SIZE:"
wc -c "$OUT" || true

echo "Uploading via scp to $DEST"
scp "$OUT" "$DEST"
echo "OK: uploaded"
