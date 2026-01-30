#!/usr/bin/env bash
set -euo pipefail

ROOT="$(pwd)"
TS="$(date +%Y%m%d_%H%M%S)"
OUT="${ROOT}/ebusta_current_context_${TS}.txt"
DEST="reva@mars:/home/reva/to_chat/"

emit_file () {
  local f="$1"
  if [[ -f "$f" ]]; then
    echo
    echo "--- START_FILE: ./${f} ---"
    cat "$f"
    echo "--- END_FILE: ./${f} ---"
  fi
}

emit_glob () {
  local g="$1"
  shopt -s nullglob
  local files=( $g )
  shopt -u nullglob
  for f in "${files[@]}"; do
    emit_file "$f"
  done
}

{
  echo "=== EBusta CURRENT Project Context: $(date) ==="
  echo "HOST: $(hostname)"
  echo "PWD:  ${ROOT}"
  echo

  echo "## GIT"
  git rev-parse --show-toplevel 2>/dev/null || true
  git rev-parse HEAD 2>/dev/null || true
  git status -sb 2>/dev/null || true
  echo
  git remote -v 2>/dev/null || true
  echo
  git log --oneline --decorate -20 2>/dev/null || true
  echo
  echo "## GIT DIFF (summary)"
  git diff --stat 2>/dev/null || true
  echo

  echo "## FILE LIST (ls -lR like)"
  (cd "$ROOT" && ls -lR --time-style=long-iso --ignore=.git 2>/dev/null) || true
  echo

  echo "## PORTS & GRPC DIAL/LISTEN (quick grep; NO internal/*)"
  echo "# listen:"
  grep -RIn --exclude-dir=.git -E 'net\.Listen\("tcp"|ListenAndServe\(":' \
    ./cmd ./lisp-converter ./api ./tests ./ 2>/dev/null || true
  echo
  echo "# dial:"
  grep -RIn --exclude-dir=.git -E 'grpc\.Dial\(".*:[0-9]+' \
    ./cmd ./tests ./ 2>/dev/null || true
  echo
  echo "# raw ports:"
  grep -RIn --exclude-dir=.git -E ':[0-9]{4,5}\b' \
    ./cmd ./lisp-converter ./api ./tests ./ 2>/dev/null || true
  echo

  echo "## KEY TOP-LEVEL FILES"
  emit_file "README.md"
  emit_file "INSTRUCTIONS.txt"
  emit_file "Makefile"
  emit_file "go.mod"
  emit_file "ebusta.yaml"
  emit_file "errors.yaml"
  emit_file "books.json"
  echo

  echo "## PROTO (api/proto/v1)"
  emit_glob "api/proto/v1/*.proto"
  echo

  echo "## GENERATED (api/proto/v1) (if present)"
  emit_glob "api/proto/v1/*.pb.go"
  emit_glob "api/proto/v1/*_grpc.pb.go"
  echo

  echo "## DSL / LISP-CONVERTER (SOURCE OF TRUTH)"
  emit_glob "lisp-converter/*.proto"
  emit_glob "lisp-converter/*.lisp"
  emit_glob "lisp-converter/*"
  echo

  echo "## CMD (entrypoints)"
  emit_glob "cmd/*/main.go"
  emit_glob "cmd/*/*.yaml"
  echo

  echo "## TESTS"
  emit_glob "tests/*"
  echo

} > "$OUT"

echo "OK: wrote $OUT"
echo "Uploading via scp to $DEST"
scp "$OUT" "$DEST"
echo "OK: uploaded"
