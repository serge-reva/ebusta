#!/usr/bin/env bash
set -euo pipefail
cd ~/projects/ebusta || exit 1

ts="$(date +%Y%m%d_%H%M%S)"
cp -a cmd/web-adapter/main.go "/tmp/web-adapter.main.go.bak.${ts}"
cp -a cmd/processor/main.go    "/tmp/processor.main.go.bak.${ts}"

python3 - <<'PY'
from pathlib import Path
import re

# 1) web-adapter: comment port 50054 -> 50053 (comment-only-ish, but we match the exact phrase)
p = Path("cmd/web-adapter/main.go")
s = p.read_text(encoding="utf-8", errors="replace")
s2, n1 = re.subn(r'порт\s+50054', 'порт 50053', s)
p.write_text(s2, encoding="utf-8")
print(f"OK: cmd/web-adapter/main.go: patched={n1}")

# 2) processor: log line :50053 -> :50054 (only the known log phrase)
p = Path("cmd/processor/main.go")
s = p.read_text(encoding="utf-8", errors="replace")
s2, n2 = re.subn(r'(Ebusta Processor started on\s+):50053\b', r'\1:50054', s)
p.write_text(s2, encoding="utf-8")
print(f"OK: cmd/processor/main.go: patched={n2}")
PY

# gofmt if available
if command -v gofmt >/dev/null 2>&1; then
  gofmt -w cmd/web-adapter/main.go cmd/processor/main.go
  echo "OK: gofmt -w applied"
else
  echo "WARN: gofmt not found in PATH (skipped)"
fi

echo "== verify snippets =="
grep -nE 'порт 5005[34]' cmd/web-adapter/main.go || true
grep -nE 'Ebusta Processor started on :5005[34]' cmd/processor/main.go || true

echo
echo "== changes =="
git diff -- cmd/web-adapter/main.go cmd/processor/main.go || true
