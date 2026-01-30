#!/usr/bin/env bash
set -euo pipefail
cd ~/projects/ebusta || exit 1

ts="$(date +%Y%m%d_%H%M%S)"
cp -a Makefile "/tmp/Makefile.bak.${ts}"

python3 - <<'PY'
from pathlib import Path
import re

p = Path("Makefile")
s = p.read_text(encoding="utf-8", errors="replace")

# 1) drop port variables (exact names)
s, n_vars = re.subn(
    r'(?m)^\s*(WEB_PORT|ORCH_PORT|DSL_PORT|DATA_PORT|METRICS_PORT)\s*(?:\?|:)?=\s*.*\n',
    '',
    s
)

# 2) remove "-port <something>" from start commands (anywhere in Makefile, but only for known binaries)
s2 = s
s2, n_portflags = re.subn(
    r'(\./(?:datamanager|orchestrator|processor|web-adapter)\b)\s+-port\s+\S+',
    r'\1',
    s2
)

if s2 == s and n_vars == 0:
    print("OK: Makefile already clean (no vars, no -port flags) -> no changes")
else:
    p.write_text(s2, encoding="utf-8")
    print(f"OK: removed var-lines={n_vars}, removed -port flags={n_portflags}")

PY

echo "== verify Makefile contains no port vars / -port flags =="
grep -nE '^(WEB_PORT|ORCH_PORT|DSL_PORT|DATA_PORT|METRICS_PORT)\b|[[:space:]]-port[[:space:]]' Makefile || true
