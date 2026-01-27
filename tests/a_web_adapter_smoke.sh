#!/usr/bin/env bash
set -euo pipefail
echo "[A] Web-adapter smoke: expecting HTTP on :50080"

# simple TCP check via diagnose (already checks web on :50080)
go run cmd/diagnose/main.go | tee /tmp/ebusta_web_diag.log
grep -q "✅ WebAdapterService OK" /tmp/ebusta_web_diag.log

echo "Web-adapter smoke OK"
