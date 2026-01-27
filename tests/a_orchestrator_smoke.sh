#!/usr/bin/env bash
set -euo pipefail
echo "[A] Orchestrator smoke: expecting orchestrator on :50053, DSL on :50052"

# Diagnose checks orchestrator + dsl
go run cmd/diagnose/main.go | tee /tmp/ebusta_orch_diag.log

# Require orchestrator and converter checks to pass
grep -q "✅ OrchestratorService OK" /tmp/ebusta_orch_diag.log
grep -q "✅ ConverterService OK" /tmp/ebusta_orch_diag.log

echo "Orchestrator smoke OK"
