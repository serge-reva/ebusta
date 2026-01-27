#!/usr/bin/env bash
set -euo pipefail
echo "[A] DSL smoke: expecting Lisp DSL on :50052"

# Uses existing Go compliance runner (talks to :50052)
go run tests/compliance_runner.go | tee /tmp/ebusta_dsl_smoke.log

# Must have at least one PASSED and zero FAILED
grep -q "PASSED" /tmp/ebusta_dsl_smoke.log
if grep -q "FAILED" /tmp/ebusta_dsl_smoke.log; then
  echo "DSL smoke failed"
  exit 1
fi

echo "DSL smoke OK"
