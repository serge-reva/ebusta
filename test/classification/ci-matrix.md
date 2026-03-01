# CI Test Matrix

This document defines where each test level should run.

## PR Pipeline
- Run: `make test-unit`, `make test-integration`, `make test-scala`
- Goal: fast feedback, deterministic, no external network dependencies.
- Target duration: short (minutes, not tens of minutes).

## Nightly Pipeline
- Run: `make test-e2e`
- Goal: validate full-service wiring and contract compatibility on a real stack.
- Includes resilience checks (service stop/start and recovery assertions via gateway).
- Environment: Docker Compose stack with runtime artifacts built on host.

## Manual / On-Demand
- Run: `make test-load`, scripts from `test/diagnose/`
- Goal: performance and deep diagnostics.
- Not a merge gate.

## Execution Policy
- Failing `unit`, `integration`, or `scala` tests blocks PR merge.
- Failing `e2e` blocks release readiness and must be triaged.
- `load/diagnostics` failures are analyzed as operational signals, not CI blockers.
