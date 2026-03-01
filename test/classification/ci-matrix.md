# CI Test Matrix

This document defines where each test level should run.

## PR Pipeline
- Run: `unit`, `integration`
- Goal: fast feedback, deterministic, no external network dependencies.
- Target duration: short (minutes, not tens of minutes).

## Nightly Pipeline
- Run: `e2e`
- Goal: validate full-service wiring and contract compatibility on a real stack.
- Environment: Docker Compose stack with runtime artifacts built on host.

## Manual / On-Demand
- Run: `load/diagnostics`
- Goal: performance and deep diagnostics.
- Not a merge gate.

## Execution Policy
- Failing `unit` or `integration` blocks PR merge.
- Failing `e2e` blocks release readiness and must be triaged.
- `load/diagnostics` failures are analyzed as operational signals, not CI blockers.
