# Engineering Standard

Version: 1.0  
Last Updated: 2026-03-01

This document defines daily engineering execution standards for `ebusta`.

## Docker-First Verification
After meaningful changes (contracts, wiring, runtime behavior):
1. Build host artifacts.
2. Start stack in Docker.
3. Run manual happy path and error path checks.
4. Stop stack.

Minimum command set:
- `make docker-build`
- `make docker-up`
- `make docker-status`
- `make docker-logs`
- `make docker-down`

## Feature vs Refactor Separation
- Keep feature work and refactoring in separate commits.
- Avoid mixed commits that change behavior and structure simultaneously.
- Preserve green gates per commit whenever possible.

## Documentation Discipline
- Read relevant files in `docs/` before starting a new stage.
- Update docs in the same change when behavior/contracts/rules change.
- Keep architecture and operations docs synchronized with Makefile gates.

## Testing Standard
- Follow taxonomy and execution model in:
  - `test/classification/README.md`
  - `test/classification/ci-matrix.md`
  - `test/classification/inventory.md`
- PR baseline gates:
  - `make test-unit`
  - `make test-integration`
  - `make test-scala`
- Use `make test-e2e` for docker stack validation.
- Keep load/diagnostics out of PR merge gate.

## Proto Workflow
- Proto contracts are immutable by default; use additive evolution.
- Required checks:
  - `make proto-generate`
  - `make proto-verify`
- See [PROTO_IMMUTABILITY.md](PROTO_IMMUTABILITY.md).

## Operational Error/Trace Discipline
- Error mapping follows [API_ERROR_MAPPING.md](API_ERROR_MAPPING.md).
- Trace propagation follows [TRACE.md](TRACE.md).
