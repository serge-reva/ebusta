# Test Classification

This directory defines the target test taxonomy for `ebusta` and the migration baseline.

## Target Taxonomy
- `unit`: isolated Go tests, no real network, no external services.
- `integration`: `httptest`, mock transport, in-memory integrations between local components.
- `e2e`: end-to-end checks against a running stack (preferably Docker-based).
- `load/diagnostics`: load tests, stress checks, ad-hoc diagnostics. Not part of PR CI.

## Scope
- This stage is documentation-only.
- No test code changes are done here.
- Classification and migration priorities are tracked in `inventory.md`.

## Priority Levels
- `P0`: broken or outdated tests that currently block reliable CI (wrong ports, legacy endpoints, log-grepping assertions).
- `P1`: useful tests that need cleanup (duplication, structure, isolation improvements).
- `P2`: acceptable tests or optional suites that can be moved/refined later.

## CI Policy (Target)
- PR: `unit + integration` only.
- Nightly: `e2e`.
- Manual/on-demand: `load/diagnostics`.

See also: `ci-matrix.md` and `inventory.md`.
