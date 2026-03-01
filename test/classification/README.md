# Test Classification

This directory defines the target test taxonomy for `ebusta`, migration baseline, and execution split.

## Target Taxonomy
- `unit`: isolated Go tests, no real network, no external services.
- `integration`: `httptest`, mock transport, in-memory integrations between local components.
- `e2e`: end-to-end checks against a running stack (preferably Docker-based).
- `load/diagnostics`: load tests, stress checks, ad-hoc diagnostics. Not part of PR CI.

## Current Layout
- `test/e2e/`: docker-stack end-to-end scripts.
- `test/load/`: load and stress scripts (non-CI).
- `test/diagnose/`: operational diagnostics (non-CI).
- `test/utils/`: shared test helpers/utilities.
- `dsl-scala/src/test/scala/`: Scala parser tests.
- `query-builder/src/test/scala/`: Scala query-builder tests.

## Priority Levels
- `P0`: broken or outdated tests that currently block reliable CI (wrong ports, legacy endpoints, log-grepping assertions).
- `P1`: useful tests that need cleanup (duplication, structure, isolation improvements).
- `P2`: acceptable tests or optional suites that can be moved/refined later.

## CI Policy (Target)
- PR: `unit + integration` only.
- Nightly: `e2e`.
- Manual/on-demand: `load/diagnostics`.

## Make Targets
- `make test-unit`
- `make test-integration`
- `make test-scala`
- `make test-e2e`
- `make test-load`

See also: `ci-matrix.md` and `inventory.md`.
