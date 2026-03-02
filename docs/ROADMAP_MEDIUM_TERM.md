# Ebusta Medium-Term Roadmap
Version: 1.1  
Last Updated: 2026-03-02

## Purpose
This document captures improvements that were intentionally deferred after completion of the current priority stages (config validation baseline, OpenAPI baseline, web-frontend unification through gateway, internal mTLS).

## Planning Window
Target horizon: next **3-6 months** (tentative, resource-dependent).

## Priority Overview
| ID | Name | Priority | Target Window | Status |
|---|---|---|---|---|
| SSRF-01 | SSRF protection in datamanager | P6 | 1-2 months | Planned |
| AUTH-01 | Gateway authentication | P7 | 2-4 months | Planned |
| CFG-02 | Extended config validation hardening | P8 | 1-3 months | Planned |
| PERF-01 | Query-builder cache tuning & controls | P9 | 3-6 months | Planned |
| REL-01 | Graceful-shutdown/runtime resilience audit | P9 | 3-6 months | Planned |
| RL-01 | Rate-limiting hardening and metrics expansion | P9 | 3-6 months | Planned |

## Tasks

### SSRF-01 — SSRF Protection in Datamanager
- Priority: `P6`
- Description: Prevent unsafe OpenSearch targets (loopback/private/link-local/reserved ranges) from being used by configuration.
- Implementation steps:
  - Introduce `IsSafeURL` helper in `internal/security` (or `internal/config`).
  - Resolve host and validate all resolved IPs against deny-list ranges.
  - Integrate check at datamanager startup (`cmd/datamanager/main.go`) and fail-fast on unsafe target.
  - Add unit tests for safe/unsafe cases (localhost, RFC1918, link-local, public).
- Expected result: Datamanager starts only with trusted OpenSearch endpoints.
- Related docs: `docs/ARCHITECTURAL_CONSTITUTION.md`, `docs/TRACE.md`.

### AUTH-01 — Authentication for Gateway
- Priority: `P7`
- Description: Add authentication/authorization controls for public gateway APIs.
- Implementation steps:
  - Select model: JWT or API keys.
  - Implement gateway middleware for auth verification.
  - Define token/key lifecycle (issue, rotate, revoke) and error mapping.
  - Add configuration examples and request/response docs.
- Expected result: Gateway endpoints are accessible only to authorized clients.
- Related docs: `docs/API_ERROR_MAPPING.md`, `docs/ARCHITECTURAL_CONSTITUTION.md`, `docs/ENGINEERING_STANDARD.md`.

### CFG-02 — Extended Config Validation Hardening
- Priority: `P8`
- Description: Extend existing validation to stricter semantic checks.
- Implementation steps:
  - Add URL scheme checks where currently only non-empty validation exists.
  - Add stronger port/address constraints and cross-field validation.
  - Add validation for filesystem paths and file readability where required.
  - Add negative tests for startup validation paths.
- Expected result: More misconfigurations are rejected before runtime.
- Related docs: `docs/ENGINEERING_STANDARD.md`, `docs/RUNBOOK.md`.

### PERF-01 — Query-Builder Cache Tuning & Controls
- Priority: `P9`
- Description: Improve operational control of query cache behavior for production load profiles.
- Implementation steps:
  - Add cache observability (hit/miss/eviction counters exported and documented).
  - Validate/tune TTL and max-size defaults for production traffic.
  - Add guardrails for cache key cardinality and memory growth.
  - Add repeatable load-profile benchmark scenario.
- Expected result: Stable and measurable cache efficiency without memory risk.
- Related docs: `docs/ENGINEERING_STANDARD.md`, `docs/TRACE.md`.

### REL-01 — Graceful Shutdown / Runtime Resilience Audit
- Priority: `P9`
- Description: Audit current shutdown behavior under container orchestration and dependency failures.
- Implementation steps:
  - Verify signal handling and shutdown timeouts under real docker-compose conditions.
  - Validate in-flight request behavior during termination.
  - Standardize shutdown logs and add checklist to runbook.
  - Add regression scenario to resilience/e2e suite.
- Expected result: Predictable and test-backed termination behavior across services.
- Related docs: `docs/RUNBOOK.md`, `docs/ARCHITECTURAL_CONSTITUTION.md`.

### RL-01 — Rate Limiting Hardening and Metrics Expansion
- Priority: `P9`
- Description: Improve abuse protection and observability of gateway rate-limiting decisions.
- Implementation steps:
  - Refine per-action policies and defaults.
  - Add explicit metrics for allow/deny decisions by source/action.
  - Validate edge cases (burst traffic, tokenized users, adapter clients).
  - Add dashboard/alerts for rejection spikes.
- Expected result: Better protection against overload and better operational visibility.
- Related docs: `docs/ARCHITECTURAL_CONSTITUTION.md`, `docs/API_ERROR_MAPPING.md`, `docs/MTLS_INTERNAL.md`.

## Notes
- This roadmap contains **deferred** items only.
- Execution order may change based on incidents, product priorities, and team capacity.
- Work starts after current active tracks are closed and resources are available.
