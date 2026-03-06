# Architectural Constitution

Version: 1.1  
Last Updated: 2026-03-06

This document defines non-negotiable architectural rules for `ebusta`.

## Invariants
- `protobuf-first`: inter-service contracts are defined via `.proto` and validated with `buf`.
- `trace-id required`: every request must carry a trace id; if absent, service generates one.
- `errors as data`: domain/business errors are represented as structured diagnostics, not hidden transport failures.
- `separation of responsibilities`: adapters, orchestration, storage, and shared infra must stay isolated.
- `append-only contract policy`: backward compatibility is mandatory for public contracts.
- `internal gRPC mTLS`: all internal gRPC traffic must use mutual TLS in runtime environments.

## Component Boundaries
- `gateway` (`cmd/gateway`, `internal/gateway`): HTTP adapter only.
- `gateway` must not implement persistence logic, storage node behavior, or domain search rules.
- `gateway` must not perform filesystem operations in request flow.
- `gateway` must not import `internal/downloads`.
- `orchestrator` (`cmd/orchestrator`): coordinates search flow between DSL, query-builder, and datamanager.
- `datamanager` (`cmd/datamanager`): OpenSearch query execution and mapping only.
- `download nodes` (`cmd/archive-node`, `cmd/tier-node`, `cmd/plasma-node`, `cmd/downloader`): storage and file delivery only.
- shared libraries (`internal/errutil`, `internal/logger`, `internal/config`): cross-service infra.

## Component Status
- `cmd/web-adapter` is a debug-only component.
- It is allowed for manual diagnostics/regression experiments.
- It is not part of the production entrypoint architecture.
- Production HTTP entrypoint is `cmd/gateway`.

## Required CI Checks
The minimum required checks are:
- `make proto-lint`
- `make proto-breaking`
- `make architecture-check`
- `make docs-check`
- `make test-unit`
- `make test-integration`
- `make test-scala`

Recommended aggregate gate: `make ci-check`.

## Forbidden Practices
- raw logging via direct `log.Printf` / ad-hoc stdout in production handlers instead of shared logger.
- filesystem writes/reads in gateway request handlers.
- importing `internal/downloads` from gateway code.
- bypassing `errutil` mapping for transport-facing errors.
- runtime Dockerfiles compiling sources (`RUN go build`, `RUN npm`) instead of copying host-built artifacts.
- breaking proto contracts without versioned migration process.

## Asynchronous Processing
- all background tasks initiated by gateway (cache warmup, IPFS link generation, CDN upload, and similar jobs) must run asynchronously via NATS.
- gateway request handlers must not execute such workloads via inline goroutines tied to request lifetime.
- async commands must carry `trace_id` and be observable in logs/metrics end-to-end.

## References
- [TRACE.md](TRACE.md)
- [API_ERROR_MAPPING.md](API_ERROR_MAPPING.md)
- [ENGINEERING_STANDARD.md](ENGINEERING_STANDARD.md)
- [PROTO_IMMUTABILITY.md](PROTO_IMMUTABILITY.md)
