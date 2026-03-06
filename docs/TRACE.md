# Trace Propagation Standard

Version: 1.0  
Last Updated: 2026-03-01

This document defines mandatory trace propagation behavior across `ebusta` services.

## Core Rules
- Every request must have a trace id.
- Accept incoming trace id if provided.
- Generate one if missing using `errutil.GenerateTraceID(prefix)`.
- Propagate trace id to every downstream call.
- Include trace id in logs and in error responses.

## Protocol Conventions
- HTTP inbound/outbound header: `X-Trace-Id`
- gRPC metadata key: `x-trace-id`

## Generation Format
Current helper behavior (`internal/errutil/trace.go`):
- format: `<prefix>-<unix_nano_timestamp>`
- example: `gw-1700000000000000000`

## Service Prefixes

| Service | Prefix |
|---|---|
| gateway | `gw` |
| orchestrator | `orch` |
| datamanager | `dm` |
| downloader | `dl` |
| archive-node | `arch` |
| tier-node | `tier` |
| plasma-node | `plasma` |
| web-frontend | `wf` |
| auth-manager | `auth` |
| irc-adapter | `irc` |
| json-gateway | `tg` |
| cli | `cli` |

## Mandatory Flow
- HTTP server:
  - read `X-Trace-Id`
  - generate if absent
  - set `X-Trace-Id` in response
  - pass into downstream context/headers
- gRPC server:
  - read `x-trace-id` from incoming metadata (`errutil.TraceIDFromContext`)
  - generate if absent
  - use in logs and propagated context
- gRPC client:
  - set outgoing metadata (`errutil.ContextWithTraceID`)
- HTTP client:
  - set outgoing header `X-Trace-Id`

## Logging Requirement
- All request-scoped logs must include `trace_id` as a structured field.
- Transport errors and business errors must carry the same trace id observed/generated at request ingress.

## References
- `internal/errutil/trace.go`
- `internal/logger/context.go`
- [API_ERROR_MAPPING.md](API_ERROR_MAPPING.md)
- [ARCHITECTURAL_CONSTITUTION.md](ARCHITECTURAL_CONSTITUTION.md)
