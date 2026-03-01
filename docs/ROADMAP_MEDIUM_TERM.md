# Ebusta Medium-Term Roadmap
Version: 1.0  
Last Updated: 2026-03-01

## Scope
This roadmap captures medium-term and long-term work deferred after completion of priority items 1–5.

## Priority Matrix
| ID | Task | Priority | Horizon | Complexity | Status |
|---|---|---|---|---|---|
| P6-1 | Startup config validation hardening | P6 | Next 1-2 months | Medium | Planned |
| P7-1 | Unify client access: web-frontend -> gateway | P7 | Next 1-3 months | Medium | Planned |
| P6-2 | SSRF protection in datamanager | P6 | Next 1-2 months | Medium | Planned |
| P6-3 | OpenAPI generation for gateway/downloader | P6 | Next 1-3 months | Medium | Planned |
| P9-1 | Gateway authentication (JWT/API keys) | P9 | Mid-term (2-4+ months) | High | Planned |
| P9-2 | mTLS for internal gRPC | P9 | Mid-term (2-4+ months) | High | Planned |
| LT-1 | Async download via object storage | Long-term | 4+ months | High | Optional |
| LT-2 | Simplify download hierarchy | Long-term | 4+ months | High | Optional |
| LT-3 | Distributed tracing with Jaeger | Long-term | 3+ months | Medium | Optional |

## Detailed Backlog

### P6: Improve startup config validation
**Motivation:** prevent startup with invalid config and reduce debugging time.

**Planned steps:**
- Define required config fields per service.
- Add `Validate()` methods to config structs where missing.
- Call validation in each `main()` right after loading config.
- Return actionable startup errors (field name + expected format).

**Expected outcome:** service fails fast with clear diagnostics if critical config is missing/invalid.

### P7: Unify client access (web-frontend via gateway)
**Motivation:** centralize policy enforcement (rate limiting, validation, metrics, auth), reduce duplicated access paths.

**Planned steps:**
- Replace direct orchestrator gRPC calls in `web-frontend` with HTTP requests to gateway search endpoint.
- Rework `web-frontend` handler flow to use gateway response contract.
- Remove (or narrow) `internal/search` usage where no longer needed.
- Update config: explicit gateway URL for web-frontend.

**Expected outcome:** all external traffic (except CLI) goes through gateway.

### P6: Add SSRF protection in datamanager
**Motivation:** avoid internal network access via malformed/malicious OpenSearch URL config.

**Planned steps:**
- Validate configured OpenSearch URL at startup.
- Add allowlist for trusted hosts/domains.
- Reject private/link-local/loopback target IP ranges unless explicitly allowed.
- Fail startup if target is outside trust policy.

**Expected outcome:** datamanager refuses untrusted OpenSearch targets.

### P6: Generate OpenAPI spec for gateway/downloader
**Motivation:** improve external integration and client generation workflow.

**Planned steps:**
- Add OpenAPI generation toolchain (`protoc-gen-openapi` or equivalent).
- Add `make openapi-generate` target.
- Generate and publish spec under `docs/api/`.
- Include in CI verification (spec freshness check).

**Expected outcome:** maintained `openapi.yaml` documenting public HTTP APIs.

### P9: Add gateway authentication
**Motivation:** protect public API from unauthorized use.

**Planned steps:**
- Select auth model (JWT or API key).
- Add auth middleware in gateway.
- Define token/key lifecycle (issue, revoke, rotate).
- Add auth-related error mapping and docs.

**Expected outcome:** only authorized clients can access gateway APIs.

### P9: Introduce mTLS for internal gRPC
**Motivation:** secure service-to-service communication against interception/spoofing.

**Planned steps:**
- Introduce CA and per-service certificates.
- Configure gRPC clients/servers for mutual TLS.
- Add certificate distribution strategy for Docker runtime.
- Add startup validation for certificate presence/validity.

**Expected outcome:** all internal gRPC links are mTLS-protected.

## Optional / Long-Term Items

### Asynchronous downloads via object storage
- Shift from synchronous chain retrieval to async jobs and object storage URLs.
- Best suited for heavy-scale traffic and large file workloads.

### Simplify download hierarchy
- Evaluate replacing multi-node chain with unified storage + HTTP Range support.
- Goal: reduce operational complexity and improve debuggability.

### Distributed tracing (Jaeger/OpenTelemetry)
- Add trace spans across gateway/orchestrator/datamanager/downloader and adapters.
- Enable cross-service latency and error-root-cause analysis.

## Completed in Prioritized Wave (for context)
- Proto immutability checks (buf).
- Graceful shutdown.
- Health checks.
- Metrics export.
- Centralized logs (Loki stack).
- E2E resilience/failure-recovery tests.

## Related Documents
- `docs/ARCHITECTURAL_CONSTITUTION.md`
- `docs/ENGINEERING_STANDARD.md`
- `docs/TRACE.md`
- `docs/API_ERROR_MAPPING.md`
- `docs/PROTO_IMMUTABILITY.md`
- `docs/LOGGING_LOKI.md`
