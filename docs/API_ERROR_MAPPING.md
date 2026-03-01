# API Error Mapping

Version: 1.0  
Last Updated: 2026-03-01

Canonical mapping source: `internal/errutil/codes.go`.

## Mapping Table

| Condition | gRPC Code | Internal Code (`errutil`) | Retryable |
|---|---|---|---|
| Invalid client input, malformed request, validation failure | `InvalidArgument` | `INVALID_ARGUMENT` | `false` |
| Requested resource not found | `NotFound` | `NOT_FOUND` | `false` |
| Missing authentication | `Unauthenticated` | `UNAUTHORIZED` | `false` |
| Permission denied | `PermissionDenied` | `FORBIDDEN` | `false` |
| Downstream/service unavailable | `Unavailable` | `SERVICE_UNAVAILABLE` | `true` |
| Timeout/deadline exceeded | `DeadlineExceeded` | `TIMEOUT` | `true` |
| Upstream gateway/proxy failure | `Internal` or translated HTTP 502 path | `BAD_GATEWAY` / `UPSTREAM_ERROR` | `true` |
| Internal unexpected failure | `Internal` | `INTERNAL_ERROR` | `false` |
| Request payload too large | usually HTTP only | `REQUEST_TOO_LARGE` | `false` |
| Downloader/storage specific failure | service-specific | `DOWNLOADER_ERROR` / `ZIP_ERROR` / `SEARCH_ERROR` | depends on cause |

## Rules
- Every error response must include `trace_id`.
- HTTP error envelope must be stable: `{"error": {"code", "message", "trace_id", ...}}`.
- gRPC-facing errors should be created via `errutil.ToGRPCError` from `AppError`.
- New internal error codes are `append-only` (additive); do not rename or repurpose existing codes.
- Retry policy defaults to `false`; enable retries only for transient categories (`SERVICE_UNAVAILABLE`, `TIMEOUT`, selected upstream failures).

## Change Policy
- Any mapping change requires doc update in this file and corresponding code update in `internal/errutil/`.
- Contract-breaking error semantics are not allowed without explicit review.

## References
- `internal/errutil/codes.go`
- `internal/errutil/grpc.go`
- `internal/errutil/http.go`
- [TRACE.md](TRACE.md)
