# Async Processing With NATS

Version: 1.0  
Last Updated: 2026-03-06

## Goal
Provide asynchronous execution of background tasks (cache warmup, IPFS/torrent generation, CDN upload) without blocking user-facing HTTP responses.

## Why NATS
NATS is selected as the default async transport for Ebusta because it gives low operational overhead and predictable latency for command-style messaging.

### Comparison Summary
- NATS:
  - simple deployment and operations footprint
  - very low latency for request-triggered background commands
  - native subject-based routing
  - JetStream support for persistence and replay when needed
  - supports request/reply pattern for lightweight RPC
- RabbitMQ:
  - rich queueing features and routing patterns
  - heavier operational model for current Ebusta needs
- Kafka:
  - strong event log capabilities for very high-throughput stream processing
  - higher complexity and infra cost than needed for command fan-out and workers
- Macula (internal/alternative bus option):
  - possible fit for specific workflows
  - lower standardization and ecosystem/tooling alignment for current Ebusta roadmap

## Target Components
- NATS server:
  - separate container/service
  - JetStream enabled where delivery guarantees are required
- Gateway:
  - publishes background commands via `CommandBus`
  - does not execute long-running background workflows inline in request handlers
- Preloader service:
  - subscribes to `preload.book`
  - performs HEAD request to downloader to warm storage/cache chain
- Future workers:
  - IPFS publisher
  - torrent/metadata packager
  - CDN uploader

## High-Level Flow
1. User requests `GET /api/book/{sha1}` (or equivalent flow that needs optional warmup).
2. Gateway returns metadata/response to user immediately.
3. Gateway publishes `preload.book` command to NATS asynchronously.
4. Preloader consumes command and executes HEAD call to downloader.
5. Preloader logs result with `trace_id` for correlation.

## Command Envelope
All commands use a common JSON envelope:

```json
{
  "type": "preload.book",
  "payload": {
    "sha1": "<sha1>"
  },
  "trace_id": "<trace-id>"
}
```

## Configuration
Add async section in runtime config:

```yaml
async:
  enabled: true
  nats_url: "nats://nats:4222"
```

Recommended extensions for production:

```yaml
async:
  enabled: true
  nats_url: "nats://nats:4222"
  subject_prefix: "ebusta"
  jetstream:
    enabled: true
    stream: "EBUSTA_COMMANDS"
    retention: "limits"
```

## Error Handling
- Publish failures in gateway:
  - do not break user response path for non-critical background tasks
  - log with `trace_id` and command type
- Consumer failures (preloader):
  - log error and context (`sha1`, `trace_id`)
  - optional bounded retry policy
  - avoid unbounded retry loops

## Design Rules
- Gateway handlers must stay short-lived and non-blocking.
- Async work must be idempotent where possible.
- Trace propagation is mandatory (`trace_id` in command envelope).
- Command payloads must be versionable and backward compatible.
