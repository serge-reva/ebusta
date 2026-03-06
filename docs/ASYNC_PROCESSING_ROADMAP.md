# Async Processing Roadmap

Version: 1.0  
Last Updated: 2026-03-06

## Objective
Introduce NATS-based asynchronous processing into Ebusta incrementally, without breaking existing synchronous behavior.

## Delivery Phases

| Phase | Priority | Scope | Expected Result | Effort |
|---|---|---|---|---|
| 1 | P0 | Infrastructure: add NATS with JetStream to docker-compose/runtime | NATS available in local/stage environments | 0.5-1 day |
| 2 | P0 | `CommandBus` abstraction in gateway + `NatsBus` implementation | Gateway can publish async commands behind interface | 1-2 days |
| 3 | P0 | `PreloadBook` command publication from gateway flow | Gateway publishes preload command after response path | 1 day |
| 4 | P0 | `cmd/preloader` worker consuming `preload.book` | Preloader performs downloader HEAD warmup asynchronously | 1-2 days |
| 5 | P1 | Config model and validation (`async.enabled`, `async.nats_url`) | Controlled feature toggling and explicit config errors | 0.5-1 day |
| 6 | P1 | Operational hardening via JetStream (durable consumers, replay strategy) | Better delivery guarantees and recovery | 1-2 days |
| 7 | P2 | Extend with new command types (IPFS, torrent, CDN) | Reusable async platform for background jobs | 2-5 days |

## Stage Details

### Phase 1: NATS Infrastructure
- Add `nats` service to compose/deploy manifests.
- Enable JetStream (`-js`) for optional persistence.
- Define health checks and startup dependency rules.

### Phase 2: CommandBus Abstraction
- Introduce `CommandBus` interface in gateway domain layer.
- Add concrete `NatsBus` implementation.
- Keep handler code independent from specific broker library.

### Phase 3: PreloadBook Command
- Define command schema and envelope (`type`, `payload`, `trace_id`).
- Publish command in relevant gateway endpoint after primary response logic.
- Make failures non-blocking for user response path.

### Phase 4: Preloader Service
- Add `cmd/preloader` Go service.
- Subscribe to `preload.book`.
- Perform HEAD to downloader and log success/failure with trace.

### Phase 5: Configuration and Validation
- Add `async` config section to `ebusta.yaml` and templates.
- Validate `nats_url` when `async.enabled=true`.
- Support feature-flagged rollout.

### Phase 6: Reliability Enhancements
- Configure JetStream stream and durable consumer.
- Add bounded retries / dead-letter strategy if needed.
- Add observability counters for publish/consume/fail/retry.

### Phase 7: Functional Expansion
- Introduce additional commands:
  - `ipfs.pin`
  - `torrent.generate`
  - `cdn.upload`
- Keep commands independently deployable and horizontally scalable.

## Acceptance Criteria Per Milestone
- Publish path tested and traceable (`trace_id` present end-to-end).
- Consumer idempotency documented.
- Failure behavior explicit and observable.
- No blocking behavior introduced into gateway request path.

## Risks and Mitigations
- Risk: hidden coupling between gateway and worker payloads.
  - Mitigation: versioned command envelope and compatibility tests.
- Risk: retry storms under downstream instability.
  - Mitigation: bounded retries + backoff + alerting.
- Risk: async failures ignored in operations.
  - Mitigation: metrics/log dashboards and SLO checks.
