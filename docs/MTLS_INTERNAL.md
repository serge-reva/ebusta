# Internal gRPC mTLS

Version: 1.0  
Last Updated: 2026-03-02  
Status: Implemented

## Scope
Internal gRPC connections in Ebusta are protected with mutual TLS (mTLS):

- `datamanager` (`50051`)
- `dsl-scala` (`50052`)
- `query-builder` (`50053`)
- `orchestrator` (`50054`)
- `auth-manager` (`50055`)
- `archive-node` (`50110`)
- `tier-node` (`50111`)
- `plasma-node` (`50112`)
- gRPC clients in `gateway`, `orchestrator`, `downloader`, `tier-node`, `plasma-node`

## Certificate Layout
Certificates are generated into `certs/`:

- `certs/ca.crt`
- `certs/ca.key`
- `certs/<service>/<service>.crt`
- `certs/<service>/<service>.key`

Service names used by generator:

- `datamanager`, `dsl-scala`, `query-builder`, `orchestrator`, `auth-manager`
- `archive-node`, `tier-node`, `plasma-node`, `downloader`, `gateway`

## Generation
Use:

```bash
make certs-generate
```

or directly:

```bash
./scripts/gen-certs.sh
```

## Runtime Wiring
- Docker compose mounts certs as read-only volume: `./certs:/certs:ro`.
- `deploy/ebusta.docker.yaml` enables mTLS for internal gRPC services and points to `/certs/...`.
- gRPC health checks use TLS flags (`grpc_health_probe -tls ...`) so health status reflects real mTLS connectivity.

## Validation
1. Generate certificates: `make certs-generate`.
2. Build artifacts/images: `make build && make docker-build`.
3. Start stack: `make docker-up`.
4. Check status: `make docker-status` (gRPC services should become `healthy`).
5. Verify API path through gateway (example):
   - `curl -i http://localhost:8443/health`
6. Stop stack: `make docker-down`.

## Failure Mode Check
If cert volume is missing or invalid, internal gRPC connections and gRPC health checks fail by design.
