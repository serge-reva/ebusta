# Loki Logging
Version: 1.0  
Last Updated: 2026-03-01

## Overview
The Docker stack includes centralized log collection with Loki + Grafana.

- Loki endpoint: `http://localhost:3100`
- Grafana UI: `http://localhost:3300`
- Default Grafana credentials: `admin` / `admin` (change on first login)

## Prerequisite: Docker Loki Logging Driver
`docker-compose.yml` uses Docker logging driver `loki` for application services.

Install once on host:

```bash
docker plugin install grafana/loki-docker-driver:latest --alias loki --grant-all-permissions
```

Verify:

```bash
docker plugin ls
```

Expected entry includes `loki` and `ENABLED=true`.

## Start
```bash
make docker-up
```

## Verify
1. Open Grafana: `http://localhost:3300`
2. Go to Explore.
3. Select datasource `Loki` (auto-provisioned).
4. Run query:

```logql
{compose_project="ebusta"}
```

You can narrow by service:

```logql
{compose_service="gateway"}
```

## Notes
- If Loki driver is not installed, container startup with `logging.driver: loki` will fail.
- Grafana datasource is provisioned from `deploy/grafana/provisioning/datasources/loki.yml`.
