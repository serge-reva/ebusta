# Trace Viewer

`trace-viewer` is a small service and CLI for looking up all stage logs related to a single `trace_id`.

It sits on top of Loki and reads container logs that already contain `trace_id` in the log line.

## Components

- `loki`: central log storage
- `grafana`: UI for raw log exploration
- `trace-viewer`: focused API/UI/CLI for one-request tracing

Stage ports:

- Loki API: `http://yuro.local:3100`
- Grafana UI: `http://yuro.local:3300`
- Trace Viewer UI/API: `http://yuro.local:8089`

## Runtime Prerequisite

Docker logging plugin must be installed on the host before starting the stack:

```bash
docker plugin install grafana/loki-docker-driver:latest --alias loki --grant-all-permissions
docker plugin ls
```

The plugin is installed automatically by `infra/ansible/playbooks/deploy_internal.yml` if it is missing.

## Docker Compose

The compose stack includes:

- Loki with config from `deploy/loki/loki-config.yml`
- Grafana with datasource provisioning from `deploy/grafana/provisioning/datasources/loki.yml`
- Trace viewer served from `cmd/trace-viewer`

All application services use the Docker Loki logging driver and push logs to:

```text
http://localhost:3100/loki/api/v1/push
```

This must point to `localhost` because the Docker logging driver runs on the host.

## Trace Viewer HTTP API

### Health

```bash
curl -fsS http://localhost:8089/health
```

Expected response:

```text
ok
```

### Query Logs By Trace ID

```bash
curl -fsS http://localhost:8089/trace/gw-1772922568796072643 | jq
```

Response shape:

```json
{
  "trace_id": "gw-1772922568796072643",
  "count": 3,
  "entries": [
    {
      "timestamp": "2026-03-07T19:29:28.797Z",
      "level": "INFO",
      "service": "orchestrator",
      "message": "search request trace_id=gw-1772922568796072643 query=tolstoy",
      "error": "",
      "labels": {
        "compose_service": "orchestrator"
      },
      "raw": "..."
    }
  ]
}
```

### Web UI

Open:

```text
http://localhost:8089/
```

Enter a `trace_id` and the page will render all matched log lines in time order.

## CLI Usage

Build:

```bash
make build-trace-viewer
```

Query as a table:

```bash
./bin/trace-viewer get gw-1772922568796072643
```

Query as JSON:

```bash
./bin/trace-viewer get --json gw-1772922568796072643
```

Override Loki URL or selector:

```bash
./bin/trace-viewer get \
  --loki-url http://127.0.0.1:3100 \
  --selector '{compose_project="ebusta"}' \
  gw-1772922568796072643
```

Run the HTTP server manually:

```bash
./bin/trace-viewer --loki-url http://127.0.0.1:3100 --port 8080
```

## Grafana

Grafana is provisioned with Loki as a datasource.

Open:

```text
http://localhost:3300
```

Use Explore to run raw LogQL queries, for example:

```logql
{compose_project="ebusta"} |= "gw-1772922568796072643"
```

## Stage Deployment

Deploy internal services:

```bash
cd infra/ansible
ansible-playbook -i inventory/stage/hosts.yml playbooks/deploy_internal.yml --become --ask-become-pass
```

After deploy, verify:

```bash
ssh yuro.local 'cd /opt/ebusta && docker compose ps'
ssh yuro.local 'curl -fsS http://127.0.0.1:3100/ready'
ssh yuro.local 'curl -fsS http://127.0.0.1:3300/api/health'
ssh yuro.local 'curl -fsS http://127.0.0.1:8089/health'
```

## Known Limits

- `trace-viewer` currently filters by `trace_id` in the log line, not by a dedicated Loki label.
- If some service stops including `trace_id` in log text, those lines will not appear in results.
- The query limit is currently `5000` log lines per request.
