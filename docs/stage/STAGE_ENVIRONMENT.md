# Ebusta Stage Environment

Version: 1.0  
Last updated: 2026-03-04

## 1. Infrastructure Overview

Stage environment is split across dedicated hosts in a private network.

- `proxy` (`192.168.1.11`): internal DNS (`dnsmasq`) for `.ebusta` zone.
- `yuro.local` (`192.168.1.179`): private Docker registry and main service host.
- `neptune.local` (`192.168.1.140`): archive host (`archive-node`) with FB2 mount.
- `dev2` (operator host): control node used for build/push and Ansible runs.

## 2. Service Placement

### proxy (`192.168.1.11`)
- `dnsmasq` (internal DNS for `.ebusta`)

### yuro.local (`192.168.1.179`)
- `gateway`
- `web-frontend`
- `datamanager`
- `dsl-scala`
- `query-builder`
- `orchestrator`
- `downloader`
- `tier-node`
- `plasma-node`
- Docker registry: `yuro.local:5000`

### neptune.local (`192.168.1.140`)
- `archive-node`
- Host mount for books:
  - `/mnt/raid0/disk4/Библиотека Flibusta (только FB2)/fb2.Flibusta.Net` -> container `/fb2`

## 3. Network and DNS

- Internal DNS server: `192.168.1.11`
- DNS zone: `.ebusta`

Current service DNS names (from `infra/ansible/inventory/stage/group_vars/all.yml` and stage templates):

| DNS name | IP | Note |
|---|---:|---|
| `gateway.ebusta` | `192.168.1.179` | deployed on yuro |
| `web-frontend.ebusta` | `192.168.1.179` | deployed on yuro |
| `datamanager.ebusta` | `192.168.1.179` | deployed on yuro |
| `dsl-scala.ebusta` | `192.168.1.179` | deployed on yuro |
| `query-builder.ebusta` | `192.168.1.179` | deployed on yuro |
| `orchestrator.ebusta` | `192.168.1.179` | deployed on yuro |
| `downloader.ebusta` | `192.168.1.179` | deployed on yuro |
| `tier-node.ebusta` | `192.168.1.179` | deployed on yuro |
| `plasma-node.ebusta` | `192.168.1.179` | deployed on yuro |
| `archive-node.ebusta` | `192.168.1.140` | deployed on neptune |
| `auth-manager.ebusta` | `192.168.1.179` | referenced in config, not in current stage compose |

## 4. Service Ports

Ports are mapped 1:1 host<->container in current stage compose templates.

| Service | Container port | Host port | Host | Note |
|---|---:|---:|---|---|
| `gateway` | 8443 | 8443 | yuro | HTTP API |
| `web-frontend` | 3000 | 3000 | yuro | UI |
| `downloader` | 50081 | 50081 | yuro | HTTP download/meta |
| `datamanager` | 50051 | 50051 | yuro | gRPC |
| `dsl-scala` | 50052 | 50052 | yuro | gRPC |
| `query-builder` | 50053 | 50053 | yuro | gRPC |
| `orchestrator` | 50054 | 50054 | yuro | gRPC |
| `tier-node` | 50111 | 50111 | yuro | gRPC |
| `plasma-node` | 50112 | 50112 | yuro | gRPC |
| `archive-node` | 50110 | 50110 | neptune | gRPC |
| Docker registry | 5000 | 5000 | yuro | private registry |

## 5. Configuration and Templates

Repository sources:
- `infra/templates/ebusta.yaml.j2`
- `infra/templates/docker-compose.internal.yml.j2`
- `infra/templates/docker-compose.archive.yml.j2`
- `infra/ansible/inventory/stage/hosts.yml`
- `infra/ansible/inventory/stage/group_vars/all.yml`
- `infra/ansible/inventory/stage/host_vars/neptune.yml`

Rendered files on hosts:
- `/opt/ebusta/ebusta.yaml`
- `/opt/ebusta/docker-compose.yml`

Persistent host data paths:
- `neptune`: `/opt/ebusta`, plus archive source mount path above
- `yuro`: `/opt/ebusta`, `/opt/ebusta/tier_data`

## 6. Deployment Process (from dev2)

### 6.1 Build and push images to registry

From repository root:

```bash
cd ~/projects/ebusta
make build
./infra/scripts/ops/stage-prereqs/build_and_push_images.sh
```

Manual equivalent (example):

```bash
docker build -t yuro.local:5000/ebusta-gateway:latest -f deploy/Dockerfile.gateway .
docker push yuro.local:5000/ebusta-gateway:latest
```

### 6.2 Deploy archive host

```bash
cd ~/projects/ebusta/infra/ansible
ansible-playbook -i inventory/stage/hosts.yml playbooks/deploy_archive.yml --ask-become-pass
```

### 6.3 Deploy internal host

```bash
cd ~/projects/ebusta/infra/ansible
ansible-playbook -i inventory/stage/hosts.yml playbooks/deploy_internal.yml --ask-become-pass
```

Or full run:

```bash
cd ~/projects/ebusta/infra/ansible
ansible-playbook -i inventory/stage/hosts.yml playbooks/site.yml --ask-become-pass
```

## 7. Health and Functional Checks

### 7.1 Container health

On `yuro.local`:

```bash
ssh yuro.local
cd /opt/ebusta
docker compose ps
```

On `neptune.local`:

```bash
ssh neptune.local
cd /opt/ebusta
docker compose ps
```

### 7.2 Gateway search

```bash
curl -X POST http://yuro.local:8443/search \
  -H "Content-Type: application/json" \
  -d '{"query":"tolstoy","page":1,"limit":5}'
```

### 7.3 Downloader check by SHA1

```bash
curl -I http://yuro.local:50081/books/bd65259056f6c1daf5e8489dcbd05bfc42a85135
curl -O http://yuro.local:50081/books/bd65259056f6c1daf5e8489dcbd05bfc42a85135
```

### 7.4 Archive node health probe

```bash
ssh neptune.local
cd /opt/ebusta
docker compose exec -T archive-node /app/grpc_health_probe -addr=localhost:50110
```

## 8. Typical Operations

### Stop all services

```bash
# yuro
ssh yuro.local 'cd /opt/ebusta && docker compose down'

# neptune
ssh neptune.local 'cd /opt/ebusta && docker compose down'
```

### Update one service

1. Rebuild image.
2. Push to `yuro.local:5000`.
3. Re-run corresponding playbook (`deploy_internal.yml` or `deploy_archive.yml`).

### Logs

```bash
# on target host
cd /opt/ebusta
docker compose logs -f <service>
```

### DNS diagnostics from container

```bash
# example on yuro
cd /opt/ebusta
docker compose exec -T gateway nslookup orchestrator.ebusta
```

## 9. Known Issues and Fixes

- Archive source mount path mismatch:
  - Symptom: `archive-node` starts but cannot find books.
  - Fix: ensure `archive_node_zip_root` is set in `infra/ansible/inventory/stage/host_vars/neptune.yml`.

- DNS resolution failures inside containers:
  - Symptom: services cannot resolve `*.ebusta`.
  - Fix: verify `dns: 192.168.1.11` in compose templates and DNS records on `proxy`.

- Registry pull failures:
  - Symptom: image pull errors from `yuro.local:5000`.
  - Fix: check Docker `insecure-registries` on hosts and registry container health on yuro.

- Ansible deploy prompts for privilege escalation:
  - `--ask-become-pass` is required unless passwordless sudo is configured.

## 10. Related Documents

- `docs/ARCHITECTURAL_CONSTITUTION.md`
- `docs/DEPLOYMENT_ARCHITECTURE.md`
- `docs/RUNBOOK.md`
