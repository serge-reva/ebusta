# Ebusta Deployment Architecture
Version: 1.0  
Last Updated: 2026-03-02

## Purpose
Define a scalable repository layout for deployment assets of Ebusta in a distributed private network (`*.ebusta`), with Ansible-driven operations, private registry, mTLS, and multi-environment support (`dev/staging/prod`).

## Design Principles
- Separate **application source code** from **infrastructure/deployment assets**.
- Use standard Ansible conventions (`inventory`, `group_vars`, `roles`, `playbooks`).
- Keep environment-specific data isolated and explicit.
- Separate host roles (`public` vs `internal`) in inventory and playbooks.
- Keep secrets in Ansible Vault only.
- Make adding a new service/group a local change (role + vars), not a global refactor.

## Proposed Root Structure

```text
.
├── infra/
│   ├── ansible/
│   │   ├── ansible.cfg
│   │   ├── requirements.yml
│   │   ├── inventory/
│   │   │   ├── dev/
│   │   │   │   ├── hosts.yml
│   │   │   │   ├── group_vars/
│   │   │   │   │   ├── all.yml
│   │   │   │   │   ├── all.vault.yml
│   │   │   │   │   ├── public.yml
│   │   │   │   │   ├── internal.yml
│   │   │   │   │   ├── registry.yml
│   │   │   │   │   └── monitoring.yml
│   │   │   │   └── host_vars/
│   │   │   │       └── <host>.yml
│   │   │   ├── staging/
│   │   │   │   └── ... (same layout)
│   │   │   └── prod/
│   │   │       └── ... (same layout)
│   │   ├── playbooks/
│   │   │   ├── site.yml
│   │   │   ├── bootstrap.yml
│   │   │   ├── deploy_public.yml
│   │   │   ├── deploy_internal.yml
│   │   │   ├── deploy_registry.yml
│   │   │   ├── deploy_monitoring.yml
│   │   │   ├── rotate_certs.yml
│   │   │   └── rollback.yml
│   │   ├── roles/
│   │   │   ├── common_base/
│   │   │   ├── docker_runtime/
│   │   │   ├── ebusta_config/
│   │   │   ├── ebusta_public_stack/
│   │   │   ├── ebusta_internal_stack/
│   │   │   ├── private_registry/
│   │   │   ├── mtls_certs/
│   │   │   ├── internal_dns/
│   │   │   ├── monitoring_prometheus/
│   │   │   └── logging_loki/
│   │   └── collections/
│   │       └── requirements.yml
│   ├── templates/
│   │   ├── ebusta.yaml.j2
│   │   ├── docker-compose.public.yml.j2
│   │   ├── docker-compose.internal.yml.j2
│   │   ├── docker-compose.registry.yml.j2
│   │   ├── docker-compose.monitoring.yml.j2
│   │   ├── prometheus.yml.j2
│   │   ├── loki-config.yml.j2
│   │   └── dns/
│   │       ├── db.ebusta.j2
│   │       └── named.conf.local.j2
│   ├── scripts/
│   │   ├── certs/
│   │   │   ├── gen-certs.sh
│   │   │   └── verify-certs.sh
│   │   ├── dns/
│   │   │   ├── check-zone.sh
│   │   │   └── render-zone.sh
│   │   ├── registry/
│   │   │   ├── login.sh
│   │   │   └── push-images.sh
│   │   └── ops/
│   │       ├── deploy.sh
│   │       ├── rollback.sh
│   │       └── smoke-check.sh
│   ├── registry/
│   │   ├── config.yml
│   │   └── htpasswd.example
│   ├── secrets/
│   │   ├── README.md
│   │   ├── dev/
│   │   │   └── vault.yml
│   │   ├── staging/
│   │   │   └── vault.yml
│   │   └── prod/
│   │       └── vault.yml
│   └── docs/
│       ├── DEPLOYMENT_RUNBOOK.md
│       ├── INVENTORY_MODEL.md
│       ├── CERT_ROTATION.md
│       ├── DNS_MODEL.md
│       └── INCIDENT_RESPONSE.md
├── deploy/
│   └── (existing Dockerfiles and local compose assets, kept for developer/runtime loop)
└── docs/
    └── (project architecture docs)
```

## Key Folder Responsibilities

| Path | Responsibility |
|---|---|
| `infra/ansible/inventory/<env>/` | Environment-specific inventory (`dev/staging/prod`) |
| `infra/ansible/inventory/<env>/group_vars/` | Group-level configuration (`public/internal/registry/monitoring`) |
| `infra/ansible/inventory/<env>/host_vars/` | Host-specific overrides (IP, disk paths, labels) |
| `infra/ansible/playbooks/` | Entry points for deployment workflows |
| `infra/ansible/roles/` | Reusable deployment logic blocks |
| `infra/templates/` | Jinja2 templates for rendered runtime configs |
| `infra/scripts/` | Operational helper scripts (certs, DNS, registry, deploy wrappers) |
| `infra/registry/` | Private registry static config and examples |
| `infra/secrets/` | Vault-encrypted secret files by environment |
| `infra/docs/` | Infra-specific operational docs/runbooks |

## Inventory Model (Example)

```yaml
# infra/ansible/inventory/prod/hosts.yml
all:
  children:
    public:
      hosts:
        gw-1.ebusta:
        web-1.ebusta:
        tg-1.ebusta:
    internal:
      hosts:
        orch-1.ebusta:
        dm-1.ebusta:
        dsl-1.ebusta:
        qb-1.ebusta:
        dl-1.ebusta:
        archive-1.ebusta:
        tier-1.ebusta:
        plasma-1.ebusta:
        irc-1.ebusta:
    registry:
      hosts:
        yuro.local:
    monitoring:
      hosts:
        mon-1.ebusta:
```

## Playbook Strategy
- `site.yml`: full deployment orchestrator (imports partial playbooks).
- `bootstrap.yml`: base OS setup, Docker runtime, users, firewall baseline.
- `deploy_public.yml`: deploy `gateway`, `web-frontend`, `telegram-adapter`.
- `deploy_internal.yml`: deploy internal services stack.
- `deploy_registry.yml`: manage private registry on `yuro.local`.
- `deploy_monitoring.yml`: Prometheus/Loki/Grafana stack.
- `rotate_certs.yml`: cert replacement + rolling restart.
- `rollback.yml`: revert to last stable image tags/config snapshot.

## Templates Strategy
- Keep one canonical `ebusta.yaml.j2` and parameterize by env/group vars.
- Render separate compose files per host role:
  - `docker-compose.public.yml.j2`
  - `docker-compose.internal.yml.j2`
  - `docker-compose.monitoring.yml.j2`
- Keep DNS/monitoring templates isolated (`templates/dns`, `prometheus.yml.j2`, `loki-config.yml.j2`).

## Secrets and Security
- No plaintext secrets in `group_vars/*.yml`.
- Store secrets only in Vault files (`all.vault.yml` or `secrets/<env>/vault.yml`).
- Recommended split:
  - non-sensitive vars: `group_vars/*.yml`
  - sensitive vars: `group_vars/*.vault.yml` (encrypted)
- mTLS cert distribution handled via dedicated `mtls_certs` role + strict file permissions.

## Scalability Rules
- New service: add role tasks + template fragment + group vars entry.
- New host group: add inventory group + optional `group_vars/<group>.yml` + playbook binding.
- New environment: copy `inventory/<env>` skeleton, set vars, reuse same roles/templates.

## Why This Approach
- Aligns with Ansible best practices and team familiarity.
- Preserves clear boundary between app code and infra automation.
- Supports progressive growth (new services, environments, observability stack) with minimal restructuring.
- Reduces operational risk via explicit inventory separation and vault-first secret handling.

## Relationship With Existing Repo Areas
- `deploy/` remains the source of runtime Dockerfiles and local compose assets.
- `infra/` becomes the deployment control-plane for distributed environments.
- `docs/` keeps product/architecture docs; `infra/docs/` keeps operations/deployment docs.

## Rollout Note
This is an architectural target layout only. Implementation can be phased:
1. `infra/ansible` skeleton and inventory
2. template migration (`ebusta.yaml.j2`, compose templates)
3. role extraction
4. cert/DNS/registry automation
5. monitoring automation
