# Stage Setup (Draft)

## Target Hosts
- `neptune.local` (`192.168.1.140`): archive-node only.
- `yuro.local` (`192.168.1.179`): datamanager, dsl-scala, query-builder, orchestrator, tier-node, plasma-node, downloader, gateway, web-frontend.

## DNS Assumptions
- Internal DNS server: `192.168.1.11`
- Zone: `.ebusta`
- Service records must resolve according to `inventory/stage/group_vars/all.yml`.

## Registry Assumptions
- Private registry: `yuro.local:5000`
- Image tag for stage: `latest`

## Paths
- Neptune:
  - config: `/opt/ebusta/ebusta.yaml`
  - archive sqlite: `/opt/ebusta/archive.meta.sqlite`
  - books mount: `/fb2`
- Yuro:
  - config: `/opt/ebusta/ebusta.yaml`
  - tier data: `/opt/ebusta/tier_data`
  - tier sqlite: `/opt/ebusta/tier.meta.sqlite`

## Deployment Entry Point
From `infra/ansible`:

```bash
ansible-playbook -i inventory/stage/hosts.yml playbooks/site.yml
```

Or via wrapper:

```bash
./infra/scripts/ops/deploy.sh
```

## Notes
- This stage setup is file-only bootstrap and can be refined before first production-like rollout.
- Secrets placeholders are kept in `inventory/stage/group_vars/all.vault.yml`.
