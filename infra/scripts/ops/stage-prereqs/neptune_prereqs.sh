#!/usr/bin/env bash
set -euo pipefail

REGISTRY_ADDR="yuro.local:5000"

echo "[1/4] Checking docker availability"
docker --version
docker compose version

echo "[2/4] Writing /etc/docker/daemon.json with insecure registry ${REGISTRY_ADDR}"
TMP_JSON="$(mktemp)"
cat > "${TMP_JSON}" <<JSON
{
  "insecure-registries": ["${REGISTRY_ADDR}"]
}
JSON
sudo install -m 0644 "${TMP_JSON}" /etc/docker/daemon.json
rm -f "${TMP_JSON}"

echo "[3/4] Restarting docker"
sudo systemctl restart docker
sudo systemctl --no-pager --full status docker | sed -n '1,20p'

echo "[4/4] Insecure registry smoke check"
docker info --format '{{json .RegistryConfig.IndexConfigs}}' | grep -q '"yuro.local:5000"' \
  && echo "OK: insecure registry configured" \
  || (echo "ERROR: insecure registry not configured"; exit 1)

echo "DONE: neptune prerequisites configured"
