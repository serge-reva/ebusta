#!/usr/bin/env bash
set -euo pipefail

REGISTRY_HOST="yuro.local"
REGISTRY_PORT="5000"
REGISTRY_ADDR="${REGISTRY_HOST}:${REGISTRY_PORT}"
REGISTRY_DATA_DIR="/opt/registry/data"

echo "[1/6] Checking docker availability"
docker --version
docker compose version

echo "[2/6] Writing /etc/docker/daemon.json with insecure registry ${REGISTRY_ADDR}"
TMP_JSON="$(mktemp)"
cat > "${TMP_JSON}" <<JSON
{
  "insecure-registries": ["${REGISTRY_ADDR}"]
}
JSON
sudo install -m 0644 "${TMP_JSON}" /etc/docker/daemon.json
rm -f "${TMP_JSON}"

echo "[3/6] Restarting docker"
sudo systemctl restart docker
sudo systemctl --no-pager --full status docker | sed -n '1,20p'

echo "[4/6] Ensuring registry data directory"
sudo mkdir -p "${REGISTRY_DATA_DIR}"
sudo chown root:root "${REGISTRY_DATA_DIR}"

echo "[5/6] Starting registry container"
docker rm -f registry >/dev/null 2>&1 || true
docker run -d \
  -p ${REGISTRY_PORT}:5000 \
  --restart=always \
  --name registry \
  -v ${REGISTRY_DATA_DIR}:/var/lib/registry \
  registry:2

echo "[6/6] Registry smoke checks"
curl --noproxy '*' -sS "http://${REGISTRY_HOST}:${REGISTRY_PORT}/v2/_catalog" || true
curl --noproxy '*' -sS "http://127.0.0.1:${REGISTRY_PORT}/v2/_catalog"
docker info --format '{{json .RegistryConfig.IndexConfigs}}' | grep -q '"yuro.local:5000"' \
  && echo "OK: insecure registry configured" \
  || (echo "ERROR: insecure registry not configured"; exit 1)

echo "DONE: yuro prerequisites configured"
