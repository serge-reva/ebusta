#!/usr/bin/env bash
set -euo pipefail

REGISTRY="yuro.local:5000"
TAG="latest"

SERVICES=(
  gateway
  web-frontend
  datamanager
  dsl-scala
  query-builder
  orchestrator
  archive-node
  tier-node
  plasma-node
  downloader
)

echo "Building/pushing ${#SERVICES[@]} images to ${REGISTRY}"
for s in "${SERVICES[@]}"; do
  echo "=== ${s} ==="
  docker build -t "${REGISTRY}/ebusta-${s}:${TAG}" -f "deploy/Dockerfile.${s}" .
  docker push "${REGISTRY}/ebusta-${s}:${TAG}"
done

echo "Registry catalog:"
curl --noproxy '*' -sS "http://${REGISTRY}/v2/_catalog"

echo "DONE: build and push finished"
