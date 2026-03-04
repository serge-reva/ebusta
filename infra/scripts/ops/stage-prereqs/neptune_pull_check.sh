#!/usr/bin/env bash
set -euo pipefail

IMAGE="yuro.local:5000/ebusta-gateway:latest"

echo "Pulling ${IMAGE}"
docker pull "${IMAGE}"

echo "Pulled successfully. Removing image"
docker rmi "${IMAGE}"

echo "DONE: neptune pull check passed"
