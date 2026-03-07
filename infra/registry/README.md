# Stage Prerequisites: Private Registry and Host Docker Setup

This document contains the exact prerequisite commands to run before Stage deployment.

## 1) yuro.local: start private registry

```bash
sudo mkdir -p /opt/registry/data
sudo docker rm -f registry || true
sudo docker run -d \
  -p 5000:5000 \
  --restart=always \
  --name registry \
  -v /opt/registry/data:/var/lib/registry \
  registry:2

curl --noproxy '*' http://yuro.local:5000/v2/_catalog
```

Expected response:

```json
{"repositories":[]}
```

## 2) yuro.local + neptune.local: configure insecure registry

`/etc/docker/daemon.json`:

```json
{
  "insecure-registries": ["yuro.local:5000"]
}
```

Apply:

```bash
sudo systemctl restart docker
sudo systemctl status docker --no-pager
```

Quick connectivity check:

```bash
docker pull yuro.local:5000/registry:2
# if image not present in registry, error should be "not found", not "connection refused"
```

## 3) Ensure Docker + Compose installed

```bash
docker --version
docker compose version
```

If missing (Ubuntu/Debian):

```bash
sudo apt-get update
sudo apt-get install -y ca-certificates curl gnupg
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] \
  https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo $VERSION_CODENAME) stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
```

## 4) Build and push Ebusta images to registry (run from repo root)

```bash
SERVICES=(
  gateway web-frontend datamanager dsl-scala query-builder
  orchestrator archive-node tier-node plasma-node downloader
)

for s in "${SERVICES[@]}"; do
  docker build -t "yuro.local:5000/ebusta-${s}:latest" -f "deploy/Dockerfile.${s}" .
  docker push "yuro.local:5000/ebusta-${s}:latest"
done

curl --noproxy '*' http://yuro.local:5000/v2/_catalog
```

## 5) Verify pull from neptune.local

```bash
docker pull yuro.local:5000/ebusta-gateway:latest
docker rmi yuro.local:5000/ebusta-gateway:latest
```

## Notes
- If corporate proxy is enabled, use `--noproxy '*'` for local/internal endpoints.
- Stage deployment playbooks should only run after all checks above are green.
