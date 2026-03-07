#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ANSIBLE_DIR="${SCRIPT_DIR}/../../ansible"

cd "${ANSIBLE_DIR}"
ansible-playbook -i inventory/stage/hosts.yml playbooks/site.yml
