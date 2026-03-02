#!/usr/bin/env bash
set -euo pipefail

CERTS_DIR="${1:-certs}"
DAYS="${DAYS:-3650}"

SERVICES=(
  "datamanager"
  "dsl-scala"
  "query-builder"
  "orchestrator"
  "auth-manager"
  "archive-node"
  "tier-node"
  "plasma-node"
  "downloader"
  "gateway"
)

mkdir -p "${CERTS_DIR}"

CA_KEY="${CERTS_DIR}/ca.key"
CA_CRT="${CERTS_DIR}/ca.crt"

if [[ ! -f "${CA_KEY}" || ! -f "${CA_CRT}" ]]; then
  echo "Generating root CA..."
  openssl genrsa -out "${CA_KEY}" 4096
  openssl req -x509 -new -nodes -key "${CA_KEY}" -sha256 -days "${DAYS}" \
    -subj "/C=US/O=Ebusta/CN=ebusta-internal-ca" \
    -out "${CA_CRT}"
else
  echo "Reusing existing CA at ${CA_CRT}"
fi

for svc in "${SERVICES[@]}"; do
  echo "Generating cert for ${svc}..."
  svc_dir="${CERTS_DIR}/${svc}"
  mkdir -p "${svc_dir}"

  key="${svc_dir}/${svc}.key"
  csr="${svc_dir}/${svc}.csr"
  crt="${svc_dir}/${svc}.crt"
  ext="${svc_dir}/${svc}.ext"

  cat > "${ext}" <<EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage=digitalSignature,keyEncipherment
extendedKeyUsage=serverAuth,clientAuth
subjectAltName=@alt_names
[alt_names]
DNS.1=${svc}
DNS.2=localhost
IP.1=127.0.0.1
EOF

  openssl genrsa -out "${key}" 2048
  openssl req -new -key "${key}" -subj "/C=US/O=Ebusta/CN=${svc}" -out "${csr}"
  openssl x509 -req -in "${csr}" -CA "${CA_CRT}" -CAkey "${CA_KEY}" -CAcreateserial \
    -out "${crt}" -days "${DAYS}" -sha256 -extfile "${ext}"

  rm -f "${csr}" "${ext}"
done

echo "Certificates generated in ${CERTS_DIR}"
