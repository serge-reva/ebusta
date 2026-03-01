#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

echo "🔥 Starting Stress Test: 100 requests (10 parallel) via gateway..."
START_TIME=$(date +%s%N)

for i in {1..100}; do
  curl -s -X POST "http://localhost:8443/search" \
    -H 'Content-Type: application/json' \
    -d '{"query":"author:Кинг","page":1,"limit":1}' > /dev/null &
  if (( $i % 10 == 0 )); then wait; fi
done

END_TIME=$(date +%s%N)
DIFF=$(( ($END_TIME - $START_TIME) / 1000000 ))
echo "✅ Done in $DIFF ms."
echo "📊 Current metrics:"
curl -s http://localhost:50090/metrics | grep orchestrator
