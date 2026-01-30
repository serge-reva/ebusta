#!/bin/bash
nc -z localhost 50053 && echo "Orchestrator gRPC: [UP]" || echo "Orchestrator gRPC: [DOWN]"
if curl -fsS http://localhost:50090/metrics | grep -q "orchestrator"; then
  echo "Metrics: [OK]"
else
  echo "Metrics: [WARN] (no metrics on :50090)"
fi
if [ "$STATUS" -eq 200 ]; then
    echo "Metrics: [OK]"
else
    echo "Metrics: [FAIL] (Status: $STATUS)"
fi
