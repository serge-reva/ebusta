#!/bin/bash
nc -z localhost 50053 && echo "Orchestrator gRPC: [OK]" || echo "Orchestrator gRPC: [FAIL]"
if curl -fsS http://localhost:50090/metrics | grep -q "orchestrator"; then
  echo "Metrics: [OK]"
else
  echo "Metrics: [WARN] (no metrics on :50090)"
fi
