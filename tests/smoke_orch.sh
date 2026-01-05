#!/bin/bash
nc -z localhost 50054 && echo "Orchestrator gRPC: [UP]" || echo "Orchestrator gRPC: [DOWN]"
STATUS=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:9090/metrics)
if [ "$STATUS" -eq 200 ]; then
    echo "Metrics: [OK]"
else
    echo "Metrics: [FAIL] (Status: $STATUS)"
fi
