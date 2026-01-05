#!/bin/bash
nc -z localhost 50054 && echo "Orchestrator gRPC: [OK]" || echo "Orchestrator gRPC: [FAIL]"
curl -s http://localhost:9090/metrics | grep -q "orchestrator" && echo "Metrics: [OK]" || echo "Metrics: [FAIL]"
