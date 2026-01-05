#!/bin/bash
echo "🔥 Starting Stress Test: 100 requests (10 parallel)..."
START_TIME=$(date +%s%N)

for i in {1..100}; do
  curl -s "http://localhost:8080/input?msg=author:Кинг" > /dev/null &
  if (( $i % 10 == 0 )); then wait; fi
done

END_TIME=$(date +%s%N)
DIFF=$(( ($END_TIME - $START_TIME) / 1000000 ))
echo "✅ Done in $DIFF ms."
echo "📊 Current metrics:"
curl -s http://localhost:9090/metrics | grep orchestrator
