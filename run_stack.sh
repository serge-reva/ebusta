#!/bin/bash

# Функция для убийства процесса на порту
kill_port() {
    PORT=$1
    PID=$(lsof -t -i:$PORT)
    if [ -n "$PID" ]; then
        echo "🔪 Killing process on port $PORT (PID $PID)..."
        kill -9 $PID 2>/dev/null
    fi
}

echo "🛑 Cleaning up ports..."
kill_port 50053 # Orchestrator
kill_port 50051 # DataManager
kill_port 50054 # DSL Service
kill_port 50055 # Query Builder
kill_port 50080 # Web Adapter
sleep 1

echo "🚀 Starting Services..."

# 1. DSL Service (Scala)
echo "   - Starting DSL Service (:50054)..."
nohup java -jar cmd/dsl-scala/dsl-server.jar > dsl.log 2>&1 &

# 2. Query Builder (Scala)
echo "   - Starting Query Builder (:50055)..."
nohup java -jar cmd/query-builder/query-builder.jar > qb.log 2>&1 &

# 3. Data Manager (Go)
echo "   - Starting Data Manager (:50051)..."
nohup ./cmd/datamanager/datamanager > dm.log 2>&1 &

# 4. Orchestrator (Go)
echo "   - Starting Orchestrator (:50053)..."
nohup ./cmd/orchestrator/orchestrator > orch.log 2>&1 &

# 5. Web Adapter (Go)
echo "   - Starting Web Adapter (:50080)..."
nohup ./cmd/web-adapter/web-adapter > web.log 2>&1 &

echo "⏳ Waiting 5s for services to warm up..."
sleep 5

echo "✅ Stack is running!"
echo "   Logs: dsl.log, qb.log, dm.log, orch.log, web.log"
