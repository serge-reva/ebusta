#!/bin/bash
PORT=50055

echo "🔍 Checking port $PORT..."

# Пытаемся найти PID процесса, занимающего порт
PID=$(lsof -t -i:$PORT)

if [ -n "$PID" ]; then
    echo "⚠️  Port $PORT is busy by PID $PID. Killing..."
    kill -9 $PID
    # Даем системе секунду на освобождение ресурса
    sleep 1
    echo "✅ Process killed."
else
    echo "✅ Port $PORT is free."
fi

echo "🚀 Starting Query Builder..."
java -jar cmd/query-builder/query-builder.jar "$@"
