# Переменные
BIN_DIR       := ./bin
LOG_DIR       := ./logs
API_PROTO_DIR := api/proto/v1

# Пути к Scala компонентам
DSL_DIR := cmd/dsl-scala
QB_DIR  := cmd/query-builder

.PHONY: all build proto build-scala build-go build-cli up down restart test clean

all: build

proto:
	@echo "🛠 Generating protobuf..."
	@mkdir -p $(BIN_DIR)
	protoc --proto_path=$(API_PROTO_DIR) \
		--go_out=paths=import:. --go_opt=module=ebusta \
		--go-grpc_out=paths=import:. --go-grpc_opt=module=ebusta \
		$(API_PROTO_DIR)/*.proto
	@go mod tidy

# Scala отдельно
build-scala:
	@echo "🛠 Building Scala components (DSL & Query Builder)..."
	@cd $(DSL_DIR) && sbt clean assembly && cp target/scala-3.3.1/dsl-server.jar ../../dsl-server.jar
	@cd $(QB_DIR)  && sbt clean assembly && cp target/scala-3.3.1/query-builder.jar ../../query-builder.jar

# CLI отдельно (Go)
build-cli:
	@echo "🛠 Building ebusta-cli..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/ebusta-cli ./cmd/cli

# Go отдельно (включая CLI и новые downloads-бинари)
build-go: proto build-cli
	@echo "🛠 Building Go binaries..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/auth-manager    ./cmd/auth-manager
	@go build -o $(BIN_DIR)/datamanager     ./cmd/datamanager
	@go build -o $(BIN_DIR)/orchestrator    ./cmd/orchestrator
	@go build -o $(BIN_DIR)/web-adapter     ./cmd/web-adapter
	@go build -o $(BIN_DIR)/archive-node    ./cmd/archive-node
	@go build -o $(BIN_DIR)/downloads-import ./cmd/downloads-import
	@go build -o $(BIN_DIR)/tier-node       ./cmd/tier-node

# Полная сборка (как раньше "build"), но через раздельные цели
build: proto build-scala build-go
	@echo "✅ Build done."

down:
	@echo "🛑 Stopping all services..."
	@-pkill -9 -f "datamanager|orchestrator|web-adapter|dsl-server.jar|query-builder.jar|archive-node|tier-node" || true
	@sleep 1

up: down
	@echo "🚀 Starting Full Stack..."
	@mkdir -p $(LOG_DIR)

	@echo -n "   - DSL Service: "
	@/home/serge/opt/jdk-17.0.10+7/bin/java -jar dsl-server.jar >> $(LOG_DIR)/dsl.log 2>&1 & sleep 2
	@pgrep -f dsl-server.jar > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED (check dsl.log)"

	@echo -n "   - Query Builder: "
	@/home/serge/opt/jdk-17.0.10+7/bin/java -jar query-builder.jar >> $(LOG_DIR)/qb.log 2>&1 & sleep 2
	@pgrep -f query-builder.jar > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED (check qb.log)"

	@echo -n "   - Data Manager: "
	@$(BIN_DIR)/datamanager >> $(LOG_DIR)/dm.log 2>&1 & sleep 0.5
	@pgrep -f datamanager > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED (check dm.log)"

	@echo -n "   - Orchestrator: "
	@$(BIN_DIR)/orchestrator >> $(LOG_DIR)/orch.log 2>&1 & sleep 0.5
	@pgrep -f orchestrator > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED (check orch.log)"

	@echo -n "   - Web Adapter: "
	@$(BIN_DIR)/web-adapter >> $(LOG_DIR)/web.log 2>&1 & sleep 0.5
	@pgrep -f web-adapter > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED (check web.log)"

	@echo "\n📊 Final check (Active processes):"
	@ps aux | grep -v grep | grep -E "datamanager|orchestrator|web-adapter|dsl-server.jar|query-builder.jar"
	@echo "\n✅ Logs are available in $(LOG_DIR)/"

restart: up

test:
	@echo "🧪 Running Go tests..."
	@go test ./...

clean:
	@echo "🧹 Cleaning up..."
	@rm -rf $(BIN_DIR) $(LOG_DIR) *.jar *.log
	@cd $(DSL_DIR) && sbt clean
	@cd $(QB_DIR)  && sbt clean
