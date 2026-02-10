# Переменные
BIN_DIR       := ./bin
LOG_DIR       := ./logs
API_PROTO_DIR := api/proto/v1
CONFIG_FILE   := ebusta.yaml

# Пути к Scala компонентам (в корне проекта)
DSL_DIR := dsl-scala
QB_DIR  := query-builder

# Определяем списки исходников (find не должен ругаться, если src отсутствует)
DSL_SCALA_SRC := $(shell [ -d "$(DSL_DIR)/src" ] && find "$(DSL_DIR)/src" -name "*.scala" || true)
QB_SCALA_SRC  := $(shell [ -d "$(QB_DIR)/src" ]  && find "$(QB_DIR)/src"  -name "*.scala" || true)
PROTO_SRC     := $(shell find "$(API_PROTO_DIR)" -name "*.proto")

# jar-файлы, которые нужны для запуска (в корне проекта, как использует up)
DSL_JAR := dsl-server.jar
QB_JAR  := query-builder.jar

# Реальные пути, куда sbt assembly пишет артефакты (по build.sbt)
DSL_ASSEMBLY_JAR := cmd/dsl-scala/dsl-server.jar
QB_ASSEMBLY_JAR  := cmd/query-builder/query-builder.jar

# Временный способ вытащить порт tier-node из ebusta.yaml
TIER_PORT := $(shell sed -n '/tier_node:/,/port:/p' $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')

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

# Scala: инкрементально
build-scala: $(DSL_JAR) $(QB_JAR)
	@echo "✅ Scala build up-to-date."

$(DSL_JAR): $(DSL_SCALA_SRC) $(PROTO_SRC)
	@echo "🛠 Building DSL Scala..."
	@cd $(DSL_DIR) && sbt assembly
	@cp $(DSL_ASSEMBLY_JAR) ./$(DSL_JAR)

$(QB_JAR): $(QB_SCALA_SRC) $(PROTO_SRC)
	@echo "🛠 Building Query Builder..."
	@cd $(QB_DIR) && sbt assembly
	@cp $(QB_ASSEMBLY_JAR) ./$(QB_JAR)

build-cli:
	@echo "🛠 Building ebusta-cli..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/ebusta-cli ./cmd/cli

build-go: proto build-cli
	@echo "🛠 Building Go binaries..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/auth-manager     ./cmd/auth-manager
	@go build -o $(BIN_DIR)/datamanager      ./cmd/datamanager
	@go build -o $(BIN_DIR)/orchestrator     ./cmd/orchestrator
	@go build -o $(BIN_DIR)/web-adapter      ./cmd/web-adapter
	@go build -o $(BIN_DIR)/archive-node     ./cmd/archive-node
	@go build -o $(BIN_DIR)/downloads-import ./cmd/downloads-import
	@go build -o $(BIN_DIR)/tier-node        ./cmd/tier-node

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

	@echo -n "   - Tier Node (downloads): "
	@$(BIN_DIR)/tier-node -listen :$(TIER_PORT) >> $(LOG_DIR)/tier.log 2>&1 & sleep 0.5
	@pgrep -f "tier-node" > /dev/null && echo "✅ RUNNING on :$(TIER_PORT)" || echo "❌ FAILED (check tier.log)"

	@echo "\n📊 Final check (Active processes):"
	@ps aux | grep -v grep | grep -E "datamanager|orchestrator|web-adapter|tier-node|dsl-server.jar|query-builder.jar"
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
