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

# Временный способ вытащить порты downloads-нод из ebusta.yaml
ARCHIVE_PORT    := $(shell sed -n '/archive_node:/,/listen_port:/p'    $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
TIER_PORT       := $(shell sed -n '/tier_node:/,/listen_port:/p'       $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
PLASMA_PORT     := $(shell sed -n '/plasma_node:/,/listen_port:/p'     $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
DOWNLOADER_PORT := $(shell sed -n '/downloader:/,/listen_port:/p'      $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')

.PHONY: all build proto build-scala build-go build-cli build-search-go build-downloads-go up down restart test clean

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

# 1) сборка компонент поиска (Go)
build-search-go: proto
	@echo "🛠 Building Go search components..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/auth-manager     ./cmd/auth-manager
	@go build -o $(BIN_DIR)/datamanager      ./cmd/datamanager
	@go build -o $(BIN_DIR)/orchestrator     ./cmd/orchestrator
	@go build -o $(BIN_DIR)/web-adapter      ./cmd/web-adapter

# 2) сборка cli
build-cli:
	@echo "🛠 Building ebusta-cli..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/ebusta-cli ./cmd/cli

# 3) сборка downloads: archive + tier + plasma + downloader (+ downloads-import)
build-downloads-go: proto
	@echo "🛠 Building Go downloads components..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/archive-node     ./cmd/archive-node
	@go build -o $(BIN_DIR)/tier-node        ./cmd/tier-node
	@go build -o $(BIN_DIR)/plasma-node      ./cmd/plasma-node
	@go build -o $(BIN_DIR)/downloader       ./downloader/cmd/downloader
	@go build -o $(BIN_DIR)/downloads-import ./cmd/downloads-import

# build-go обязан собрать: (1) поиск, (2) cli, (3) downloads
build-go: build-search-go build-cli build-downloads-go
	@echo "✅ Go build done."

build: proto build-scala build-go
	@echo "✅ Build done."

down:
	@echo "🛑 Stopping all services..."
	@-pkill -9 -f "datamanager|orchestrator|web-adapter|dsl-server.jar|query-builder.jar|archive-node|tier-node|plasma-node|downloader" || true
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

	@echo -n "   - Archive Node (downloads): "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/archive-node >> $(LOG_DIR)/archive.log 2>&1 & sleep 0.5
	@pgrep -f "archive-node" > /dev/null && echo "✅ RUNNING on :$(ARCHIVE_PORT)" || echo "❌ FAILED (check archive.log)"

	@echo -n "   - Tier Node (downloads): "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/tier-node -listen :$(TIER_PORT) >> $(LOG_DIR)/tier.log 2>&1 & sleep 0.5
	@pgrep -f "tier-node" > /dev/null && echo "✅ RUNNING on :$(TIER_PORT)" || echo "❌ FAILED (check tier.log)"

	@echo -n "   - Plasma Node (downloads): "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/plasma-node >> $(LOG_DIR)/plasma.log 2>&1 & sleep 0.5
	@pgrep -f "plasma-node" > /dev/null && echo "✅ RUNNING on :$(PLASMA_PORT)" || echo "❌ FAILED (check plasma.log)"

	@echo -n "   - Downloader (HTTP): "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/downloader >> $(LOG_DIR)/downloader.log 2>&1 & sleep 0.5
	@pgrep -f "downloader" > /dev/null && echo "✅ RUNNING on :$(DOWNLOADER_PORT)" || echo "❌ FAILED (check downloader.log)"

	@echo "\n📊 Final check (Active processes):"
	@ps aux | grep -v grep | grep -E "datamanager|orchestrator|web-adapter|archive-node|tier-node|plasma-node|downloader|dsl-server.jar|query-builder.jar"
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
