# Переменные
BIN_DIR       := ./bin
LOG_DIR       := ./logs
API_PROTO_DIR := api/proto/v1
CONFIG_FILE   := ebusta.yaml
IRC_ADAPTER_PORT := 6667
TELEGRAM_ADAPTER_PORT := $(shell sed -n '/telegram_adapter:/,/port:/p' $(CONFIG_FILE) | grep port | head -1 | awk '{print $$2}')

# Пути к Scala компонентам (в корне проекта)
DSL_DIR := dsl-scala
QB_DIR  := query-builder

DSL_SCALA_SRC := $(shell [ -d "$(DSL_DIR)/src" ] && find "$(DSL_DIR)/src" -name "*.scala" || true)
QB_SCALA_SRC  := $(shell [ -d "$(QB_DIR)/src" ]  && find "$(QB_DIR)/src"  -name "*.scala" || true)
PROTO_SRC     := $(shell find "$(API_PROTO_DIR)" -name "*.proto")

DSL_JAR := dsl-server.jar
QB_JAR  := query-builder.jar

DSL_ASSEMBLY_JAR := cmd/dsl-scala/dsl-server.jar
QB_ASSEMBLY_JAR  := cmd/query-builder/query-builder.jar

ARCHIVE_PORT    := $(shell sed -n '/archive_node:/,/listen_port:/p'    $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
TIER_PORT       := $(shell sed -n '/tier_node:/,/listen_port:/p'       $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
PLASMA_PORT     := $(shell sed -n '/plasma_node:/,/listen_port:/p'     $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
DOWNLOADER_PORT := $(shell sed -n '/downloader:/,/listen_port:/p'      $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
WEB_FRONTEND_PORT := $(shell sed -n '/web_frontend:/,/listen_port:/p'  $(CONFIG_FILE) | grep listen_port | awk '{print $$2}')
GATEWAY_PORT    := $(shell sed -n '/gateway:/,/port:/p'                $(CONFIG_FILE) | grep port | head -1 | awk '{print $$2}')

.PHONY: all build proto proto-generate proto-verify build-scala build-go build-cli build-search-go build-web-frontend build-downloads-go build-gateway build-irc build-telegram up down restart test clean architecture-check docs-check proto-lint proto-breaking test-go test-scala test-unit test-integration test-e2e test-load ci-check docker-build docker-up docker-down docker-logs docker-status

all: build

proto: proto-generate

proto-generate:
	@echo "🛠 Generating protobuf..."
	@command -v protoc >/dev/null 2>&1 || (echo "❌ protoc not installed. Install protobuf compiler."; exit 1)
	@command -v protoc-gen-go >/dev/null 2>&1 || (echo "❌ protoc-gen-go not installed. Run: go install google.golang.org/protobuf/cmd/protoc-gen-go@latest"; exit 1)
	@command -v protoc-gen-go-grpc >/dev/null 2>&1 || (echo "❌ protoc-gen-go-grpc not installed. Run: go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest"; exit 1)
	@mkdir -p $(BIN_DIR)
	protoc --proto_path=$(API_PROTO_DIR) \
		--go_out=paths=import:. --go_opt=module=ebusta \
		--go-grpc_out=paths=import:. --go-grpc_opt=module=ebusta \
		$(API_PROTO_DIR)/*.proto
	@go mod tidy

.PHONY: proto-verify
proto-verify: proto-generate proto-lint proto-breaking
	@git diff --quiet -- $(API_PROTO_DIR)/*.pb.go $(API_PROTO_DIR)/*_grpc.pb.go || (echo "❌ Generated proto files are out of date. Run 'make proto-generate' and commit changes."; exit 1)
	@echo "✅ proto-verify passed"

build-scala: $(DSL_JAR) $(QB_JAR)
	@echo "✅ Scala build up-to-date."


build-irc: proto
	@echo "🛠 Building IRC adapter..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/irc-adapter ./cmd/irc-adapter

build-telegram: proto
	@echo "🛠 Building Telegram adapter..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/telegram-adapter ./cmd/telegram-adapter

$(DSL_JAR): $(DSL_SCALA_SRC) $(PROTO_SRC)
	@echo "🛠 Building DSL Scala..."
	@cd $(DSL_DIR) && sbt assembly
	@cp $(DSL_ASSEMBLY_JAR) ./$(DSL_JAR)

$(QB_JAR): $(QB_SCALA_SRC) $(PROTO_SRC)
	@echo "🛠 Building Query Builder..."
	@cd $(QB_DIR) && sbt assembly
	@cp $(QB_ASSEMBLY_JAR) ./$(QB_JAR)

build-search-go: proto
	@echo "🛠 Building Go search components..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/auth-manager     ./cmd/auth-manager
	@go build -o $(BIN_DIR)/datamanager      ./cmd/datamanager
	@go build -o $(BIN_DIR)/orchestrator     ./cmd/orchestrator
	@go build -o $(BIN_DIR)/web-adapter      ./cmd/web-adapter

build-cli:
	@echo "🛠 Building ebusta-cli..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/ebusta-cli ./cmd/cli

build-web-frontend: proto
	@echo "🛠 Building web-frontend..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/web-frontend ./cmd/web-frontend

build-downloads-go: proto
	@echo "🛠 Building Go downloads components..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/archive-node     ./cmd/archive-node
	@go build -o $(BIN_DIR)/tier-node        ./cmd/tier-node
	@go build -o $(BIN_DIR)/plasma-node      ./cmd/plasma-node
	@go build -o $(BIN_DIR)/downloader       ./cmd/downloader
	@go build -o $(BIN_DIR)/downloads-import ./cmd/downloads-import

build-gateway: proto
	@echo "🛠 Building Gateway..."
	@mkdir -p $(BIN_DIR)
	@go build -o $(BIN_DIR)/gateway ./cmd/gateway

build-go: build-search-go build-cli build-web-frontend build-downloads-go build-gateway build-irc build-telegram
	@echo "✅ Go build done."

build: proto build-scala build-go
	@echo "✅ Build done."

down:
	@echo "🛑 Stopping all services..."
	@-pkill -9 -f "datamanager|orchestrator|web-adapter|web-frontend|gateway|dsl-server.jar|query-builder.jar|archive-node|tier-node|plasma-node|downloader|irc-adapter|telegram-adapter" || true
	@sleep 1

up: down
	@echo "🚀 Starting Full Stack..."
	@mkdir -p $(LOG_DIR)

	@echo -n "   - DSL Service: "
	@/home/serge/opt/jdk-17.0.10+7/bin/java -jar dsl-server.jar >> $(LOG_DIR)/dsl.log 2>&1 & sleep 2
	@pgrep -f dsl-server.jar > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED"

	@echo -n "   - Query Builder: "
	@/home/serge/opt/jdk-17.0.10+7/bin/java -jar query-builder.jar >> $(LOG_DIR)/qb.log 2>&1 & sleep 2
	@pgrep -f query-builder.jar > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED"

	@echo -n "   - Data Manager: "
	@$(BIN_DIR)/datamanager >> $(LOG_DIR)/dm.log 2>&1 & sleep 0.5
	@pgrep -f datamanager > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED"

	@echo -n "   - Orchestrator: "
	@$(BIN_DIR)/orchestrator >> $(LOG_DIR)/orch.log 2>&1 & sleep 0.5
	@pgrep -f orchestrator > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED"

	@echo -n "   - Web Adapter: "
	@$(BIN_DIR)/web-adapter >> $(LOG_DIR)/web.log 2>&1 & sleep 0.5
	@pgrep -f web-adapter > /dev/null && echo "✅ RUNNING" || echo "❌ FAILED"

	@echo -n "   - Archive Node: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/archive-node >> $(LOG_DIR)/archive.log 2>&1 & sleep 0.5
	@pgrep -f archive-node > /dev/null && echo "✅ RUNNING on :$(ARCHIVE_PORT)" || echo "❌ FAILED"

	@echo -n "   - Tier Node: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/tier-node -listen :$(TIER_PORT) >> $(LOG_DIR)/tier.log 2>&1 & sleep 0.5
	@pgrep -f tier-node > /dev/null && echo "✅ RUNNING on :$(TIER_PORT)" || echo "❌ FAILED"

	@echo -n "   - Plasma Node: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/plasma-node >> $(LOG_DIR)/plasma.log 2>&1 & sleep 0.5
	@pgrep -f plasma-node > /dev/null && echo "✅ RUNNING on :$(PLASMA_PORT)" || echo "❌ FAILED"

	@echo -n "   - Downloader: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/downloader >> $(LOG_DIR)/downloader.log 2>&1 & sleep 0.5
	@pgrep -f downloader > /dev/null && echo "✅ RUNNING on :$(DOWNLOADER_PORT)" || echo "❌ FAILED"

	@echo -n "   - Web Frontend: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/web-frontend >> $(LOG_DIR)/web-frontend.log 2>&1 & sleep 0.5
	@pgrep -f web-frontend > /dev/null && echo "✅ RUNNING on :$(WEB_FRONTEND_PORT)" || echo "❌ FAILED"

	@echo -n "   - Gateway: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/gateway >> $(LOG_DIR)/gateway.log 2>&1 & sleep 0.5
	@pgrep -f gateway > /dev/null && echo "✅ RUNNING on :$(GATEWAY_PORT)" || echo "❌ FAILED"

	@echo -n "   - IRC Adapter: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/irc-adapter >> $(LOG_DIR)/irc.log 2>&1 & sleep 0.5
	@pgrep -f irc-adapter > /dev/null && echo "✅ RUNNING on port $(IRC_ADAPTER_PORT)" || echo "❌ FAILED"

	@echo -n "   - Telegram Adapter: "
	@EBUSTA_CONFIG=./$(CONFIG_FILE) $(BIN_DIR)/telegram-adapter >> $(LOG_DIR)/telegram.log 2>&1 & sleep 0.5
	@pgrep -f telegram-adapter > /dev/null && echo "✅ RUNNING on port $(TELEGRAM_ADAPTER_PORT)" || echo "❌ FAILED"


	@echo "\n📊 Active processes:"
	@ps aux | grep -v grep | grep -E "archive-node|tier-node|plasma-node|downloader|web-adapter|orchestrator|datamanager|gateway|irc-adapter|telegram-adapter"

restart: up

clean:
	@rm -rf $(BIN_DIR) $(LOG_DIR) *.jar *.log

.PHONY: architecture-check
architecture-check:
	@echo "🔍 Checking architecture boundaries..."
	@command -v rg >/dev/null 2>&1 || (echo "❌ rg not installed"; exit 1)
	@if rg -n 'os\.(Create|OpenFile|WriteFile|Rename|Mkdir|MkdirAll|ReadDir|ReadFile)' cmd/gateway internal/gateway; then \
		echo "❌ Gateway must not perform filesystem operations"; exit 1; \
	fi
	@if rg -n '"ebusta/internal/downloads"' cmd/gateway internal/gateway; then \
		echo "❌ Gateway must not import internal/downloads"; exit 1; \
	fi
	@test -f cmd/datamanager/main.go || (echo "❌ cmd/datamanager/main.go missing"; exit 1)
	@test -f cmd/orchestrator/main.go || (echo "❌ cmd/orchestrator/main.go missing"; exit 1)
	@test -f cmd/gateway/main.go || (echo "❌ cmd/gateway/main.go missing"; exit 1)
	@if rg --files -g 'Dockerfile*' | grep -q .; then \
		if rg -n --glob 'Dockerfile*' '^[[:space:]]*RUN[[:space:]]+(go build|npm)' ; then \
			echo "❌ Dockerfile must not run go build or npm"; exit 1; \
		fi; \
	fi
	@echo "✅ architecture-check passed"

.PHONY: docs-check
docs-check:
	@test -f docs/ARCHITECTURAL_CONSTITUTION.md || echo "⚠️  docs/ARCHITECTURAL_CONSTITUTION.md missing (will be created later)"
	@test -f docs/API_ERROR_MAPPING.md || echo "⚠️  docs/API_ERROR_MAPPING.md missing"
	@test -f docs/TRACE.md || echo "⚠️  docs/TRACE.md missing"
	@echo "✅ docs-check passed (warnings are non-fatal for now)"

.PHONY: proto-lint proto-breaking
proto-lint:
	@command -v buf >/dev/null 2>&1 || (echo "❌ buf not installed"; exit 1)
	buf lint

proto-breaking:
	@command -v buf >/dev/null 2>&1 || (echo "❌ buf not installed"; exit 1)
	buf breaking --against '.git#branch=main'

.PHONY: test-go
test-go:
	go test ./...

.PHONY: test-scala
test-scala:
	@command -v sbt >/dev/null 2>&1 || (echo "❌ sbt not installed"; exit 1)
	cd dsl-scala && sbt test
	cd query-builder && sbt test

.PHONY: test-unit
test-unit:
	go test -short $$(go list ./... | grep -v '^ebusta/tests')

.PHONY: test-integration
test-integration:
	go test ./internal/gateway/... ./internal/logger/... ./cmd/irc-adapter ./cmd/telegram-adapter ./tests/errutil ./tests/gateway

.PHONY: test-e2e
test-e2e:
	@set -e; \
	$(MAKE) docker-up; \
	ec=0; \
	for t in ./test/e2e/datamanager.sh ./test/e2e/orchestrator.sh ./test/e2e/cli_results.sh ./test/e2e/errutil.sh ./test/e2e/dsl_multiword.sh; do \
		echo "▶ Running $$t"; \
		if ! $$t; then ec=$$?; break; fi; \
	done; \
	$(MAKE) docker-down; \
	exit $$ec

.PHONY: test-load
test-load:
	./test/load/load_test.sh
	./test/load/stress_test.sh

.PHONY: ci-check
ci-check: test-unit test-integration test-scala proto-verify architecture-check docs-check
	@echo "✅ ci-check passed"

.PHONY: docker-build
docker-build:
	docker compose build

.PHONY: docker-up
docker-up:
	docker compose up -d

.PHONY: docker-down
docker-down:
	docker compose down

.PHONY: docker-logs
docker-logs:
	docker compose logs -f

.PHONY: docker-status
docker-status:
	docker compose ps
