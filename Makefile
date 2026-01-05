BIN_DIR=bin
PROTO_DIR=api/proto/v1

.PHONY: build run stop clean smoke-test smoke

build:
	@mkdir -p $(BIN_DIR)
	@printf "#!/bin/bash\ncat" > tee.sh && chmod +x tee.sh
	@protoc --proto_path=$(PROTO_DIR) --go_out=$(PROTO_DIR) --go_opt=paths=source_relative --go-grpc_out=$(PROTO_DIR) --go-grpc_opt=paths=source_relative $(PROTO_DIR)/library.proto
	@go build -o $(BIN_DIR)/data-manager ./cmd/data-manager
	@go build -o $(BIN_DIR)/message-converter ./cmd/message-converter
	@go build -o $(BIN_DIR)/processor ./cmd/processor
	@go build -o $(BIN_DIR)/orchestrator ./cmd/orchestrator
	@go build -o $(BIN_DIR)/web-adapter ./cmd/web-adapter
	@go build -o $(BIN_DIR)/ebusta-cli ./cmd/cli

run: stop build
	@echo "🚀 Starting services..."
	@./$(BIN_DIR)/data-manager 2>&1 | ./tee.sh data-manager.log &
	@./$(BIN_DIR)/message-converter 2>&1 | ./tee.sh message-converter.log &
	@./$(BIN_DIR)/processor 2>&1 | ./tee.sh processor.log &
	@./$(BIN_DIR)/orchestrator 2>&1 | ./tee.sh orchestrator.log &
	@./$(BIN_DIR)/web-adapter 2>&1 | ./tee.sh web-adapter.log &
	@sleep 2

stop:
	@echo "🛑 Stopping services..."
	@-pkill -f $(BIN_DIR)/data-manager > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/message-converter > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/processor > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/orchestrator > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/web-adapter > /dev/null 2>&1 || true

smoke-test: build
	@echo "🧪 Running CLI Smoke Tests..."
	@./$(BIN_DIR)/ebusta-cli "author:Кинг" | grep -q "Plan" && echo "  ✅ OK" || (echo "  ❌ Failed"; exit 1)

smoke:
	@for test in tests/smoke_*.sh; do \
		bash $$test; \
	done

clean: stop
	rm -rf $(BIN_DIR) *.log tee.sh
