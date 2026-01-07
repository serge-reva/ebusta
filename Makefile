BIN_DIR=bin
PROTO_DIR=api/proto/v1

.PHONY: build run stop clean smoke-test smoke proto tidy

# Главная цель: сначала генерация proto, потом сборка
build: proto
	@mkdir -p $(BIN_DIR)
	@# Создаем скрипт для логирования (вывод в консоль + файл)
	@printf "#!/bin/bash\ntee -a \$$1" > $(BIN_DIR)/tee.sh && chmod +x $(BIN_DIR)/tee.sh
	
	@echo "📦 Tidy root dependencies..."
	@go mod tidy

	@echo "🏗️  Building Core Services..."
	@go build -o $(BIN_DIR)/datamanager ./cmd/datamanager
	@go build -o $(BIN_DIR)/auth-manager ./cmd/auth-manager
	@go build -o $(BIN_DIR)/message-converter ./cmd/message-converter
	@go build -o $(BIN_DIR)/processor ./cmd/processor
	@go build -o $(BIN_DIR)/orchestrator ./cmd/orchestrator
	@go build -o $(BIN_DIR)/web-adapter ./cmd/web-adapter
	@go build -o $(BIN_DIR)/ebusta-cli ./cmd/cli
	@go build -o $(BIN_DIR)/client ./cmd/client

	@echo "🏗️  Building F2Bulker (Nested Module)..."
	@cd f2bulker && go mod tidy && go build -o ../$(BIN_DIR)/f2bulker ./cmd/bulker

# Генерация gRPC кода
proto:
	@echo "🧬 Generating gRPC code..."
	@protoc --proto_path=. \
		--go_out=. --go_opt=paths=source_relative \
		--go-grpc_out=. --go-grpc_opt=paths=source_relative \
		$(PROTO_DIR)/library.proto

# Запуск инфраструктуры
run: stop build
	@echo "🚀 Starting services..."
	@./$(BIN_DIR)/datamanager 2>&1 | ./$(BIN_DIR)/tee.sh datamanager.log &
	@./$(BIN_DIR)/auth-manager 2>&1 | ./$(BIN_DIR)/tee.sh auth-manager.log &
	@./$(BIN_DIR)/message-converter 2>&1 | ./$(BIN_DIR)/tee.sh message-converter.log &
	@./$(BIN_DIR)/processor 2>&1 | ./$(BIN_DIR)/tee.sh processor.log &
	@./$(BIN_DIR)/orchestrator 2>&1 | ./$(BIN_DIR)/tee.sh orchestrator.log &
	@./$(BIN_DIR)/web-adapter 2>&1 | ./$(BIN_DIR)/tee.sh web-adapter.log &
	@echo "✅ All systems go! Logs are being written to *.log"
	@sleep 2

# Остановка (игнорируем ошибки если процесс не найден)
stop:
	@echo "🛑 Stopping services..."
	@-pkill -f $(BIN_DIR)/datamanager > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/auth-manager > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/message-converter > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/processor > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/orchestrator > /dev/null 2>&1 || true
	@-pkill -f $(BIN_DIR)/web-adapter > /dev/null 2>&1 || true

# Быстрый тест CLI
smoke-test:
	@echo "🧪 Running CLI Smoke Check..."
	@./$(BIN_DIR)/ebusta-cli "author:Кинг" | grep -q "Plan" && echo "  ✅ CLI OK" || (echo "  ❌ CLI Failed"; exit 1)

# Запуск скриптовых тестов
smoke:
	@echo "🧪 Running Integration Smoke Tests..."
	@for test in tests/smoke_*.sh; do \
		echo -n "Running $$test... "; \
		bash $$test; \
	done

# Очистка
clean: stop
	@echo "🧹 Cleaning up..."
	rm -rf $(BIN_DIR) *.log
	# Удаляем сгенерированные pb.go файлы, чтобы гарантировать чистую пересборку
	find . -name "*.pb.go" -delete
