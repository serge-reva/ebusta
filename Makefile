LISP_DIR=$(shell pwd)/lisp-converter
API_PROTO_DIR=api/proto/v1

# Конфигурация портов
WEB_PORT=50080
ORCH_PORT=50053
DSL_PORT=50052
DATA_PORT=50051

.PHONY: build-all stop-all start-all restart-all test-compliance test-e2e proto clean

proto:
	@echo "Generating protobuf files..."
	@mkdir -p $(API_PROTO_DIR)
	protoc --proto_path=$(API_PROTO_DIR) \
		--go_out=paths=import:. \
		--go-grpc_out=paths=import:. \
		$(API_PROTO_DIR)/*.proto
	@go mod tidy
	@echo "✅ Proto generation complete"

build-all: proto
	@echo "Building DSL-Converter..."
	sbcl --noinform --eval '(push (truename "$(LISP_DIR)/") asdf:*central-registry*)' \
		--eval '(ql:quickload :ebusta-search :silent t)' \
		--load "$(LISP_DIR)/dsl-service.lisp" \
		--eval '(ebusta-service:build-binary)' --quit
	@echo "Building Go Stack..."
	go build -o datamanager ./cmd/datamanager
	go build -o orchestrator ./cmd/orchestrator
	go build -o web-adapter ./cmd/web-adapter

stop-all:
	@-pkill -f dsl-converter || true
	@-pkill -f orchestrator || true
	@-pkill -f datamanager || true
	@-pkill -f web-adapter || true

start-all:
	@echo "Starting Full Stack..."
	./datamanager -port $(DATA_PORT) >> data.log 2>&1 &
	./dsl-converter >> dsl.log 2>&1 &
	@sleep 2
	./orchestrator -port $(ORCH_PORT) >> orch.log 2>&1 &
	@sleep 1
	./web-adapter -port $(WEB_PORT) >> web.log 2>&1 &
	@sleep 1
	@echo "--- Service Status ---"
	@grep "===" data.log | tail -n 1 || echo "[DATAMANAGER] No banner"
	@grep "===" dsl.log | tail -n 1 || echo "[DSL-CONVERTER] No banner"
	@grep "===" orch.log | tail -n 1 || echo "[ORCHESTRATOR] No banner"
	@grep "===" web.log | tail -n 1 || echo "[WEB-ADAPTER] No banner"
	@echo "----------------------"

test-compliance:
	@go run tests/compliance_runner.go

test-e2e:
	@echo "=== Running E2E Test (HTTP :$(WEB_PORT)/input) ==="
	@curl -v -X POST --data-urlencode "msg=author:\"Стивен Кинг\"" http://localhost:$(WEB_PORT)/input

restart-all: stop-all build-all start-all

clean:
	@echo "Cleaning build artifacts..."
	@rm -f datamanager orchestrator web-adapter dsl-converter
	@rm -f *.log
	@echo "✅ Clean complete"
