LISP_DIR=$(shell pwd)/lisp-converter
API_PROTO_DIR=api/proto/v1
API_PROTOS := $(filter-out $(API_PROTO_DIR)/auth.proto, $(wildcard $(API_PROTO_DIR)/*.proto))

# Конфигурация портов
.PHONY: build-all stop-all start-all restart-all test-compliance test-e2e proto clean gen-dsl

proto:
	@echo "Generating protobuf files..."
	@mkdir -p $(API_PROTO_DIR)
	protoc --proto_path=$(API_PROTO_DIR) \
		--go_out=paths=import:. --go_opt=module=ebusta \
		--go-grpc_out=paths=import:. --go-grpc_opt=module=ebusta \
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
	./datamanager >> data.log 2>&1 &
	./dsl-converter >> dsl.log 2>&1 &
	@sleep 2
	./orchestrator >> orch.log 2>&1 &
	@sleep 1
	./web-adapter >> web.log 2>&1 &
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
	@bash tests/test_e2e_chain_neighbors.sh


# BEGIN EBUSTA TEST STACK
.PHONY: build-cli test-components test-e2e-chain-neighbors test-stack stack-stop stack-build stack-up \
        test-component-datamanager test-component-orchestrator test-component-web-adapter test-component-cli

build-cli:
	@mkdir -p bin
	go build -o bin/ebusta-cli ./cmd/cli

test-component-datamanager:
	@bash tests/test_component_datamanager.sh

test-component-orchestrator:
	@bash tests/test_component_orchestrator.sh

test-component-web-adapter:
	@bash tests/test_component_web_adapter.sh

test-component-cli:
	@bash tests/test_component_cli.sh

test-components: test-component-datamanager test-component-orchestrator test-component-web-adapter test-component-cli

test-e2e-chain-neighbors:
	@bash tests/test_e2e_chain_neighbors.sh

stack-stop:
	@$(MAKE) stop-all

stack-build: stack-stop
	@$(MAKE) build-all
	@$(MAKE) build-cli

stack-up: stack-build
	@$(MAKE) start-all

test-stack: stack-up
	@$(MAKE) test-components
	@$(MAKE) test-e2e-chain-neighbors

.PHONY: test-functional test-functional-only test-functional-web test-functional-cli

test-functional-web:
	@bash tests/test_functional_ast_web_adapter.sh

test-functional-cli:
	@bash tests/test_functional_ast_cli.sh

test-functional-only: test-functional-web test-functional-cli

# Starts stack via stack-up, then runs functional AST tests
test-functional: stack-up test-functional-only

.PHONY: test-results test-results-only test-results-web test-results-cli

test-results-web:
	@bash tests/test_functional_results_web_adapter.sh

test-results-cli:
	@bash tests/test_functional_results_cli.sh

test-results-only: test-results-web test-results-cli

# Starts stack via stack-up, then runs functional results tests (expects non-empty results)
test-results: stack-up test-results-only
# END EBUSTA TEST STACK

# Runs stack tests + functional AST suite in one routine run
.PHONY: test-routine
test-routine: test-stack test-functional-only
