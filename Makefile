# === EBusta Project Master Makefile ===

# Пути
LISP_DIR=$(shell pwd)/lisp-converter
GRPC_DIR=$(shell pwd)/grpc
TESTS_DIR=$(shell pwd)/tests

# Параметры компонентов
DSL_PORT=50052
DATA_PORT=50051
ORCH_PORT=50053
WEB_PORT=8080

.PHONY: build-bin stop-all start-all restart-all test-compliance test-load clean help

help:
	@echo "Usage:"
	@echo "  make build-bin       - Собрать автономный бинарник dsl-converter"
	@echo "  make start-all       - Последовательный запуск всех сервисов"
	@echo "  make stop-all        - Принудительная остановка всех компонентов"
	@echo "  make restart-all     - Пересборка и перезапуск стека"
	@echo "  make test-compliance - Запуск функциональных тестов соответствия DSL"
	@echo "  make test-load       - Запуск нагрузочного теста (200 RPS)"

# --- Сборка ---
build-bin:
	@echo "Building DSL-Converter binary..."
	sbcl --noinform \
		--eval '(push (truename "$(GRPC_DIR)/") asdf:*central-registry*)' \
		--eval '(push (truename "$(LISP_DIR)/") asdf:*central-registry*)' \
		--eval '(ql:quickload :ebusta-search :silent t)' \
		--load "$(LISP_DIR)/dsl-service.lisp" \
		--eval '(ebusta-service:build-binary)' \
		--quit
	@chmod +x dsl-converter

# --- Управление процессами ---

stop-all:
	@echo "=== Stopping Ebusta Stack ==="
	-pkill -f dsl-converter || true
	-pkill -f orchestrator || true
	-pkill -f data-manager || true
	-pkill -f web-adapter || true
	@echo "Processes terminated."

start-all:
	@echo "=== Starting Ebusta Stack (Sequential) ==="
	
	@echo "[1/4] Starting Data-Manager..."
	# ./data-manager & 
	@sleep 1
	
	@echo "[2/4] Starting DSL-Converter (V19)..."
	./dsl-converter &
	@sleep 2
	
	@echo "[3/4] Starting Orchestrator..."
	# ./orchestrator &
	@sleep 1
	
	@echo "[4/4] Starting Web-Adapter..."
	# ./web-adapter &
	
	@$(MAKE) check-ports
	@echo "=== Stack is UP ==="

restart-all: stop-all build-bin start-all

# --- Тестирование ---

# 1. Цель по функциональному тестированию (Compliance)
# Проверяет корректность парсинга различных конструкций DSL
test-compliance:
	@echo "Running DSL compliance tests..."
	@bash $(TESTS_DIR)/test_compliance.sh

# 2. Цель по нагрузочному тестированию (Load Testing)
# Проверяет стабильность и Latency под нагрузкой в 200 RPS
test-load:
	@echo "Running load test (200 RPS, 30s)..."
	@if [ -f $(LISP_DIR)/ghz ]; then \
		bash $(TESTS_DIR)/load_test.sh; \
	else \
		echo "Error: ghz tool not found in $(LISP_DIR)"; \
		exit 1; \
	fi

# --- Диагностика ---

check-ports:
	@echo "Checking service ports..."
	@nc -z localhost $(DATA_PORT) && echo "Data-Manager (50051): OK" || echo "Data-Manager: FAIL"
	@nc -z localhost $(DSL_PORT)  && echo "DSL-Converter (50052): OK" || echo "DSL-Converter: FAIL"
	@nc -z localhost $(ORCH_PORT) && echo "Orchestrator (50053): OK" || echo "Orchestrator: FAIL"
	@nc -z localhost $(WEB_PORT)  && echo "Web-Adapter (8080): OK" || echo "Web-Adapter: FAIL"

clean:
	rm -f dsl-converter
	-pkill -f dsl-converter || true

# --- DEBUG: Direct Parser Test ---
# Позволяет проверить логику Shunting-yard и Greedy Capture без gRPC
test-parser-direct:
	@echo "=== Direct Parser Logic Test (Zero Network) ==="
	@sbcl --noinform \
		--eval '(push (truename "$(GRPC_DIR)/") asdf:*central-registry*)' \
		--eval '(push (truename "$(LISP_DIR)/") asdf:*central-registry*)' \
		--eval '(ql:quickload :ebusta-search :silent t)' \
		--load "$(LISP_DIR)/dsl-service.lisp" \
		--eval '(format t "Input: author:\"Стивен Кинг\"~%Output: ~S~%" (ebusta-service:parse-raw-to-sexp "author:\"Стивен Кинг\""))' \
		--eval '(format t "Input: author:Стивен Кинг AND year:2026~%Output: ~S~%" (ebusta-service:parse-raw-to-sexp "author:Стивен Кинг AND year:2026"))' \
		--quit
