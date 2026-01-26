# EBusta Project Makefile

.PHONY: all build-go run-dsl-server run-dsl-client run-example-server test-dsl clean

# Переменные путей
PROJ_DIR := $(shell pwd)
LISP_DIR := $(PROJ_DIR)/lisp-converter
GRPC_DIR := $(PROJ_DIR)/grpc

all: build-go

# --- Go Backend ---
build-go:
	go build -o bin/ ./cmd/...

# --- Lisp DSL Converter & gRPC ---

## Запуск основного DSL сервера
run-dsl-server:
	bash $(LISP_DIR)/run_dsl_server.sh

## Запуск тестового Lisp клиента для DSL
run-dsl-client:
	bash $(LISP_DIR)/run_dsl_client.sh

## Запуск примера SayHello (порт 50051)
run-example-server:
	bash $(LISP_DIR)/run_example_server.sh

## Запуск примера SayHello клиента
run-example-client:
	bash $(LISP_DIR)/run_example_client.sh

## Тест DSL через grpcurl (автоматический вызов с импортом)
test-dsl:
	$(LISP_DIR)/grpcurl -plaintext \
		-import-path $(LISP_DIR) \
		-proto $(LISP_DIR)/search.proto \
		-d '{"raw_query": "(:and (:field \"title\" \"Lisp\") (:field \"author\" \"Serge\"))"}' \
		localhost:50052 ebusta.library.v1.MessageConverter/Convert

## Обновление API документации
update-docs:
	bash $(LISP_DIR)/update_api_docs.sh

# --- Служебные команды ---
clean:
	rm -rf bin/*
	fuser -k 50051/tcp 50052/tcp || true

