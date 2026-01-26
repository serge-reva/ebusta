LISP_DIR=$(shell pwd)/lisp-converter
GRPC_DIR=$(shell pwd)/grpc
GEN_DIR=grpc/gen/go

.PHONY: build-bin start-all stop-all test-compliance proto restart-all

proto:
	@mkdir -p $(GEN_DIR)
	protoc --proto_path=lisp-converter \
		--go_out=$(GEN_DIR) --go_opt=paths=source_relative \
		--go-grpc_out=$(GEN_DIR) --go-grpc_opt=paths=source_relative \
		lisp-converter/search.proto
	@go mod tidy

build-bin:
	@echo "Building DSL-Converter binary..."
	sbcl --noinform \
		--eval '(push (truename "$(LISP_DIR)/") asdf:*central-registry*)' \
		--eval '(ql:quickload :ebusta-search :silent t)' \
		--load "$(LISP_DIR)/dsl-service.lisp" \
		--eval '(ebusta-service:build-binary)' \
		--quit

stop-all:
	-pkill -f dsl-converter || true

start-all: stop-all
	./dsl-converter > dsl.log 2>&1 &
	@sleep 2

test-compliance:
	@go run tests/compliance_runner.go

restart-all: stop-all build-bin start-all
