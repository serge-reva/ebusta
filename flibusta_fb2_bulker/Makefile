BINARY_NAME=bulker
INSTALL_DIR=/opt/flibusta_fb2_bulker
BIN_PATH=$(INSTALL_DIR)/$(BINARY_NAME)
LOCAL_BIN=./bin/$(BINARY_NAME)
IS_ROOT = $(shell id -u)

.PHONY: build install clean check-root

build:
	go mod tidy
	mkdir -p bin
	go build -o $(LOCAL_BIN) ./cmd/bulker/main.go

check-root:
ifneq ($(IS_ROOT), 0)
	@echo "Error: Run 'sudo make install'"
	@exit 1
endif

install: check-root
	@if [ ! -f $(LOCAL_BIN) ]; then echo "Run 'make build' first"; exit 1; fi
	mkdir -p $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)/data/src $(INSTALL_DIR)/data/out $(INSTALL_DIR)/data/warn
	cp $(LOCAL_BIN) $(INSTALL_DIR)/
	cp ./config.yaml $(INSTALL_DIR)/
	cp ./scripts/scan_zips.sh $(INSTALL_DIR)/
	chmod +x $(BIN_PATH)
	chmod +x $(INSTALL_DIR)/scan_zips.sh
	@echo "Installed to $(INSTALL_DIR)"
	@echo "Link: sudo ln -sf $(BIN_PATH) /usr/local/bin/$(BINARY_NAME)"

clean:
	rm -rf bin
