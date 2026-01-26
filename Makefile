# EBusta Project Makefile
LISP_DIR=./lisp-converter
TESTS_DIR=./tests

.PHONY: run-dsl-server test-dsl test-compliance update-docs

run-dsl-server:
	bash $(LISP_DIR)/run_dsl_server.sh

test-dsl:
	$(LISP_DIR)/grpcurl -plaintext \
		-import-path $(LISP_DIR) \
		-proto $(LISP_DIR)/search.proto \
		-d '{"raw_query": "(:and (:field \"title\" \"Lisp\") (:field \"author\" \"Serge\"))"}' \
		localhost:50052 ebusta.library.v1.MessageConverter/Convert

test-compliance:
	bash $(TESTS_DIR)/test_compliance.sh

update-docs:
	bash $(LISP_DIR)/update_api_docs.sh
