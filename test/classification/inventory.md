# Test Inventory Baseline

Legend:
- Quality: `good` / `medium` / `bad`
- Target Type: `unit` / `integration` / `e2e` / `load` / `drop`
- Priority: `P0` / `P1` / `P2`

| File | Current Type | Quality | Target Type | Priority | Comment |
|------|--------------|---------|-------------|----------|---------|
| cmd/datamanager/main_test.go | unit | good | unit | P2 | Pure function tests, isolated. |
| cmd/irc-adapter/bot_test.go | integration-local | medium | integration | P1 | Protocol-heavy tests, complex setup. |
| cmd/irc-adapter/handler_test.go | integration-local | medium | integration | P1 | Large scenario file, should be split. |
| cmd/irc-adapter/main_test.go | unit | good | unit | P2 | Config/default checks. |
| cmd/telegram-adapter/handler_test.go | integration-local | good | integration | P2 | Uses httptest and mock transport. |
| cmd/telegram-adapter/main_test.go | unit | good | unit | P2 | Config/default checks. |
| internal/edge/engine_test.go | unit | good | unit | P2 | Isolated engine behavior tests. |
| internal/edge/guards_test.go | unit | good | unit | P2 | Validation checks, no external deps. |
| internal/edge/telemetry_test.go | unit | good | unit | P2 | Local telemetry hooks only. |
| internal/edge/throttle_test.go | unit | medium | unit | P1 | Uses `time.Sleep`, can be de-flaked. |
| internal/errutil/codes_test.go | unit | good | unit | P2 | Fast deterministic mapping tests. |
| internal/gateway/clients/downloader_test.go | integration-local | good | integration | P2 | Proper mock RoundTripper usage. |
| internal/gateway/handlers_diag_test.go | unit | good | unit | P2 | Small deterministic parser test. |
| internal/gateway/mapper/mapper_test.go | unit | medium | unit | P1 | TTL checked via sleep, fragile timing. |
| internal/gateway/middleware/middleware_test.go | integration-local | good | integration | P2 | Good httptest coverage. |
| internal/gateway/validation/validation_test.go | unit | good | unit | P2 | Deterministic input validation. |
| internal/logger/logger_functional_test.go | functional-local | medium | integration | P1 | Very large file, needs decomposition. |
| internal/logger/logger_test.go | unit/integration-local | good | integration | P2 | Stable tests with local I/O/httptest. |
| internal/presenter/formatter_test.go | unit | good | unit | P2 | Pure formatting logic. |
| internal/presenter/irc_formatter_test.go | unit | good | unit | P2 | Pure formatting logic. |
| internal/presenter/search_result_test.go | unit | good | unit | P2 | Pure pagination/result logic. |
| tests/errutil/error_test.go | unit | medium | unit | P1 | Duplicates internal errutil suite. |
| tests/errutil/grpc_test.go | unit | medium | unit | P1 | Duplicates internal errutil suite. |
| tests/errutil/codes_test.go | unit | medium | unit | P1 | Duplicates internal errutil suite. |
| tests/errutil/http_test.go | integration-local | medium | integration | P1 | Useful but duplicated placement. |
| tests/errutil/trace_test.go | integration-local | medium | integration | P1 | Useful but duplicated placement. |
| tests/gateway/integration_test.go | pseudo-integration | bad | integration | P1 | Named integration, mostly partial local checks. |
| tests/test_component_datamanager.sh | e2e-script | medium | e2e | P2 | Depends on running gRPC service. |
| tests/test_component_orchestrator.sh | e2e-script | bad | e2e | P0 | Uses outdated orchestrator port (`50053`). |
| tests/test_component_web_adapter.sh | e2e-script | bad | drop | P0 | Legacy endpoint `/input`, outdated behavior. |
| tests/test_functional_ast_web_adapter.sh | e2e-script | bad | drop | P0 | Legacy `/input` + log-grepping (`orch.log`). |
| tests/test_functional_results_web_adapter.sh | e2e-script | bad | drop | P0 | Legacy `/input` endpoint. |
| tests/test_functional_ast_cli.sh | e2e-script | bad | e2e | P0 | Asserts via log file patterns; flaky by design. |
| tests/test_functional_results_cli.sh | e2e-script | medium | e2e | P2 | Useful contract check, stack-dependent. |
| tests/test_e2e_chain_neighbors.sh | e2e-script | bad | drop | P0 | Legacy web-adapter `/input` and metric coupling. |
| tests/test_errutil_e2e.sh | e2e-script | medium | e2e | P1 | Valuable contracts, depends on local stack/ports. |
| tests/test_logger_e2e.sh | mixed script | medium | integration | P1 | Mixes build/grep checks, should be split. |
| tests/test_dsl_multiword.sh | integration/e2e script | medium | e2e | P2 | Heavy, but useful end-to-end DSL smoke. |
| tests/opensearch_test.sh | external integration | bad | drop | P0 | Depends on external OpenSearch (`cloud-1`). |
| tests/opensearch_templates_test.sh | external integration | bad | load | P2 | Diagnostic inspector, not CI-suitable. |
| tests/load_test.sh | load | bad | load | P2 | Throughput benchmark, not regression test. |
| tests/stress_test.sh | load | bad | load | P2 | Stress utility, not deterministic CI test. |
| tests/test_compliance.sh | legacy integration script | bad | drop | P0 | Legacy converter path and brittle shell assertions. |
| tests/compliance_runner.go | diagnostic utility | bad | drop | P0 | `//go:build ignore`, legacy import path. |
| tests/grpc_check/main.go | diagnostic utility | medium | e2e | P2 | Useful helper for smoke checks. |
| tests/diagnose/main.go | diagnostic utility | bad | drop | P0 | Legacy ports and legacy web-adapter endpoint. |
| smoke.sh | script wrapper | bad | drop | P0 | Calls missing Makefile targets (`run/smoke-test/stop`). |
| dsl-scala/src/test (missing) | n/a | bad | integration | P1 | No Scala tests for DSL service. |
| query-builder/src/test (missing) | n/a | bad | integration | P1 | No Scala tests for query-builder service. |
