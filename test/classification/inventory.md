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
| test/e2e/datamanager.sh | e2e-script | medium | e2e | P2 | Depends on running gRPC service. |
| test/e2e/orchestrator.sh | e2e-script | medium | e2e | P1 | Updated to current orchestrator port (`50054`), kept as live gRPC check. |
| tests/test_component_web_adapter.sh | removed | n/a | drop | done | Removed as legacy web-adapter-only contract test. |
| tests/test_functional_ast_web_adapter.sh | removed | n/a | drop | done | Removed (legacy `/input` + log-grepping). |
| tests/test_functional_results_web_adapter.sh | removed | n/a | drop | done | Removed as web-adapter legacy suite. |
| tests/test_functional_ast_cli.sh | e2e-script | medium | e2e | P1 | Rewritten to API/CLI output assertions (no log-file checks). |
| test/e2e/cli_results.sh | e2e-script | medium | e2e | P2 | Useful contract check, stack-dependent. |
| tests/test_e2e_chain_neighbors.sh | removed | n/a | drop | done | Removed as legacy web-adapter/metrics-coupled test. |
| test/e2e/errutil.sh | e2e-script | medium | e2e | P1 | Valuable contracts, depends on local stack/ports. |
| tests/test_logger_e2e.sh | mixed script | medium | integration | P1 | Mixes build/grep checks, should be split. |
| test/e2e/dsl_multiword.sh | integration/e2e script | medium | e2e | P2 | Heavy, but useful end-to-end DSL smoke. |
| test/e2e/test_failures.sh | resilience e2e-script | good | e2e | P1 | Stops critical services (`datamanager`, `orchestrator`, `dsl-scala`, `query-builder`, `downloader`) and verifies fail/recovery behavior through gateway. |
| tests/opensearch_test.sh | removed | n/a | drop | done | Removed due to external dependency on `cloud-1`. |
| test/diagnose/opensearch_templates.sh | external integration | bad | load | P2 | Diagnostic inspector, not CI-suitable. |
| test/load/load_test.sh | load | bad | load | P2 | Throughput benchmark, not regression test. |
| test/load/stress_test.sh | load | bad | load | P2 | Stress utility, not deterministic CI test. |
| tests/test_compliance.sh | removed | n/a | drop | done | Removed as legacy compliance script. |
| tests/compliance_runner.go | removed | n/a | drop | done | Removed as legacy ignored diagnostic utility. |
| test/utils/grpc_check.go | diagnostic utility | medium | e2e | P2 | Useful helper for smoke checks. |
| tests/diagnose/main.go | removed | n/a | drop | done | Removed as legacy diagnostics tool with outdated ports/endpoints. |
| smoke.sh | removed | n/a | drop | done | Removed (referenced non-existent Make targets). |
| dsl-scala/src/test/scala/BookQueryParserSpec.scala | scala-unit | good | unit | P2 | Basic parser coverage in Scala test suite. |
| query-builder/src/test/scala/OsDslBuilderSpec.scala | scala-unit | good | unit | P2 | Basic JSON generation checks for AST mapping. |
