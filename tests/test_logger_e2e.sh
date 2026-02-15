#!/usr/bin/env bash
set -euo pipefail

# Integration tests for logger module
# Tests: config integration, TraceID propagation, output format

echo "=== Logger E2E Tests ==="
echo ""

# Test counter
TESTS_RUN=0
TESTS_PASS=0
TESTS_FAIL=0

run_test() {
    local name="$1"
    shift
    TESTS_RUN=$((TESTS_RUN + 1))
    echo -n "[$TESTS_RUN] $name... "
    if "$@"; then
        TESTS_PASS=$((TESTS_PASS + 1))
        echo "PASS"
        return 0
    else
        TESTS_FAIL=$((TESTS_FAIL + 1))
        echo "FAIL"
        return 1
    fi
}

# ============================================
# Test 1: Logger compiles with config
# ============================================
test_logger_compiles() {
    go build ./internal/logger/... 2>&1 || return 1
    return 0
}

# ============================================
# Test 2: Logger unit tests pass
# ============================================
test_logger_unit_tests() {
    go test ./internal/logger/... 2>&1 || return 1
    return 0
}

# ============================================
# Test 3: LoggerConfig exists in config
# ============================================
test_logger_config_exists() {
    grep -q "LoggerConfig" internal/config/config.go || return 1
    grep -q "Logger LoggerConfig" internal/config/config.go || return 1
    return 0
}

# ============================================
# Test 4: All logger files exist
# ============================================
test_logger_files_exist() {
    [ -f internal/logger/level.go ] || return 1
    [ -f internal/logger/output.go ] || return 1
    [ -f internal/logger/formatter.go ] || return 1
    [ -f internal/logger/logger.go ] || return 1
    [ -f internal/logger/context.go ] || return 1
    [ -f internal/logger/config.go ] || return 1
    [ -f internal/logger/global.go ] || return 1
    return 0
}

# ============================================
# Test 5: TraceID generation works (via go test)
# ============================================
test_trace_id_generation() {
    go test ./internal/logger/... -run TestGenerateTraceID 2>&1 || return 1
    return 0
}

# ============================================
# Test 6: Config loads logger section
# ============================================
test_config_loads_logger() {
    if [ -f ebusta.yaml ]; then
        go build ./internal/config/... 2>&1 || return 1
    fi
    return 0
}

# ============================================
# Test 7: Logger integration with components
# ============================================
test_components_compile_with_logger() {
    go build ./cmd/web-frontend/... 2>&1 || return 1
    go build ./cmd/orchestrator/... 2>&1 || return 1
    go build ./cmd/downloader/... 2>&1 || return 1
    return 0
}

# ============================================
# Test 8: errutil integration (TraceID compatibility)
# ============================================
test_errutil_trace_compatibility() {
    grep -q "TraceIDFromContext" internal/logger/context.go || return 1
    grep -q "ContextWithTraceID" internal/logger/context.go || return 1
    return 0
}

# ============================================
# Run all tests
# ============================================
run_test "Logger compiles" test_logger_compiles
run_test "Logger unit tests" test_logger_unit_tests
run_test "LoggerConfig in config" test_logger_config_exists
run_test "Logger files exist" test_logger_files_exist
run_test "TraceID generation" test_trace_id_generation
run_test "Config loads logger" test_config_loads_logger
run_test "Components compile with logger" test_components_compile_with_logger
run_test "errutil TraceID compatibility" test_errutil_trace_compatibility

# ============================================
# Summary
# ============================================
echo ""
echo "=== Summary ==="
echo "Tests run:  $TESTS_RUN"
echo "Tests pass: $TESTS_PASS"
echo "Tests fail: $TESTS_FAIL"

if [ "$TESTS_FAIL" -gt 0 ]; then
    echo ""
    echo "Some tests failed!"
    exit 1
fi

echo ""
echo "All tests passed!"
exit 0
