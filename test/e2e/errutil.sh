#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# Integration tests for errutil module
# Tests: TraceID propagation, JSON error format, HTTP headers

BASE_URL="${BASE_URL:-http://localhost:3000}"
GATEWAY_URL="${GATEWAY_URL:-http://localhost:8443}"
DOWNLOADER_URL="${DOWNLOADER_URL:-http://localhost:50081}"

echo "=== errutil E2E Tests ==="
echo "web-frontend: $BASE_URL"
echo "gateway:      $GATEWAY_URL"
echo "downloader:   $DOWNLOADER_URL"
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
# Test 1: web-frontend returns JSON error with TraceID
# ============================================
test_web_frontend_error_format() {
    local resp
    resp=$(curl -sS "$BASE_URL/download/invalid_sha1_format" 2>&1) || true
    
    # Check JSON structure
    echo "$resp" | grep -q '"error"' || return 1
    echo "$resp" | grep -q '"code"' || return 1
    echo "$resp" | grep -q '"message"' || return 1
    echo "$resp" | grep -q '"trace_id"' || return 1
    
    # Check error code
    echo "$resp" | grep -q '"INVALID_ARGUMENT"' || return 1
    
    return 0
}

# ============================================
# Test 2: web-frontend returns X-Trace-Id header
# ============================================
test_web_frontend_trace_header() {
    local headers
    headers=$(curl -sSI "$BASE_URL/download/0000000000000000000000000000000000000000" 2>&1) || true
    
    # Check X-Trace-Id header present
    echo "$headers" | grep -iq "X-Trace-Id:" || return 1
    
    return 0
}

# ============================================
# Test 3: gateway returns JSON with TraceID
# ============================================
test_gateway_trace_id() {
    local resp
    resp=$(curl -sS -X POST "$GATEWAY_URL/search" \
      -H 'Content-Type: application/json' \
      -d '{"query":"test","page":1,"limit":1}' 2>&1) || true
    
    echo "$resp" | grep -q '"trace_id"' || return 1
    
    return 0
}

# ============================================
# Test 4: downloader returns JSON error for not found
# ============================================
test_downloader_error_format() {
    local resp
    resp=$(curl -sS "$DOWNLOADER_URL/books/0000000000000000000000000000000000000000" 2>&1) || true
    
    # Should return JSON error (NOT_FOUND)
    echo "$resp" | grep -q '"error"' || return 1
    echo "$resp" | grep -q '"code"' || return 1
    echo "$resp" | grep -q '"trace_id"' || return 1
    
    return 0
}

# ============================================
# Test 5: downloader returns X-Trace-Id header
# ============================================
test_downloader_trace_header() {
    local headers
    headers=$(curl -sSI "$DOWNLOADER_URL/books/0000000000000000000000000000000000000000" 2>&1) || true
    
    echo "$headers" | grep -iq "X-Trace-Id:" || return 1
    
    return 0
}

# ============================================
# Test 6: TraceID format is correct
# ============================================
test_trace_id_format() {
    local resp
    resp=$(curl -sS "$BASE_URL/download/invalid" 2>&1) || true
    
    # Extract trace_id and check format (should be like "wf-1234567890")
    local trace_id
    trace_id=$(echo "$resp" | grep -o '"trace_id":"[^"]*"' | cut -d'"' -f4)
    
    if [ -z "$trace_id" ]; then
        return 1
    fi
    
    # Check prefix
    case "$trace_id" in
        wf-*|dl-*|cli-*|wa-*|orch-*|tier-*|plasma-*|arch-*)
            return 0
            ;;
        *)
            echo "Unexpected trace_id format: $trace_id"
            return 1
            ;;
    esac
}

# ============================================
# Test 7: Error code mapping (NOT_FOUND -> 404)
# ============================================
test_not_found_http_code() {
    local http_code
    http_code=$(curl -sSo /dev/null -w "%{http_code}" "$DOWNLOADER_URL/books/0000000000000000000000000000000000000000" 2>&1) || true
    
    # NOT_FOUND should map to 404
    [ "$http_code" = "404" ] || return 1
    
    return 0
}

# ============================================
# Test 8: Invalid argument maps to 400
# ============================================
test_invalid_argument_http_code() {
    local http_code
    http_code=$(curl -sSo /dev/null -w "%{http_code}" "$BASE_URL/download/bad_format" 2>&1) || true
    
    # INVALID_ARGUMENT should map to 400
    [ "$http_code" = "400" ] || return 1
    
    return 0
}

# ============================================
# Run all tests
# ============================================
run_test "web-frontend error format" test_web_frontend_error_format
run_test "web-frontend X-Trace-Id header" test_web_frontend_trace_header
run_test "gateway TraceID" test_gateway_trace_id
run_test "downloader error format" test_downloader_error_format
run_test "downloader X-Trace-Id header" test_downloader_trace_header
run_test "TraceID format" test_trace_id_format
run_test "NOT_FOUND -> 404" test_not_found_http_code
run_test "INVALID_ARGUMENT -> 400" test_invalid_argument_http_code

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
