#!/bin/bash
# OpenSearch Integration Test for Ebusta
# Tests OpenSearch connectivity, index, template, and search functionality

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
OS_URL="${OPENSEARCH_URL:-http://cloud-1:9200}"
INDEX_NAME="${INDEX_NAME:-flibusta_merged_index}"
TEMPLATE_ID="fl_mixed_search"

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  OpenSearch Integration Test${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""
echo "OpenSearch URL: $OS_URL"
echo "Index: $INDEX_NAME"
echo ""

# Test counter
PASSED=0
FAILED=0

# Helper function to run test
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo -e "${YELLOW}▶ Testing: ${test_name}${NC}"
    
    if eval "$test_command" > /tmp/test_output.json 2>&1; then
        echo -e "${GREEN}✓ PASSED${NC}"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ FAILED${NC}"
        cat /tmp/test_output.json
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# Test 1: OpenSearch connectivity
echo -e "\n${BLUE}[1/7] OpenSearch Connectivity${NC}"
run_test "Connection to OpenSearch" \
    "curl -s -f '$OS_URL/' | jq -e '.version.number' > /dev/null"

if [ $? -eq 0 ]; then
    VERSION=$(curl -s "$OS_URL/" | jq -r '.version.number')
    echo "    Version: $VERSION"
fi

# Test 2: Index exists and has documents
echo -e "\n${BLUE}[2/7] Index Status${NC}"
run_test "Index exists and has documents" \
    "curl -s '$OS_URL/$INDEX_NAME/_count' | jq -e '.count > 0'"

if [ $? -eq 0 ]; then
    COUNT=$(curl -s "$OS_URL/$INDEX_NAME/_count" | jq -r '.count')
    echo "    Document count: $COUNT"
fi

# Test 3: Search template exists
echo -e "\n${BLUE}[3/7] Search Template${NC}"
run_test "Template '$TEMPLATE_ID' exists" \
    "curl -s '$OS_URL/_scripts/$TEMPLATE_ID' | jq -e '.found == true'"

if [ $? -eq 0 ]; then
    echo "    Template structure:"
    curl -s "$OS_URL/_scripts/$TEMPLATE_ID" | jq -r '.script.source' | head -3
fi

# Test 4: Simple search by author
echo -e "\n${BLUE}[4/7] Simple Author Search${NC}"
SIMPLE_SEARCH=$(curl -s -X POST "$OS_URL/$INDEX_NAME/_search" \
    -H 'Content-Type: application/json' \
    -d '{
        "query": {
            "match": {
                "authors": "Кинг"
            }
        },
        "size": 5
    }')

SIMPLE_HITS=$(echo "$SIMPLE_SEARCH" | jq -r '.hits.total.value')
run_test "Simple search finds results" \
    "[ $SIMPLE_HITS -gt 0 ]"

if [ $? -eq 0 ]; then
    echo "    Found: $SIMPLE_HITS results"
    echo "    Sample titles:"
    echo "$SIMPLE_SEARCH" | jq -r '.hits.hits[0:3][] | "      - \(._source.title) by \(._source.authors | join(", "))"'
fi

# Test 5: Template search with simple query
echo -e "\n${BLUE}[5/7] Template Search (simple)${NC}"
TEMPLATE_SIMPLE=$(curl -s -X POST "$OS_URL/$INDEX_NAME/_search/template" \
    -H 'Content-Type: application/json' \
    -d "{
        \"id\": \"$TEMPLATE_ID\",
        \"params\": {
            \"query\": \"Кинг\",
            \"from\": 0,
            \"size\": 5
        }
    }")

TEMPLATE_SIMPLE_HITS=$(echo "$TEMPLATE_SIMPLE" | jq -r '.hits.total.value')
run_test "Template search (simple) finds results" \
    "[ $TEMPLATE_SIMPLE_HITS -gt 0 ]"

if [ $? -eq 0 ]; then
    echo "    Found: $TEMPLATE_SIMPLE_HITS results"
fi

# Test 6: Template search with DSL syntax (author:)
echo -e "\n${BLUE}[6/7] Template Search (DSL syntax)${NC}"
TEMPLATE_DSL=$(curl -s -X POST "$OS_URL/$INDEX_NAME/_search/template" \
    -H 'Content-Type: application/json' \
    -d "{
        \"id\": \"$TEMPLATE_ID\",
        \"params\": {
            \"query\": \"author:Кинг\",
            \"from\": 0,
            \"size\": 5
        }
    }")

TEMPLATE_DSL_HITS=$(echo "$TEMPLATE_DSL" | jq -r '.hits.total.value')

if [ $TEMPLATE_DSL_HITS -eq 0 ]; then
    echo -e "${YELLOW}⚠ WARNING: Template does NOT support DSL syntax (author:)${NC}"
    echo "    The template treats 'author:Кинг' as literal text"
    echo "    This means DSL-converter is REQUIRED for proper search"
    FAILED=$((FAILED + 1))
else
    echo -e "${GREEN}✓ Template supports DSL syntax${NC}"
    echo "    Found: $TEMPLATE_DSL_HITS results"
    PASSED=$((PASSED + 1))
fi

# Test 7: Document structure validation
echo -e "\n${BLUE}[7/7] Document Structure${NC}"
SAMPLE_DOC=$(curl -s "$OS_URL/$INDEX_NAME/_search?size=1" | jq '.hits.hits[0]._source')

run_test "Document has required fields" \
    "echo '$SAMPLE_DOC' | jq -e 'has(\"title\") and has(\"fileInfo\")'"

if [ $? -eq 0 ]; then
    echo "    Sample document structure:"
    echo "$SAMPLE_DOC" | jq '{title, authors, fileInfo: {container, filename, size}}'
fi

# Summary
echo ""
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  Test Summary${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "Total tests: $((PASSED + FAILED))"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"
echo ""

# Important findings
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  Key Findings${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""
echo "1. OpenSearch Status: ✓ Working"
echo "2. Index '$INDEX_NAME': $COUNT documents"
echo "3. Simple search (\"Кинг\"): ✓ Works ($SIMPLE_HITS results)"
echo "4. Template search (\"Кинг\"): ✓ Works ($TEMPLATE_SIMPLE_HITS results)"
echo ""

if [ $TEMPLATE_DSL_HITS -eq 0 ]; then
    echo -e "${YELLOW}⚠ IMPORTANT:${NC}"
    echo "   Template does NOT support DSL syntax (author:, title:, etc)"
    echo "   DSL-Converter is REQUIRED to translate queries"
    echo ""
    echo "   Current flow:"
    echo "   User -> Web -> Orchestrator -> DataManager -> OpenSearch"
    echo "                                                  ↓"
    echo "                                          'author:Кинг' = 0 results"
    echo ""
    echo "   Required flow:"
    echo "   User -> Web -> Orchestrator -> DSL-Converter -> DataManager -> OpenSearch"
    echo "                                   ↓"
    echo "                           Parses 'author:Кинг'"
    echo "                           to proper query"
else
    echo -e "${GREEN}✓ Template supports DSL syntax${NC}"
    echo "   No DSL-Converter needed for basic queries"
fi

echo ""

# Exit with failure if any test failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi

exit 0
