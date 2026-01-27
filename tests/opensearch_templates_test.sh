#!/bin/bash
# OpenSearch Templates Inspector
# Lists and analyzes all search templates in OpenSearch

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Configuration
OS_URL="${OPENSEARCH_URL:-http://cloud-1:9200}"
INDEX_NAME="${INDEX_NAME:-flibusta_merged_index}"

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  OpenSearch Templates Inspector${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""
echo "OpenSearch URL: $OS_URL"
echo "Index: $INDEX_NAME"
echo ""

# Get all templates
echo -e "${CYAN}Fetching all search templates...${NC}"
TEMPLATES=$(curl -s "$OS_URL/_scripts")

# Check if we got any templates
TEMPLATE_COUNT=$(echo "$TEMPLATES" | jq 'keys | length')

if [ "$TEMPLATE_COUNT" -eq 0 ]; then
    echo -e "${RED}✗ No search templates found${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Found $TEMPLATE_COUNT template(s)${NC}"
echo ""

# List all template IDs
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  Template IDs${NC}"
echo -e "${BLUE}======================================${NC}"
echo "$TEMPLATES" | jq -r 'keys[]' | while read -r template_id; do
    echo -e "  ${CYAN}→${NC} $template_id"
done
echo ""

# Analyze each template
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  Template Details${NC}"
echo -e "${BLUE}======================================${NC}"

TEMPLATE_IDS=$(echo "$TEMPLATES" | jq -r 'keys[]')
TEMPLATE_NUM=1

for template_id in $TEMPLATE_IDS; do
    echo ""
    echo -e "${MAGENTA}[$TEMPLATE_NUM/$TEMPLATE_COUNT] Template: ${YELLOW}$template_id${NC}"
    echo -e "${BLUE}────────────────────────────────────${NC}"
    
    TEMPLATE_DATA=$(curl -s "$OS_URL/_scripts/$template_id")
    
    # Extract template info
    LANG=$(echo "$TEMPLATE_DATA" | jq -r '.script.lang')
    SOURCE=$(echo "$TEMPLATE_DATA" | jq -r '.script.source')
    
    echo -e "${CYAN}Language:${NC} $LANG"
    echo ""
    
    # Pretty print the template source
    echo -e "${CYAN}Template Source:${NC}"
    if [ "$LANG" = "mustache" ]; then
        # Try to parse and pretty-print the Mustache template
        echo "$SOURCE" | jq '.' 2>/dev/null || echo "$SOURCE"
    else
        echo "$SOURCE"
    fi
    
    echo ""
    
    # Analyze template parameters
    echo -e "${CYAN}Parameters Used:${NC}"
    PARAMS=$(echo "$SOURCE" | grep -oP '\{\{[^}]+\}\}' | sort -u)
    if [ -n "$PARAMS" ]; then
        echo "$PARAMS" | while read -r param; do
            # Remove {{ and }}
            PARAM_NAME=$(echo "$param" | sed 's/{{//g' | sed 's/}}//g' | sed 's/[^}]*//g')
            echo -e "  ${GREEN}•${NC} $param"
        done
    else
        echo -e "  ${YELLOW}(no parameters)${NC}"
    fi
    
    echo ""
    
    # Analyze query structure
    echo -e "${CYAN}Query Structure:${NC}"
    if echo "$SOURCE" | jq -e '.query' > /dev/null 2>&1; then
        QUERY_TYPE=$(echo "$SOURCE" | jq -r '.query | keys[0]')
        echo -e "  Type: ${GREEN}$QUERY_TYPE${NC}"
        
        # Extract fields being searched
        if echo "$SOURCE" | jq -e '.query.multi_match.fields' > /dev/null 2>&1; then
            echo -e "  Fields:"
            echo "$SOURCE" | jq -r '.query.multi_match.fields[]' | while read -r field; do
                echo -e "    ${GREEN}•${NC} $field"
            done
        fi
    else
        echo -e "  ${YELLOW}(parsing as Mustache)${NC}"
        if echo "$SOURCE" | grep -q "multi_match"; then
            echo -e "  Type: ${GREEN}multi_match${NC}"
        elif echo "$SOURCE" | grep -q "match"; then
            echo -e "  Type: ${GREEN}match${NC}"
        elif echo "$SOURCE" | grep -q "query_string"; then
            echo -e "  Type: ${GREEN}query_string${NC}"
        fi
    fi
    
    echo ""
    
    # Test the template with sample data
    echo -e "${CYAN}Testing Template:${NC}"
    
    # Test 1: Simple query
    TEST_QUERY="Кинг"
    echo -e "  ${YELLOW}Test 1:${NC} Simple query (\"$TEST_QUERY\")"
    TEST_RESULT=$(curl -s -X POST "$OS_URL/$INDEX_NAME/_search/template" \
        -H 'Content-Type: application/json' \
        -d "{
            \"id\": \"$template_id\",
            \"params\": {
                \"query\": \"$TEST_QUERY\",
                \"from\": 0,
                \"size\": 3
            }
        }")
    
    TEST_HITS=$(echo "$TEST_RESULT" | jq -r '.hits.total.value // 0')
    if [ "$TEST_HITS" -gt 0 ]; then
        echo -e "    ${GREEN}✓ Found: $TEST_HITS results${NC}"
        echo "$TEST_RESULT" | jq -r '.hits.hits[0:2][] | "      - \(._source.title // "N/A")"'
    else
        echo -e "    ${RED}✗ No results${NC}"
    fi
    
    # Test 2: DSL-style query (if applicable)
    TEST_DSL_QUERY="author:Кинг"
    echo -e "  ${YELLOW}Test 2:${NC} DSL query (\"$TEST_DSL_QUERY\")"
    TEST_DSL_RESULT=$(curl -s -X POST "$OS_URL/$INDEX_NAME/_search/template" \
        -H 'Content-Type: application/json' \
        -d "{
            \"id\": \"$template_id\",
            \"params\": {
                \"query\": \"$TEST_DSL_QUERY\",
                \"from\": 0,
                \"size\": 3
            }
        }")
    
    TEST_DSL_HITS=$(echo "$TEST_DSL_RESULT" | jq -r '.hits.total.value // 0')
    if [ "$TEST_DSL_HITS" -gt 0 ]; then
        echo -e "    ${GREEN}✓ Found: $TEST_DSL_HITS results${NC}"
        echo -e "    ${GREEN}✓ Template supports DSL syntax${NC}"
    else
        echo -e "    ${YELLOW}⚠ No results (Template may not support DSL syntax)${NC}"
    fi
    
    TEMPLATE_NUM=$((TEMPLATE_NUM + 1))
done

# Summary
echo ""
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  Summary${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""
echo "Total templates found: $TEMPLATE_COUNT"
echo ""

# Recommendations
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  Recommendations${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""

# Check if any template supports DSL
DSL_SUPPORT=false
for template_id in $TEMPLATE_IDS; do
    TEST_DSL_RESULT=$(curl -s -X POST "$OS_URL/$INDEX_NAME/_search/template" \
        -H 'Content-Type: application/json' \
        -d "{
            \"id\": \"$template_id\",
            \"params\": {
                \"query\": \"author:Кинг\",
                \"from\": 0,
                \"size\": 1
            }
        }")
    
    TEST_DSL_HITS=$(echo "$TEST_DSL_RESULT" | jq -r '.hits.total.value // 0')
    if [ "$TEST_DSL_HITS" -gt 0 ]; then
        DSL_SUPPORT=true
        break
    fi
done

if [ "$DSL_SUPPORT" = true ]; then
    echo -e "${GREEN}✓ At least one template supports DSL syntax${NC}"
    echo "  You can use queries like: author:Кинг, title:Unix, etc."
else
    echo -e "${YELLOW}⚠ No templates support DSL syntax natively${NC}"
    echo ""
    echo "  Current templates use simple multi_match queries."
    echo "  To support DSL syntax (author:, title:, etc), you need:"
    echo ""
    echo -e "  ${CYAN}Option 1:${NC} Use DSL-Converter service"
    echo "    Flow: Web -> Orch -> DSL-Converter -> DataManager -> OpenSearch"
    echo ""
    echo -e "  ${CYAN}Option 2:${NC} Create query_string based template"
    echo "    Example template using query_string parser:"
    echo '    {
      "query": {
        "query_string": {
          "query": "{{query}}",
          "fields": ["title^3", "authors", "annotation"],
          "default_operator": "AND"
        }
      }
    }'
fi

echo ""
echo -e "${BLUE}======================================${NC}"
echo "Test completed at $(date)"
echo -e "${BLUE}======================================${NC}"

