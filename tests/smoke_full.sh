#!/bin/bash

# ==========================================
# CONFIGURATION
# ==========================================
# IP адрес OpenSearch (из твоего скрипта)
OS_HOST="cloud-1:9200"
INDEX="flibusta_merged_index"
CLI="./bin/ebusta-cli"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Проверка зависимостей
if ! command -v jq &> /dev/null; then
    echo -e "${RED}Error: jq is not installed.${NC} Please run 'sudo apt install jq'"
    exit 1
fi

if [ ! -f "$CLI" ]; then
    echo -e "${RED}Error: Binary $CLI not found.${NC} Run 'make build' first."
    exit 1
fi

echo -e "${YELLOW}🚀 STARTING FULL SMOKE TEST SUITE${NC}"
echo "OpenSearch Host: $OS_HOST"
echo "Target Index:    $INDEX"
echo "-----------------------------------------------------"

# ==========================================
# PART 1: DIRECT OPENSEARCH TEMPLATE TESTS
# ==========================================
echo -e "\n${YELLOW}📡 [LEVEL 1] Direct OpenSearch Template Tests (via curl)${NC}"

# Функция для проверки шаблона
check_template() {
    local test_name="$1"
    local template_id="$2"
    local param_value="$3"
    
    echo -n "   👉 $test_name ... "
    
    # ВАЖНО: Мы используем параметр "q", так как унифицировали это в коде ранее
    response=$(curl -s -H "Content-Type: application/json" \
      -XPOST "http://${OS_HOST}/${INDEX}/_search/template" \
      -d "{
        \"id\": \"$template_id\",
        \"params\": {
          \"q\": \"$param_value\", 
          \"from\": 0,
          \"size\": 1
        }
      }")
    
    # Извлекаем количество хитов
    hits=$(echo "$response" | jq '.hits.total.value')
    
    if [[ "$hits" != "null" && "$hits" -gt 0 ]]; then
        echo -e "${GREEN}[PASS]${NC} (Hits: $hits)"
    else
        echo -e "${RED}[FAIL]${NC} (Hits: $hits)"
        # echo "Response: $response" # Раскомментируй для отладки
    fi
}

# Запуск тестов шаблонов
check_template "Mixed Search ('приключения')" "fl_mixed_search" "приключения"
check_template "Author Search ('Кинг')"      "fl_authors_all"  "Кинг"
check_template "Title Search ('Туман')"      "fl_titles_all"   "Туман"
check_template "Title Substring ('остров')"  "fl_title_substring" "остров"


# ==========================================
# PART 2: FULL PIPELINE TESTS (CLI)
# ==========================================
echo -e "\n${YELLOW}🏭 [LEVEL 2] Full Pipeline Tests (CLI -> Orch -> Proc -> Converter -> OS)${NC}"

run_cli_test() {
    local test_name="$1"
    local query="$2"
    local expected_pattern="$3"
    
    echo -n "   👉 $test_name ... "

    # Запускаем CLI
    output=$($CLI "$query" 2>&1)
    
    # Проверка на наличие ID (означает, что книги вернулись)
    if echo "$output" | grep -q "$expected_pattern"; then
        echo -e "${GREEN}[PASS]${NC}"
    else
        echo -e "${RED}[FAIL]${NC}"
        echo "      Command: $CLI \"$query\""
        echo "      Output head: $(echo "$output" | head -n 2)"
    fi
}

# Запуск тестов пайплайна
run_cli_test "Simple Text ('Кинг')"          "Кинг"                    "ID"
run_cli_test "Smart Author ('author:Кинг')"  "author:Кинг"             "ID"
run_cli_test "Smart Title ('title:Туман')"   "title:Туман"             "ID"
run_cli_test "Complex Logic (AND)"           "author:Кинг AND title:Туман" "ID"

echo "-----------------------------------------------------"
echo -e "${GREEN}✅ All tests finished.${NC}"
