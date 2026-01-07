#!/bin/bash
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${GREEN}>>> Starting Meat Grinder Local Test...${NC}"

# 1. Проверка бинарника
if [ ! -f "./bin/bulker" ]; then
    echo -e "${RED}Error: Binary ./bin/bulker not found. Run 'make build' first.${NC}"
    exit 1
fi

# 2. Поиск тестового файла
TEST_FILE=$(ls ./data/src/*.zip | head -n 1)
if [ -z "$TEST_FILE" ]; then
    echo -e "${RED}Error: No ZIP files in ./data/src/${NC}"
    exit 1
fi

echo -e "Target file: $(basename "$TEST_FILE")"

# 3. Пробный запуск (парсим первые 5 документов)
echo -n "Processing... "
./bin/bulker -config ./config.yaml -src "$TEST_FILE" | head -n 10 > ./data/out/smoke_test.jsonl

if [ -s ./data/out/smoke_test.jsonl ]; then
    echo -e "${GREEN}SUCCESS${NC}"
    echo -e "Sample output saved to ./data/out/smoke_test.jsonl"
    # Покажем одну строку для контроля
    echo -e "${GREEN}Result preview:${NC}"
    head -n 2 ./data/out/smoke_test.jsonl | cut -c1-120...
else
    echo -e "${RED}FAILED${NC} (No output generated)"
    exit 1
fi
