#!/bin/bash

OS_URL="http://cloud-1:9200"
TEMPLATE_DIR="opensearch/templates"
mkdir -p $TEMPLATE_DIR

echo "🛠 1. Updating template files locally..."

# Основной поиск с дедупликацией
cat << 'JSON' > $TEMPLATE_DIR/fl_mixed_search.json
{
  "script": {
    "lang": "mustache",
    "source": {
      "query": {
        "multi_match": {
          "query": "{{query}}",
          "fields": ["title^3", "authors", "annotation"],
          "type": "best_fields",
          "fuzziness": "AUTO"
        }
      },
      "collapse": {
        "field": "title.kw",
        "inner_hits": { "name": "best", "size": 1, "sort": [{"fileInfo.size": "desc"}] }
      },
      "from": "{{from}}{{^from}}0{{/from}}",
      "size": "{{size}}{{^size}}10{{/size}}"
    }
  }
}
JSON

# Точный поиск автора
cat << 'JSON' > $TEMPLATE_DIR/fl_author_exact.json
{
  "script": {
    "lang": "mustache",
    "source": {
      "query": { "term": { "authors.kw": "{{author}}" } },
      "collapse": { "field": "title.kw", "inner_hits": { "name": "best", "size": 1, "sort": [{"fileInfo.size": "desc"}] } },
      "size": "{{size}}{{^size}}20{{/size}}"
    }
  }
}
JSON

echo "🚀 2. Deploying to OpenSearch..."
for file in $TEMPLATE_DIR/*.json; do
    name=$(basename "$file" .json)
    status=$(curl -s -o /dev/null -w "%{http_code}" -X POST "$OS_URL/_scripts/$name" -H 'Content-Type: application/json' -d @"$file")
    if [ "$status" == "200" ]; then echo "  ✅ $name: Uploaded"; else echo "  ❌ $name: Error $status"; exit 1; fi
done

echo "🔍 3. Running Smoke Tests..."

# Тест 1: Проверка смешанного поиска
echo -n "  🧪 Test fl_mixed_search... "
TEST_SEARCH=$(curl -s -X POST "$OS_URL/flibusta_merged_index/_search/template" \
    -H 'Content-Type: application/json' \
    -d '{"id":"fl_mixed_search","params":{"query":"Мастер и Маргарита"}}')

HITS_COUNT=$(echo "$TEST_SEARCH" | jq '.hits.total.value // 0')
HAS_INNER_HITS=$(echo "$TEST_SEARCH" | jq 'if .hits.hits[0].inner_hits then true else false end')

if [ "$HITS_COUNT" -gt 0 ] && [ "$HAS_INNER_HITS" == "true" ]; then
    echo "PASS ($HITS_COUNT hits, deduplication active)"
else
    echo "FAIL (Hits: $HITS_COUNT, InnerHits: $HAS_INNER_HITS)"
    exit 1
fi

# Тест 2: Проверка точного автора
echo -n "  🧪 Test fl_author_exact... "
TEST_AUTHOR=$(curl -s -X POST "$OS_URL/flibusta_merged_index/_search/template" \
    -H 'Content-Type: application/json' \
    -d '{"id":"fl_author_exact","params":{"author":"михаил булгаков"}}')

AUTHOR_COUNT=$(echo "$TEST_AUTHOR" | jq '.hits.total.value // 0')

if [ "$AUTHOR_COUNT" -gt 0 ]; then
    echo "PASS ($AUTHOR_COUNT books found)"
else
    echo "FAIL (No books found for author)"
    exit 1
fi

echo "🎉 All systems green!"
