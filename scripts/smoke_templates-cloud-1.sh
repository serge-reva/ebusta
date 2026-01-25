#!/bin/bash
# Настройки для хоста yuno
BASE="http://cloud-1:9200"
INDEX="ebusta_merged_index"

echo "🧪 Running Smoke Tests for Search Templates..."
echo "--------------------------------------------"

# 1. Смешанный поиск (Mixed Search)
echo -n "Test 1: Mixed search (query: 'приключения') -> "
curl -s -H "Content-Type: application/json" \
  -XPOST "${BASE}/${INDEX}/_search/template" \
  -d '{
    "id": "fl_mixed_search",
    "params": {
      "query": "приключения",
      "from": 0,
      "size": 2
    }
  }' | jq '.hits.total.value'

# 2. Список авторов (Authors aggregations)
# В твоем шаблоне используется ID 'fl_authors_all'
echo -n "Test 2: Authors aggregation (composite) -> "
curl -s -H "Content-Type: application/json" \
  -XPOST "${BASE}/${INDEX}/_search/template" \
  -d '{
    "id": "fl_authors_all",
    "params": {
      "size": 5
    }
  }' | jq '.aggregations.authors.buckets | length'

# 3. Список названий (Titles aggregations)
# В твоем шаблоне используется ID 'fl_titles_all'
echo -n "Test 3: Titles aggregation (composite) -> "
curl -s -H "Content-Type: application/json" \
  -XPOST "${BASE}/${INDEX}/_search/template" \
  -d '{
    "id": "fl_titles_all",
    "params": {
      "size": 5
    }
  }' | jq '.aggregations.titles.buckets | length'

# 4. Поиск по подстроке (Substring)
# В твоем шаблоне используется ID 'fl_title_substring'
echo -n "Test 4: Title substring search (query: 'остров') -> "
curl -s -H "Content-Type: application/json" \
  -XPOST "${BASE}/${INDEX}/_search/template" \
  -d '{
    "id": "fl_title_substring",
    "params": {
      "query": "остров",
      "size": 5
    }
  }' | jq '.hits.hits | length'

echo "--------------------------------------------"
echo "✅ Smoke tests finished."
