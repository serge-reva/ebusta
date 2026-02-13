# Спецификация входных и выходных данных DSL-парсера

## 1. Входные данные (Input)
Входом является текстовая строка поискового запроса в формате DSL. Поддерживаются:
* **Логические операторы**: `AND`, `OR`, `NOT` (регистрозависимые).
* **Группировка**: Круглые скобки `(...)`.
* **Поля (Scoped Search)**: `field:value` или `field:"multi word value"`.
* **Регулярные выражения**: `/pattern/`.
* **Полнотекстовый поиск**: Слова без указания поля (автоматически получают поле `any`).

**Пример сложного входа:**
`author:"Стивен Кинг" AND (title:Куджо OR tags:/horror/)`

---

## 2. Промежуточное представление (S-Expression)
Внутри ядра на Common Lisp строка преобразуется в дерево (S-expression). Это формат, используемый для отладки и внутренней логики трансляции.

**Результат трансформации:**
```lisp
(:AND 
  (:FIELD "author" "Стивен Кинг") 
  (:OR 
    (:FIELD "title" "Куджо") 
    (:FIELD "tags" "horror" :OP :REGEX)))
3. Выходные данные (gRPC / Protobuf)
Финальный выход сервиса — это бинарный объект Protobuf. Ниже представлено JSON-представление этого объекта, которое получает клиент.

Пример команды для получения выхода:
Bash

~/projects/ebusta/lisp-converter/grpcurl -plaintext \
    -import-path ~/projects/ebusta/lisp-converter \
    -proto ~/projects/ebusta/lisp-converter/search.proto \
    -d '{"raw_query": "author:\"Стивен Кинг\" AND (title:Куджо OR tags:/horror/)"}' \
    localhost:50052 ebusta.library.v1.MessageConverter/Convert
Структура ответа (JSON View):
JSON

{
  "requestId": "req-1737898956",
  "logical": {
    "op": "AND",
    "nodes": [
      {
        "filter": {
          "field": "author",
          "value": "Стивен Кинг",
          "operator": "EQUALS"
        }
      },
      {
        "logical": {
          "op": "OR",
          "nodes": [
            {
              "filter": {
                "field": "title",
                "value": "Куджо",
                "operator": "EQUALS"
              }
            },
            {
              "filter": {
                "field": "tags",
                "value": "horror",
                "operator": "REGEX"
              }
            }
          ]
        }
      }
    ]
  },
  "canonicalForm": "(:AND (:FIELD \"author\" \"Стивен Кинг\") (:OR (:FIELD \"title\" \"Куджо\") (:FIELD \"tags\" \"horror\" :OP :REGEX)))"
}
4. Заметки для SRE
request_id: Генерируется сервером для трассировки запроса через все компоненты системы.

canonical_form: Строковое представление нормализованного дерева. Идеально подходит для использования в качестве ключа кэширования (Redis/In-memory).

operator: Перечисление (Enum), которое жестко задает тип поиска (EQUALS, REGEX и т.д.), избавляя нижележащие сервисы от необходимости повторного анализа строки. EOF
