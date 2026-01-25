Цель: Перевод Processor на полную поддержку Ebusta Search DSL v1.1 через обход дерева SearchQuery.
+3

1. Рефакторинг контракта взаимодействия
Изменить логику обработки в cmd/processor/main.go , чтобы сервис извлекал поле query типа SearchQuery из входящего сообщения UnmarshaledMessage.
+4

Обеспечить передачу структурированного объекта SearchQuery от Message-Converter к Processor через gRPC.
+3

2. Реализация компонента AST Walker
Разработать рекурсивную функцию обхода дерева SearchQuery в internal/processor.
+2

Реализовать обработку узла LogicalNode для поддержки операторов AND и OR.
+1

Реализовать обработку узла NotNode для поддержки инверсии запросов (negation).
+1

3. Маппинг узлов на шаблоны OpenSearch
Заменить проверку strings.HasPrefix(queryLower, "author:") на извлечение FilterNode с полем field: "author".
+1

Привязать FilterNode  к существующим шаблонам данных:


field: "author" -> fl_author_exact / fl_author_fuzzy.


field: "title" -> fl_title_substring / fl_title_prefix.


field: "any" -> fl_mixed_search.
+1

Интегрировать поддержку Operator:
+1


OP_REGEX -> трансляция в регулярные выражения OpenSearch.


OP_EQUALS -> точное совпадение.
+1

4. Координация логических условий
Реализовать трансляцию LogicalNode в структуру bool query (must, should, must_not) для OpenSearch.
+3

Обеспечить соблюдение приоритетов операторов: NOT > AND > OR.

5. Тестирование и верификация
Добавить интеграционные тесты в tests/smoke_full.sh для проверки цепочки: DSL-строка -> Message-Converter (AST) -> Processor (Walker) -> Data-Manager.
+2

Верифицировать поле meta.canonical_form в ответе для подтверждения корректности разобранного дерева.
+1

Аудит готовности:


Переменные: Поля LogicalOp, Operator и SearchQuery уже объявлены в api/proto/v1/library.proto.
+1


Функции: Парсер parser.Parse(req.Data) уже интегрирован в cmd/message-converter/main.go.


Инфраструктура: Шаблоны OpenSearch (fl_mixed_search, fl_author_exact и др.) готовы к приему структурированных параметров.
