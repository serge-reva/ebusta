# GRPC API Reference: Доступные сервисы и методы

В данном документе перечислены все gRPC методы, зарегистрированные в системе EBusta, их пути для вызова и структуры сообщений.

---

## 1. Сервис: MessageConverter (Основной)
**Описание:** Трансляция поисковых запросов из Lisp DSL в Protobuf-структуры.
**Прото-файл:** `lisp-converter/search.proto`
**Пакет:** `ebusta.library.v1`

### Метод: `Convert`
* **Полный путь:** `ebusta.library.v1.MessageConverter/Convert`
* **Request:** `ConvertRequest` (поле `raw_query` типа string)
* **Response:** `SearchQuery` (рекурсивное дерево)
* **Пример вызова:**
  ```bash
  grpcurl -plaintext -d '{"raw_query": "(:field \"title\" \"Lisp\")"}' localhost:50052 ebusta.library.v1.MessageConverter/Convert
2. Сервис: Greeter (Тестовый)
Описание: Используется для Smoke-тестов инфраструктуры. Прото-файл: lisp-converter/helloworld.proto Пакет: cl_protobufs.lisp.grpc.integration_testing

Метод: SayHello
Полный путь: cl_protobufs.lisp.grpc.integration_testing.Greeter/SayHello

Request: HelloRequest (поле name)

Response: HelloReply (поле message)

Пример вызова:

Bash

grpcurl -plaintext -d '{"name": "Admin"}' localhost:50051 cl_protobufs.lisp.grpc.integration_testing.Greeter/SayHello
3. Обнаруженные в системе типы (Internal Types)
Ниже перечислены основные структуры, используемые в SearchQuery:

LogicalNode
Используется для группировки условий.

op: 1 (AND), 2 (OR)

nodes: Список (repeated) объектов SearchQuery.

FilterNode
Конечный фильтр для базы данных.

field: Имя поля (title, author, series, etc.)

value: Значение для поиска.

operator: Тип сравнения (по умолчанию 1 = EQ).

4. Интроспекция (Как найти новые методы)
Если в будущем будут добавлены новые .proto файлы, список методов можно получить через grpcurl, если сервер поддерживает Reflection (или передав путь к файлу):

Bash

# Список сервисов
grpcurl -plaintext -import-path ./lisp-converter -proto ./lisp-converter/search.proto list

# Детальное описание метода
grpcurl -plaintext -import-path ./lisp-converter -proto ./lisp-converter/search.proto describe ebusta.library.v1.MessageConverter/Convert
