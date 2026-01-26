# RUNBOOK: Инфраструктура gRPC и DSL Конвертера (Common Lisp)

Этот документ является основным техническим руководством по работе с Lisp-сервисами в проекте **EBusta**.

---

## 1. Архитектура и компоненты

Система построена на базе `cl-protobufs` и обертки над C++ gRPC core.

### Файловая структура
* `grpc/`: Подмодуль с Lisp-биндингами gRPC. Содержит `grpc.so` и макросы для определения серверов.
* `lisp-converter/search.proto`: Контракт Protobuf. Определяет сообщения `SearchQuery`, `LogicalNode` (AND/OR) и `FilterNode`.
* `lisp-converter/dsl-service.lisp`: Серверная логика. Содержит рекурсивный парсер S-выражений.
* `lisp-converter/dsl-client.lisp`: Тестовый клиент для верификации сложных запросов.

---

## 2. Спецификация DSL (S-Expressions)

Конвертер преобразует Lisp-синтаксис в строго типизированные Protobuf-объекты:

| DSL Пример | Тип Node | Protobuf структура |
| :--- | :--- | :--- |
| `(:field "title" "Lisp")` | **Filter** | `field: "title", value: "Lisp", op: 1` |
| `(:and ... ...)` | **Logical** | `op: 1 (AND), nodes: [...]` |
| `(:or ... ...)` | **Logical** | `op: 2 (OR), nodes: [...]` |

**Пример сложного запроса:**
`(:and (:field "title" "Lisp") (:or (:field "author" "Serge") (:field "author" "Reva")))`

---

## 3. Инструкции по запуску

### А. Уровень: Example (SayHello)
Проверка базовой связности (порт `50051`).

1.  **Сервер:** `bash ~/projects/ebusta/lisp-converter/run_example_server.sh`
2.  **Клиент (Lisp):** `bash ~/projects/ebusta/lisp-converter/run_example_client.sh`
3.  **Клиент (grpcurl):**
    ```bash
    ~/projects/ebusta/lisp-converter/grpcurl -plaintext \
        -import-path ~/projects/ebusta/lisp-converter/ \
        -proto ~/projects/ebusta/lisp-converter/helloworld.proto \
        -d '{"name": "Admin"}' localhost:50051 \
        cl_protobufs.lisp.grpc.integration_testing.Greeter/SayHello
    ```

### Б. Уровень: Production (DSL Converter)
Основной сервис трансляции (порт `50052`).

1.  **Сервер:** `bash ~/projects/ebusta/lisp-converter/run_dsl_server.sh`
2.  **Клиент (Lisp):** `bash ~/projects/ebusta/lisp-converter/run_dsl_client.sh`
3.  **Клиент (grpcurl):**
    ```bash
    ~/projects/ebusta/lisp-converter/grpcurl -plaintext \
        -import-path ~/projects/ebusta/lisp-converter/ \
        -proto ~/projects/ebusta/lisp-converter/search.proto \
        -d '{"raw_query": "(:and (:field \"title\" \"Lisp\") (:field \"author\" \"Serge\"))"}' \
        localhost:50052 ebusta.library.v1.MessageConverter/Convert
    ```

---

## 4. SRE: Диагностика и устранение неисправностей

### Порты и процессы
Если сервер не запускается (Address already in use):
* `fuser -k 50051/tcp` (для Example)
* `fuser -k 50052/tcp` (для DSL)

### Пути и зависимости (ASDF)
Серверы полагаются на `asdf:*central-registry*`. Если возникает ошибка `Failed to find TRUENAME`, проверьте, что в скриптах запуска указан верный путь к подмодулю `grpc`:
`~/projects/ebusta/grpc/`

### Пакеты и символы
Если при вызове метода возникает `TYPE-ERROR ... is not of type GRPC::METHOD-DETAILS`:
1.  Проверьте полное имя сервиса в логах сервера.
2.  Убедитесь, что `grpcurl` использует верный путь: `ebusta.library.v1.MessageConverter/Convert`.

### Использование grpcurl
Всегда указывайте `-import-path`, иначе `grpcurl` не сможет найти зависимости в `.proto` файлах при использовании абсолютных путей.

---

## 5. Обновление протоколов
При изменении `.proto` файлов необходимо:
1.  Перезапустить сервер (ASDF автоматически пересоберет `.lisp` файлы из `.proto`).
2.  Если добавились новые поля, обновить функцию `parse-dsl` в `dsl-service.lisp`.
