# Интеграция EBusta DSL Engine V19

## 1. Актуализированный процесс обработки (Pipeline)

**Семантический разбор**: Orchestrator передает сырую строку в **dsl-converter** (EBusta DSL Engine V19). Сервис запущен на порту `50052` с пулом из 8 воркеров и отключенным Verbose-логом для максимальной производительности. Он превращает строку в дерево AST (например, выделяет поля `author:` или операторы `AND`), возвращая структурированный Protobuf-ответ.

---

## 2. Work Breakdown Structure (WBS)

### 1. Подготовка артефакта (Engine V19)
* **1.1. Компиляция**: Сборка статического бинарника `dsl-converter` (49MB) через `make build-bin`.
* **1.2. Конфигурация SRE**: Настройка параметров запуска (Workers: 8, Port: 50052, Verbose: NIL).
* **1.3. CLI Validation**: Проверка корректности обработки флага `--verbose` в изолированной среде.

### 2. Интеграция с Orchestrator
* **2.1. Удаление Legacy**: Выпиливание зависимости от внутренней библиотеки `internal/parser` из кода Orchestrator.
* **2.2. gRPC Client**: Реализация/обновление gRPC клиента в Orchestrator для взаимодействия с `MessageConverter/Convert`.
* **2.3. Error Handling**: Обработка сетевых таймаутов и недоступности сервиса `dsl-converter`.

### 3. Нагрузочное тестирование и QA
* **3.1. Базовый профиль**: Запуск `ghz` с нагрузкой 200 RPS (Target Average < 0.8ms).
* **3.2. Stress Test**: Определение точки отказа при 1000+ RPS на 8 воркерах.
* **3.3. Regression**: Проверка соответствия (Compliance) через `test_compliance.sh`.

### 4. Наблюдаемость (Observability)
* **4.1. Logging Strategy**: Проверка отсутствия логов в Silent-режиме и их наличия в Verbose.
* **4.2. Трассировка**: Проверка передачи `request_id` от Orchestrator через DSL Engine в финальный AST.

---

## 3. Матрица взаимодействия (Updated)

| Компонент | Роль | Взаимодействие | Стек |
| :--- | :--- | :--- | :--- |
| **Orchestrator** | Клиент | Передает `raw_query` | Go / Java / CLI |
| **dsl-converter** | Сервис | Выполняет Shunting-yard (V19) | Common Lisp (SBCL) |
| **gRPC/Proto** | Транспорт | Бинарная сериализация | Port 50052 |

