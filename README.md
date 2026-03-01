# Ebusta 📚

Микросервисная поисковая система для архивов Flibusta. Позволяет выполнять быстрый поиск по миллионам записей через OpenSearch, используя собственный DSL (Domain Specific Language).

## 🏗 Архитектура системы

Система состоит из нескольких независимых сервисов, взаимодействующих по gRPC:

* **Web-Adapter (The Door)**: Принимает внешние HTTP-запросы и передает их в оркестратор.
* **Orchestrator**: Координирует работу всех сервисов, управляет Trace-ID.
* **Message-Converter**: Парсит строку запроса в AST-дерево.
* **Processor**: Обрабатывает бизнес-логику и выбирает стратегию поиска.
* **Datamanager**: Слой данных, работающий с OpenSearch.
* **Auth-Manager**: Проверяет права доступа и управляет whitelist.
* **Ebusta-CLI**: Интерактивная оболочка для работы с системой.



## 🚦 Карта портов

| Сервис            | Порт (gRPC) | Функции                          |
|:------------------|:------------|:---------------------------------|
| Datamanager       | `:50051`    | Слой данных (OpenSearch)         |
| Message-Converter | `:50052`    | Парсер (AST)                     |
| Processor         | `:50053`    | Логика и выбор шаблонов          |
| Orchestrator      | `:50054`    | Координация                      |
| Auth-Manager      | `:50055`    | Безопасность (Whitelist)         |
| Web-Adapter       | `:8080`     | HTTP-вход (REST)                 |
| Metrics           | `:9091`     | Prometheus метрики (Datamanager) |

## 🚀 Быстрый старт

### Сборка и запуск
Требуется установленный Go 1.21+ и Protoc.

```bash
make build   # Генерация Proto и компиляция всех сервисов
make run     # Запуск всей системы в фоновом режиме
```

## Ops: Prometheus metrics (orchestrator)
- Metrics endpoint: `http://localhost:50090/metrics`
- Manual helper:
  - `./scripts/ebusta_metrics.sh head`
  - `./scripts/ebusta_metrics.sh check`
  - `./scripts/ebusta_metrics.sh inc 5`

## Testing Classification

Test taxonomy and migration baseline are documented here:

- `test/classification/README.md`
- `test/classification/inventory.md`
- `test/classification/ci-matrix.md`

Quick test commands:

- `make test-unit`
- `make test-integration`
- `make test-scala`
- `make test-e2e`
- `make test-load`

## Documentation

Core architecture and operations docs:

- `docs/ARCHITECTURAL_CONSTITUTION.md`
- `docs/API_ERROR_MAPPING.md`
- `docs/TRACE.md`
- `docs/ENGINEERING_STANDARD.md`
- `docs/PROTO_IMMUTABILITY.md`
