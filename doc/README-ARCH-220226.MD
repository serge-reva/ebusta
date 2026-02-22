# Архитектурный обзор ebusta (2026-02-22)

## 1) Архитектура: сервисы/компоненты, роли, интерфейсы, контракты

### Gateway
- Путь: `cmd/gateway`, `internal/gateway/*`
- Роль: внешний HTTP API (`/search`, `/download/*`, `/health`), rate-limit, валидация, токенизация ссылок скачивания.
- Интерфейсы:
  - HTTP вход
  - gRPC клиент к orchestrator
  - HTTP клиент к downloader
- Контракты:
  - JSON-схема поиска: `internal/gateway/validation/schema.go`
  - gRPC для поиска: `api/proto/v1/library.proto`
  - gRPC/DTO для storage-chain: `api/proto/v1/downloads.proto`

### Orchestrator
- Путь: `cmd/orchestrator/main.go`
- Роль: координация search pipeline: DSL -> QueryBuilder -> DataManager.
- Интерфейсы:
  - gRPC сервер `OrchestratorService.Search`
  - gRPC клиенты `DslTransformer`, `QueryBuilder`, `StorageService`
- Контракты:
  - `api/proto/v1/library.proto`
  - `api/proto/v1/dsl.proto`
  - `api/proto/v1/query_builder.proto`

### Datamanager
- Путь: `cmd/datamanager/main.go`
- Роль: адаптер к OpenSearch; исполняет JSON-запрос и мапит hits в `Book`.
- Интерфейсы:
  - gRPC сервер `StorageService.SearchBooks`
  - HTTP клиент к OpenSearch (`/_search` или `/_search/template`)
- Контракты:
  - `api/proto/v1/library.proto`

### DSL-Scala service
- Путь: внешний сервис/JAR (`dsl-scala`)
- Роль: преобразование текстового запроса в AST.
- Интерфейсы: gRPC `DslTransformer.Transform`.
- Контракт: `api/proto/v1/dsl.proto` (+ AST из `api/proto/v1/common.proto`).

### Query Builder service
- Путь: внешний сервис/JAR (`query-builder`)
- Роль: генерация OpenSearch JSON из AST, выбор типа исполнения (DSL/TEMPLATE).
- Интерфейсы: gRPC `QueryBuilder.Build`.
- Контракт: `api/proto/v1/query_builder.proto`.

### Web Frontend
- Путь: `cmd/web-frontend/*`
- Роль: HTML UI поиска и скачивания.
- Интерфейсы:
  - HTTP (`/`, `/search`, `/download/{sha1}`)
  - gRPC клиент к orchestrator через `internal/search`
  - HTTP клиент к downloader
- Контракт: косвенно `library.proto` через `internal/search`.

### Web Adapter
- Путь: `cmd/web-adapter/main.go`
- Роль: упрощенный HTTP-to-orchestrator адаптер (`/search?q=&limit=&offset=`).
- Интерфейсы: HTTP вход + gRPC к orchestrator через `internal/search`.
- Контракт: `library.proto`.

### CLI
- Путь: `cmd/cli/main.go`
- Роль: консольный поиск и скачивание.
- Интерфейсы:
  - gRPC к orchestrator (поиск)
  - gRPC к `StorageNode` в plasma (скачивание)
- Контракты: `library.proto`, `downloads.proto`.

### Downloader
- Путь: `cmd/downloader/main.go`
- Роль: HTTP-шлюз скачивания книги по SHA1.
- Интерфейсы:
  - HTTP `/books/{sha1}`
  - gRPC клиент к `StorageNode` (plasma)
- Контракт: `api/proto/v1/downloads.proto`.

### Plasma Node
- Путь: `cmd/plasma-node`, `internal/downloads/plasma/node.go`
- Роль: in-memory hot cache книг и метаданных с ограничениями `max_bytes/max_items`, eviction по частоте/возрасту.
- Интерфейсы: gRPC `StorageNode`.
- Контракт: `downloads.proto`.

### Tier Node
- Путь: `cmd/tier-node`, `internal/downloads/tier/node.go`
- Роль: warm слой; локальные файлы + sqlite метаданные; при miss подтягивает из parent.
- Интерфейсы: gRPC `StorageNode`.
- Контракт: `downloads.proto`.

### Archive Node
- Путь: `cmd/archive-node`, `internal/downloads/archive/node.go`
- Роль: cold/source-of-truth для download-контура; читает ZIP-контейнеры и sqlite индексы метаданных.
- Интерфейсы: gRPC `StorageNode`.
- Контракт: `downloads.proto`.

### Auth Manager
- Путь: `cmd/auth-manager/main.go`
- Роль: whitelist-доступ по `platform/user`.
- Интерфейсы: gRPC `AuthService.CheckAccess`.
- Контракт: `api/proto/v1/auth.proto`.
- Примечание: в текущем gateway/orchestrator потоке в коде не задействован.

## 2) Потоки данных (end-to-end)

### 2.1 Поиск через Gateway
- Вход: HTTP JSON `{"query","page","limit"}`.
- Сервисы: `gateway` -> `orchestrator` -> `dsl-scala` -> `query-builder` -> `datamanager` -> OpenSearch.
- Хранилища/индексы/кэши: OpenSearch индекс (в `ebusta.yaml`: `flibusta_merged_index`).
- Выход: HTTP JSON (`trace_id,total,books,page,pages`), у книг `download_url` с токеном.

### 2.2 Поиск через Web Frontend
- Вход: HTTP query params `q`, `page`.
- Сервисы: `web-frontend` -> `internal/search` -> `orchestrator` -> `dsl/qb/datamanager` -> OpenSearch.
- Хранилища/индексы/кэши: OpenSearch.
- Выход: HTML с результатами и пагинацией.

### 2.3 Поиск через Web Adapter
- Вход: HTTP query params `q,limit,offset`.
- Сервисы: `web-adapter` -> `internal/search` -> `orchestrator` -> `dsl/qb/datamanager` -> OpenSearch.
- Хранилища/индексы/кэши: OpenSearch.
- Выход: JSON DTO `SearchResult`.

### 2.4 Поиск через CLI
- Вход: CLI query/flags.
- Сервисы: `cli` -> `internal/search` -> `orchestrator` -> `dsl/qb/datamanager` -> OpenSearch.
- Хранилища/индексы/кэши: OpenSearch.
- Выход: текстовый табличный вывод в stdout.

### 2.5 Скачивание через Gateway token URL
- Вход: HTTP `/download/{token}` (токен от mapper).
- Сервисы: `gateway` (resolve token -> sha1) -> `downloader` HTTP -> `plasma` gRPC -> при miss `tier` -> при miss `archive`.
- Хранилища/индексы/кэши:
  - in-memory token map в `gateway`
  - in-memory cache в `plasma`
  - файловый кэш + sqlite в `tier`
  - ZIP + sqlite в `archive`
- Выход: бинарный stream файла или ошибка.

### 2.6 Скачивание через Web Frontend
- Вход: HTTP `/download/{sha1}`.
- Сервисы: `web-frontend` -> `downloader` -> `plasma` -> `tier` -> `archive` (каскадно по miss).
- Хранилища/индексы/кэши: plasma/tier/archive.
- Выход: бинарный stream файла.

### 2.7 Fill-path при miss в download-контуре
- Вход: `GetMeta/GetStream(sha1)` в `plasma` или `tier`.
- Сервисы:
  - `plasma.ensureLocal` вызывает parent `tier`
  - `tier.ensureLocal` вызывает parent `archive`
  - `archive` читает ZIP entry
- Хранилища:
  - `archive.meta.sqlite`, ZIP root
  - `tier.meta.sqlite`, `tier.data`
  - RAM map в `plasma`
- Выход: локальное заполнение слоя и возврат данных вверх.

## 3) Конфигурация и обнаружение сервисов

- Единый root-конфиг: `ebusta.yaml`, загрузка через `internal/config/config.go` (`config.Get()`).
- Путь к конфигу: env `EBUSTA_CONFIG`, иначе `ebusta.yaml`.
- Адреса gRPC сервисов: `ComponentConfig.Address()` (`host:port`).
- Orchestrator берет адреса downstream (`dsl_scala`, `query_builder`, `datamanager`) из `ebusta.yaml`.
- Gateway берет `services.orchestrator/downloader/auth` из `ebusta.yaml` через `internal/gateway/config/config.go`.
- Downloads-цепочка задается в `downloads.*` секциях `ebusta.yaml` (`archive_node`, `tier_node.parent`, `plasma_node.parent`, `downloader.plasma`).
- `Makefile up` запускает сервисы и для части из них задает `EBUSTA_CONFIG=./ebusta.yaml`.
- `auth-manager` слушает жестко `:50055`, whitelist грузит из `cmd/auth-manager/whitelist.yaml`.

## 4) Сильные стороны (по факту)

1. Четкое разделение ответственности между API, orchestration, query-building, data-access и download-tiering.
2. Контракты API централизованы в `api/proto/v1/*.proto` (поиск отдельно от storage-chain).
3. Сквозной `trace_id` поддерживается в нескольких слоях (`gateway`, `web-frontend`, `search`, `orchestrator`, `downloader`, `tier/archive/plasma`).
4. В download-контуре реализована иерархия hot/warm/cold (`plasma` -> `tier` -> `archive`) с lazy fill.
5. В `tier/plasma` есть singleflight-подобная защита (`sf map`) от дублирующих fetch для одного sha1.
6. `tier` использует атомарный путь записи (`.tmp` + `rename`) и проверку размера against `meta.size`.
7. Gateway содержит базовые защиты: body-size limit, schema validation, rate limit, security/CORS middleware.
8. Токенизация download URL в gateway скрывает прямой SHA1 во внешнем API.
9. Download API поддерживает `HEAD` и `meta=1`.
10. Plasma публикует метрики через expvar (`hits/misses/evictions/items/bytes`).

## 5) Что улучшить (без изменений прямо сейчас)

1. Проблема/риск: дублирование gateway config-моделей.
   - Возможное улучшение: оставить один источник правды и убрать дубли конфиг-типов.
   - Затронутые файлы/модули: `internal/gateway/config.go`, `internal/gateway/config/config.go`, `cmd/gateway/main.go`.

2. Проблема/риск: `auth-manager` использует жесткий адрес и путь whitelist вне root-конфига.
   - Возможное улучшение: перевести адрес и путь whitelist в `ebusta.yaml`/`config.Get()`.
   - Затронутые файлы/модули: `cmd/auth-manager/main.go`, `internal/config/config.go`, `ebusta.yaml`.

3. Проблема/риск: в orchestrator соединения к downstream создаются без явной обработки ошибок.
   - Возможное улучшение: fail-fast и явные проверки подключения на старте с логированием.
   - Затронутые файлы/модули: `cmd/orchestrator/main.go`.

4. Проблема/риск: риск несогласованной пагинации (`page/offset`) по цепочке orchestration/query-builder/opensearch.
   - Возможное улучшение: выровнять проброс `from/offset` по всей pipeline.
   - Затронутые файлы/модули: `cmd/orchestrator/main.go`, `api/proto/v1/query_builder.proto`, реализация query-builder, `cmd/datamanager/main.go`.

5. Проблема/риск: политика `trace_id` в gateway может расходиться с downstream trace.
   - Возможное улучшение: закрепить единую стратегию trace propagation/echo.
   - Затронутые файлы/модули: `internal/gateway/handlers.go`, `internal/gateway/clients/orchestrator.go`.

6. Проблема/риск: в `irc-adapter` есть сомнительный debug output в `fmt.Fprintf(client.conn.RemoteAddr().String(), ...)`.
   - Возможное улучшение: убрать/заменить на structured logging.
   - Затронутые файлы/модули: `cmd/irc-adapter/handler.go`.

7. Проблема/риск: валидация query распределена между schema regex и отдельными эвристиками SQL injection.
   - Возможное улучшение: унифицировать правила в одном месте и синхронизировать с допустимой DSL-грамматикой.
   - Затронутые файлы/модули: `internal/gateway/validation/schema.go`, `internal/gateway/validation/sanitize.go`, `internal/search/query.go`.

8. Проблема/риск: middleware `csrf` создается, но не подключен в цепочку `setupRoutes`.
   - Возможное улучшение: либо подключить middleware, либо удалить неиспользуемую сущность.
   - Затронутые файлы/модули: `internal/gateway/server.go`, `internal/gateway/middleware/*`.

9. Проблема/риск: download-chain не показывает явную retry/backoff стратегию для transient upstream сбоев.
   - Возможное улучшение: добавить ограниченные retry/backoff + метрики ошибок upstream.
   - Затронутые файлы/модули: `cmd/downloader/main.go`, `internal/downloads/plasma/node.go`, `internal/downloads/tier/node.go`.

10. Проблема/риск: README расходится с фактическими портами/ролями из конфига и Makefile.
    - Возможное улучшение: синхронизировать документацию с `ebusta.yaml` и текущими service targets.
    - Затронутые файлы/модули: `README.md`, `ebusta.yaml`, `Makefile`.

## 6) Прочитанные файлы (paths)

- `AGENTS.md`
- `Makefile`
- `ebusta.yaml`
- `README.md`
- `go.mod`
- `internal/config/config.go`
- `internal/search/service.go`
- `internal/search/search.go`
- `internal/search/query.go`
- `internal/presenter/search_result.go`
- `internal/presenter/pager.go`
- `internal/presenter/text_formatter.go`
- `internal/presenter/irc_formatter.go`
- `cmd/orchestrator/main.go`
- `cmd/datamanager/main.go`
- `cmd/gateway/main.go`
- `internal/gateway/server.go`
- `internal/gateway/handlers.go`
- `internal/gateway/clients/orchestrator.go`
- `internal/gateway/clients/downloader.go`
- `internal/gateway/mapper/mapper.go`
- `internal/gateway/config/config.go`
- `internal/gateway/config.go`
- `internal/gateway/validation/schema.go`
- `internal/gateway/validation/sanitize.go`
- `internal/gateway/middleware/ratelimit.go`
- `cmd/web-adapter/main.go`
- `cmd/web-frontend/main.go`
- `cmd/web-frontend/handlers.go`
- `cmd/cli/main.go`
- `cmd/irc-adapter/main.go`
- `cmd/irc-adapter/handler.go`
- `cmd/downloader/main.go`
- `cmd/plasma-node/main.go`
- `cmd/tier-node/main.go`
- `cmd/archive-node/main.go`
- `internal/downloads/plasma/node.go`
- `internal/downloads/tier/node.go`
- `internal/downloads/archive/node.go`
- `internal/downloads/sqlite/schema.go`
- `cmd/auth-manager/main.go`
- `cmd/auth-manager/whitelist.yaml`
- `api/proto/v1/library.proto`
- `api/proto/v1/downloads.proto`
- `api/proto/v1/dsl.proto`
- `api/proto/v1/query_builder.proto`
- `api/proto/v1/auth.proto`
