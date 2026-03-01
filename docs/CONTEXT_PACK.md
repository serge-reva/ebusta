# CONTEXT_PACK.md

## Полный контекст проекта Ebusta (для нового чата)

### 0) Идентификация проекта
- **Репозиторий**: ebusta (monorepo)
- **Цель**: предоставить удобный поиск и скачивание книг через различные интерфейсы (CLI, IRC, Telegram, Web).
- **Текущая версия**: MVP (v0.1) – базовая функциональность поиска и скачивания.

### 1) Архитектура (компоненты и границы)

#### 1.1 Внешние адаптеры
- `cmd/cli` – интерфейс командной строки (Go).
- `cmd/irc-adapter` – IRC-бот/сервер.
- `cmd/telegram-adapter` – HTTP-сервер для Telegram-бота.
- `cmd/web-frontend` – простой веб-интерфейс (HTML+CSS, без фреймворков).
- `cmd/web-adapter` – упрощённый HTTP-адаптер (обратная совместимость).

#### 1.2 Внутренние сервисы
- `cmd/gateway` – единая точка входа для внешних клиентов (кроме CLI). Выполняет валидацию, rate limiting, маршрутизацию к `orchestrator` и `downloader`.
- `cmd/orchestrator` – оркестратор поиска: вызывает `dsl-scala`, `query-builder`, `datamanager`.
- `cmd/datamanager` – выполняет запросы к OpenSearch.
- `cmd/downloader` – HTTP-сервис для скачивания файлов, проксирует к `plasma-node`.
- `cmd/archive-node`, `cmd/tier-node`, `cmd/plasma-node` – узлы хранения с иерархическим кэшированием.

#### 1.3 Scala-компоненты
- `dsl-scala/` – парсер DSL, возвращает AST.
- `query-builder/` – строит JSON-запрос для OpenSearch на основе AST.

#### 1.4 Общие библиотеки (internal/)
- `internal/errutil` – унифицированные ошибки с кодом и TraceID.
- `internal/logger` – структурированное логирование.
- `internal/edge` – валидация ввода и rate limiting.
- `internal/presenter` – форматирование результатов.
- `internal/search` – клиент для orchestrator.

### 2) Технологический стек
- **Языки**: Go (основные сервисы), Scala (парсер и query builder).
- **Протоколы**: gRPC (межсервисный), REST (для внешних адаптеров).
- **Хранилища**: SQLite (метаданные archive/tier), файловая система (zip-архивы), OpenSearch (индекс).
- **Инфраструктура**: Docker (runtime-образы), Makefile (сборка/тесты).

### 3) Ключевые контракты
- Proto-файлы в `api/proto/v1/` – неизменны для версии v1.
- Конфигурация – `ebusta.yaml` (основной) и переменные окружения.
- TraceID – передаётся через заголовки `X-Trace-Id` (HTTP) и `x-trace-id` (gRPC metadata). При отсутствии генерируется сервисом.

### 4) Инварианты (см. ARCHITECTURAL_CONSTITUTION.md)
- Gateway не содержит логики хранения.
- Все ошибки маппятся согласно API_ERROR_MAPPING.md.
- Тесты (`make e2e`) должны оставаться зелёными.

### 5) Глоссарий (основные термины)
- **DSL** – предметно-ориентированный язык поисковых запросов (например, `author:king`).
- **AST** – абстрактное синтаксическое дерево, внутреннее представление запроса.
- **TraceID** – сквозной идентификатор запроса для трассировки.
- **Layout** – в текущей версии не используется (относится к UI-редактору графов, которого пока нет).

### 6) AI Quick Start (для нового исполнителя)
1. Прочитай этот документ полностью.
2. Ознакомься с основными конституционными документами: `ARCHITECTURAL_CONSTITUTION.md`, `API_ERROR_MAPPING.md`, `TRACE.md`.
3. При получении задачи используй соответствующий шаблон (`CODEX_TASK_PROMPT.md` или `CLAUDE_TASK_PROMPT.md`).
4. Всегда проверяй изменения локально: `make test && make e2e`.
5. В ответе обязательно указывай список изменённых файлов и хэш коммита.

### Ссылки
- [ARCHITECTURAL_CONSTITUTION.md](ARCHITECTURAL_CONSTITUTION.md)
- [API_ERROR_MAPPING.md](API_ERROR_MAPPING.md)
- [TRACE.md](TRACE.md)
- [GLOSSARY.md](GLOSSARY.md)
- [RUNBOOK.md](RUNBOOK.md)
