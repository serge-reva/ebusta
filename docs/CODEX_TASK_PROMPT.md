# CODEX_TASK_PROMPT.md

## Шаблон задачи для Codex (Go/Infra)

### Контекст
Проект: ebusta (monorepo). Ты работаешь прямо в репозитории.  
Твоя основная зона ответственности:
- Go-сервисы: `cmd/gateway`, `cmd/orchestrator`, `cmd/datamanager`, `cmd/web-adapter`, `cmd/irc-adapter`, `cmd/json-gateway`, `cmd/web-frontend`
- Инфраструктура: `Makefile`, `docker-compose.yml`, `ebusta.yaml`
- Общие библиотеки: `internal/gateway`, `internal/edge`, `internal/presenter`, `internal/search`

### Задача
<ОПИСАНИЕ_ЗАДАЧИ_ОДНОЙ_ФРАЗОЙ>

### Scope (что нужно сделать)
- <пункт 1>
- <пункт 2>

### Out of scope
- Не изменять proto-контракты.
- Не добавлять новые базы данных или внешние сервисы.
- Не рефакторить код, не относящийся к задаче.

### Источники истины
- `docs/ARCHITECTURAL_CONSTITUTION.md`
- `docs/API_ERROR_MAPPING.md`
- `docs/TRACE.md`
- Код в указанных директориях.

### Definition of Done
- <критерии>
- `make test-go` (go test ./...) успешен.
- `make e2e` (если затрагивает интеграцию).
- `make docker-up` и ручная проверка (если требуется).

### Что вернуть в ответе
1) Краткий отчёт.
2) Список изменённых файлов.
3) Команды для проверки.
4) Текст коммита.
