# CLAUDE_TASK_PROMPT.md

## Шаблон задачи для Claude (Scala/Storage)

### Контекст
Проект: ebusta (monorepo). Ты работаешь прямо в репозитории.  
Твоя основная зона ответственности:
- Scala-сервисы: `dsl-scala/`, `query-builder/`
- Сервисы хранения: `cmd/archive-node`, `cmd/tier-node`, `cmd/plasma-node`, `cmd/downloader`
- Общие библиотеки: `internal/downloads/`, `internal/errutil`, `internal/logger`

### Задача
<ОПИСАНИЕ_ЗАДАЧИ_ОДНОЙ_ФРАЗОЙ>

### Scope (что нужно сделать)
- <пункт 1>
- <пункт 2>

### Out of scope (что НЕ делать)
- Не менять proto-контракты без явного разрешения.
- Не добавлять новые сервисы/технологии.
- Не рефакторить код, не относящийся к задаче.

### Источники истины (прочитать)
- `docs/ARCHITECTURAL_CONSTITUTION.md`
- `docs/API_ERROR_MAPPING.md`
- `docs/TRACE.md`
- Код в указанных директориях.

### Definition of Done
- <проверяемые критерии, например: тесты проходят, e2e не сломаны>
- `make test-scala` (или `sbt test`) успешен.
- `make e2e` (если затрагивает интеграцию).

### Что вернуть в ответе
1) Краткий отчёт (bullet list).
2) Список изменённых файлов.
3) Команды для сборки/проверки (локально).
4) Текст коммита (или commit hash).
