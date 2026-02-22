# Edge Security Refactoring: Gateway + External Adapters

## Контекст

Сейчас защита внешнего периметра реализована неравномерно:
- `gateway` содержит набор middleware/валидаций/ограничений.
- `irc-adapter` имеет минимальные локальные проверки и не использует общий защитный слой.

Из-за этого безопасность дублируется и расходится между адаптерами. Для любых новых внешних адаптеров (например, Telegram) высок риск повторной реализации похожей логики с расхождениями.

## Текущее состояние

### Что уже есть в gateway

В `gateway` реализованы:

1. HTTP middleware-цепочка
- `Recover`
- `SecurityHeaders`
- `CORS`
- `RateLimiter`
- `ContentTypeValidation`
- `SizeLimiter`

2. Валидация/санитизация запроса поиска
- JSON schema validation для `/search`
- ограничение размера body и глубины JSON
- sanitize + SQL-safety check
- унифицированные JSON-ошибки (`errutil`)

3. Токенизация и ограничение скачивания
- download token mapping
- TTL токена
- optional revoke для `single_use=1`

4. Базовая защита от перегрузки
- rate limit по IP
- отдельный лимит для `/download/`

5. Трассировка
- генерация `trace_id`
- проброс trace в downstream вызовы

### Что в IRC сейчас

`irc-adapter`:
- принимает TCP-соединения и читает строковые команды;
- передает команды в handler;
- делает HTTP запросы в gateway для поиска;
- не имеет полноценного собственного слоя connection controls и command throttling;
- локальная `checkRateLimit()` есть, но в текущем pipeline не задействована.

Итого: gateway защищает HTTP backend, но не закрывает сам вход IRC TCP на уровне адаптера.

## Цель рефакторинга

Выделить общую сущность защиты внешнего периметра, применимую для:
- `gateway`
- `irc-adapter`
- будущих внешних адаптеров (например, Telegram)

Рабочее имя: `internal/edge`.

## Предлагаемая архитектура

### 1) Общий transport-agnostic core (`internal/edge`)

Вынести в общий слой:
- rate limit/throttling ядро по ключам (`ip`, `user`, `chat`, `command`, `token`);
- policy-конфигурации per action (`search`, `download`, `resolve`, `command`);
- общие лимиты payload/message (length/size/depth);
- общий набор validators/sanitizers;
- единый формат security-ошибок (коды/детали/trace hooks).

### 2) Transport adapters

Оставить transport-specific обвязки:

HTTP (gateway):
- CORS
- security headers
- content-type
- CSRF (если реально включается)

IRC/TCP:
- connection caps
- per-connection read limits
- line framing guards
- command flood control

Telegram (будущий):
- per-user/per-chat quotas
- dedup/idempotency окна

### 3) Интеграция с текущими компонентами

Gateway:
- middleware остается HTTP-ориентированным,
- но throttling/limit/validation backend берется из `internal/edge`.

IRC:
- перед handler добавляется pipeline guard:
  - connection control
  - input bounds
  - command throttle
  - basic validation policy
- локальный ad-hoc rate limiter заменяется на общий.

Telegram:
- сразу использует `internal/edge` без копипаста логики.

## Варианты внедрения

### Вариант 1 (минимальный)

Вынести только limiter+policy keys.
- Плюсы: быстрый эффект.
- Минусы: валидация и guards остаются фрагментированными.

### Вариант 2 (рекомендуемый)

Вынести limiter + validators + guard interfaces.
- Плюсы: реальный единый внешний security layer.
- Минусы: больше изменений, но контролируемо.

### Вариант 3 (максимальный)

Policy engine/DSL.
- Плюсы: гибкость.
- Минусы: избыточная сложность для текущей стадии.

## Риски

1. Регрессия в gateway runtime-поведении.
- Митигировать: сохранить публичные HTTP middleware контракты.

2. Жесткое блокирование пользователей в IRC при ошибочных лимитах.
- Митигировать: запуск в shadow/log mode до enforce.

3. Переобобщение и чрезмерная абстракция.
- Митигировать: четкое разделение core и transport layers.

## Тестовая стратегия

1. Unit
- limiter semantics (window/burst/concurrency)
- validators/sanitizers
- policy evaluation

2. Component
- gateway middleware с edge-core
- irc command pipeline с throttling и limits

3. Abuse/negative
- flood
- oversized payload
- malformed input

4. Regression
- обычные сценарии поиска/скачивания не деградируют

## Вывод

Рефакторинг в общий `edge`-слой целесообразен:
- уменьшит дублирование;
- выровняет безопасность всех внешних адаптеров;
- упростит подключение новых каналов (включая Telegram) без копирования защитной логики.

Рекомендация: идти через вариант 2 (сбалансированный), поэтапно с сохранением обратной совместимости на уровне текущих gateway/adapter entrypoints.
