# README-SEC-IMPL

## 1. Постановка задачи

Цель текущей реализации безопасности в ebusta:
- защитить внешние transport-точки от злоупотреблений и некорректного входного трафика;
- стандартизировать поведение защит между адаптерами (gateway, IRC, telegram);
- обеспечить наблюдаемость решений защиты (allow/deny/throttle) с едиными reason-кодами;
- сохранить разделение ответственности: edge-защиты на периметре, бизнес-логика поиска внутри сервисов.

Ключевой принцип: любой адаптер, который «торчит наружу», должен проходить через единый набор механизмов `limits + validation + throttling + connection controls`.

---

## 2. Модель угроз (что закрываем)

В текущей реализации закрываются следующие практические риски:
- flood/abuse запросами (rate limiting по action/key);
- oversized payload (ограничение размера тела и line length);
- malformed/deep JSON (синтаксис и глубина JSON);
- некорректные token/sha1 форматы;
- попытки инъекций в search query (базовые санитаризация и SQL-safety check);
- утечки внутренних адресов при внешних URL (SSRF guard, где применён);
- паники обработчиков (recover middleware);
- отсутствие трассировки ошибок (trace id через HTTP цепочку).

Что **не является** полностью закрытым в текущем объёме:
- полноценная CSRF-схема с серверной сессией/ротацией токенов (есть только базовая проверка заголовка);
- полноценная runtime hot-reload policy без рестарта (реализован safe restart-путь через SIGHUP/restart mode);
- прямой экспорт метрик в Prometheus endpoint из edge-core (есть label-counter hook, но без отдельного exporter в этом шаге).

---

## 3. Архитектурная реализация защиты

## 3.1. Единый edge-core

Базовый security-core вынесен в `internal/edge`:
- `Policy`:
  - `MaxLineLength`
  - `MaxBodyBytes`
  - `MaxJSONDepth`
  - `Actions[action] => {PerMinute, Burst}`
- `Engine`:
  - `Allow(ctx, action, key)` — throttling/rate control;
  - `ValidateLine(ctx, action, line)` — line-length guard;
  - `ValidateJSON(ctx, action, body)` — body-size + JSON-depth/syntax guard.
- `Decision` и унифицированные коды причины:
  - decision: `allow | deny | throttle`
  - reason: `ok | rate_limit | input_too_long | body_too_large | json_too_deep | malformed_json | invalid_token | invalid_sha1`
- hook-модель (`Hook`, `MultiHook`) для общей телеметрии.

Итог: одинаковая semantics защит для gateway/irc/telegram без дублирования логики.

## 3.2. Policy-конфигурация и источник правды

Security policy централизована в конфиге:
- `internal/config/config.go`:
  - `EdgeConfig`
  - `EdgePolicyConfig`
  - `EdgeActionConfig`
  - метод `EdgePolicyForSource(source)`.
- `internal/edge/config_policy.go`:
  - `PolicyFromConfig(cfg, source)` строит runtime policy из `edge.default` + `edge.sources[source]`.

Реализация поддерживает:
- общие дефолты (для всех external adapters);
- per-source overrides (`gateway`, `irc`, `telegram`);
- reload contract через `edge.reload_mode` (`restart`/SIGHUP-сценарий).

---

## 4. Защиты в gateway

Gateway остаётся основным внешним perimeter для HTTP:

## 4.1. Middleware-слой

- Rate limiting через edge-engine (`internal/gateway/middleware/ratelimit.go`):
  - action `ip` для общего HTTP потока;
  - action `resolve` для `/download/*` сценариев;
  - key = клиентский IP (`X-Forwarded-For`/`X-Real-IP`/RemoteAddr);
  - при превышении: `429`.

- Security headers (`internal/gateway/middleware/security.go`):
  - `X-Content-Type-Options: nosniff`
  - `X-Frame-Options: DENY`
  - `X-XSS-Protection`
  - `Referrer-Policy`
  - `Strict-Transport-Security`
  - `Content-Security-Policy`

- Content-Type validation (`application/json` для POST/PUT).

- CORS middleware (ограничения origin/method/headers из конфига).

- Panic recovery middleware (`internal/gateway/middleware/recover.go`).

## 4.2. Валидация payload и схем

- `internal/gateway/validation/size.go`:
  - `http.MaxBytesReader` (hard body cap);
  - `ValidateJSONPayload` (size/depth/syntax).

- `internal/gateway/validation/schema.go`:
  - JSON schema validation search request;
  - нормализация query;
  - валидация SHA1/token через `internal/edge` regex guards.

## 4.3. Download token security

- `internal/gateway/mapper/mapper.go`:
  - генерация random URL-safe token;
  - TTL;
  - лимит максимума токенов;
  - `Resolve` с проверкой истечения;
  - `Revoke` (single-use сценарии).

- `internal/gateway/handlers.go`:
  - строгая проверка token формата до resolve;
  - безопасная обработка expired/invalid token;
  - выдача только tokenized URL, без прямого раскрытия внутреннего SHA1/path.

## 4.4. SSRF guard

- `internal/gateway/ssrf/guard.go`:
  - блок private CIDR/localhost/metadata hosts;
  - reject небезопасных URL.

---

## 5. Защиты в IRC adapter

`cmd/irc-adapter` интегрирован с тем же edge-core:
- policy берётся из `edge.sources.irc` (`PolicyFromConfig`);
- проверка длины команд до обработки;
- throttling action `command` на пользователя/ключ;
- проброс `X-Trace-Id` в gateway;
- обработка отказов gateway с trace-id для расследования.

Safe reload поведение:
- на `SIGHUP` не делается «магический» hot-reload;
- логируется режим и предполагается controlled restart.

---

## 6. Защиты в Telegram adapter

`cmd/json-gateway` реализован как внешний HTTP edge-adapter:
- endpoint `/update` проходит через:
  - `ValidateJSON(action=update)`;
  - парсинг и валидацию полей (`user_id`, `message`);
  - `ValidateLine(action=command)`;
  - `Allow(action=command, key=user_id)`;
- при deny/throttle возвращаются контролируемые ошибки (`400`/`429`) с trace-id;
- при proxy в gateway передаётся `X-Trace-Id`.

Safe reload:
- аналогично IRC, поддержан `SIGHUP` как trigger для безопасного restart-пути.

---

## 7. Наблюдаемость: OpenTelemetry и Prometheus labels

В edge-core добавлена единая telemetry-модель решений защиты:

- `LabelCounterHook` (`internal/edge/telemetry.go`):
  - счётчики вида:
    - `edge_decisions_total|source=...|action=...|decision=...|reason_code=...`
    - `edge_throttle_dropped_total|source=...|action=...|reason_code=...`
  - только low-cardinality labels (source/action/decision/reason_code).

- `OTelHook`:
  - callback bridge без жёсткой зависимости edge-core от SDK;
  - передаёт атрибуты:
    - `edge.source`
    - `edge.action`
    - `edge.decision`
    - `edge.reason_code`

- Защита от high-cardinality label leakage:
  - `IsSafeLabelKey` блокирует использование полей вроде `trace_id`, `user_id`, `query`, `token`, `path_raw` как labels.

Практический результат:
- одинаковая причина deny/throttle в логике, метриках и (опционально) OTel-атрибутах;
- удобная корреляция abuse-событий между transport-адаптерами.

---

## 8. Конфигурация безопасности (текущее состояние)

Основные security-настройки находятся в `ebusta.yaml`:
- `gateway.*`:
  - TLS/mTLS параметры;
  - rate limits;
  - validation caps;
  - CORS;
  - адреса downstream сервисов.
- `edge.*`:
  - `reload_mode`
  - default policy
  - source-specific overrides (`gateway`, `irc`, `telegram`).
- `irc_adapter.*`, `telegram_adapter.*`:
  - bind address/port;
  - gateway URL;
  - page size;
  - debug.

---

## 9. Тестовое покрытие средств защиты

Покрытые наборы тестов:
- `internal/edge/*_test.go`:
  - throttling correctness;
  - line/json guards;
  - reason-code consistency;
  - telemetry hook behavior;
  - safe labels.

- `internal/gateway/middleware/middleware_test.go`:
  - rate limiter behavior;
  - telemetry snapshot keys.

- `internal/gateway/validation/validation_test.go`:
  - schema validation;
  - token format validation.

- `cmd/irc-adapter/handler_test.go`:
  - search flow;
  - rate-limit deny path;
  - long command rejection.

- `cmd/json-gateway/*_test.go`:
  - command parse;
  - success flow;
  - rate-limit deny;
  - malformed/oversized input rejection.

---

## 10. Итог: что уже достигнуто

На текущем этапе реализовано:
- единое edge security ядро для внешних адаптеров;
- унифицированный deny/throttle reason model;
- общий telemetry hook контракт для Prometheus labels и OTel-атрибутов;
- policy-driven защита из YAML с per-source overrides;
- transport coverage для gateway + irc + telegram;
- проверяемость через тесты на abuse/validation сценарии.

Это формирует общий security baseline для текущих и будущих внешних адаптеров.
