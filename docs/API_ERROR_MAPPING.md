# API_ERROR_MAPPING.md

## Маппинг ошибок: условие → gRPC код → внутренний код → retryable

| Условие | gRPC код | Внутренний код (errutil) | Retryable |
|---------|----------|---------------------------|-----------|
| Невалидный аргумент (пустой запрос, неверный формат) | `InvalidArgument` | `INVALID_ARGUMENT` | false |
| Ресурс не найден (книга, пользователь) | `NotFound` | `NOT_FOUND` | false |
| Ошибка авторизации (нет прав) | `PermissionDenied` | `FORBIDDEN` | false |
| Неаутентифицирован | `Unauthenticated` | `UNAUTHORIZED` | false |
| Сервис недоступен (плазма, даунлоадер) | `Unavailable` | `UNAVAILABLE` | true |
| Таймаут | `DeadlineExceeded` | `TIMEOUT` | true |
| Внутренняя ошибка (IO, база данных) | `Internal` | `INTERNAL` | false |
| Ошибка парсинга DSL | `InvalidArgument` | `DSL_INVALID` | false |
| Конфликт версий ( optimistic lock ) | `FailedPrecondition` | `VERSION_CONFLICT` | false |

## Правила
- Все ответы с ошибкой (включая gRPC и HTTP) **обязаны** содержать `trace_id`.
- Поле `retryable` используется клиентами для автоматических повторов (только для `UNAVAILABLE`, `TIMEOUT`).
- Новые внутренние коды добавляются только в конец списка (append-only).

## Ссылки
- [TRACE.md](TRACE.md) – правила работы с TraceID.
- [internal/errutil/codes.go](../../internal/errutil/codes.go) – реализация кодов.
