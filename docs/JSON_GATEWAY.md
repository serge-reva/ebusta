# JSON Gateway

Version: 1.0
Last Updated: 2026-03-06

## Назначение

`json-gateway` это отладочный HTTP-компонент для ручных и интеграционных сценариев.

Он не является production Telegram-ботом и не должен рассматриваться как пользовательская точка входа для Telegram.

Компонент принимает простой JSON payload, валидирует его через `internal/edge`, выполняет поиск через `gateway` и возвращает JSON-ответ.

## Статус

- debug-only компонент
- пригоден для локальной отладки и smoke/integration сценариев
- не предназначен для production-эксплуатации как Telegram Bot API адаптер

## Эндпоинты

### `GET /health`

Возвращает статус сервиса:

```json
{"status":"ok"}
```

### `POST /update`

Принимает JSON-команду:

```json
{
  "user_id": "u1",
  "message": "/search tolstoy page 2"
}
```

Поддерживаемый формат команды:

- `/search <query>`
- `/search <query> page <n>`

## Формат ответа

Успешный ответ:

```json
{
  "ok": true,
  "trace_id": "tg-1700000000000000000",
  "books": [
    {
      "id": "id1",
      "title": "Book title",
      "authors": ["Author"],
      "full_authors": "Author",
      "download_url": "/download/token"
    }
  ],
  "total": 1,
  "page": 1,
  "pages": 1
}
```

Ответ с подсказкой по использованию:

```json
{
  "ok": true,
  "trace_id": "tg-1700000000000000000",
  "message": "Usage: /search <query> [page <n>]"
}
```

Ошибка возвращается через общий JSON envelope `errutil` и содержит `trace_id`.

## Trace

- входящий trace принимается из заголовка `X-Trace-Id`
- если trace отсутствует, он генерируется внутри сервиса
- успешные и ошибочные ответы возвращают `X-Trace-Id`

## Конфигурация

На текущем этапе компонент использует конфигурационную секцию `telegram_adapter` для обратной совместимости:

```yaml
telegram_adapter:
  listen_host: "0.0.0.0"
  port: 8087
  gateway_url: "http://localhost:8443"
  page_size: 5
  debug: false
```

## Примеры `curl`

Проверка здоровья:

```bash
curl -i http://localhost:8087/health
```

Поиск:

```bash
curl -i -X POST http://localhost:8087/update \
  -H "Content-Type: application/json" \
  -H "X-Trace-Id: manual-1" \
  -d '{"user_id":"u1","message":"/search tolstoy page 1"}'
```

Неверная команда:

```bash
curl -i -X POST http://localhost:8087/update \
  -H "Content-Type: application/json" \
  -d '{"user_id":"u1","message":"/help"}'
```
