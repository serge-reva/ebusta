# Telegram Bot Usage

Version: 1.0
Last Updated: 2026-03-07

## Purpose

`cmd/telegram-bot` is the production Telegram Bot API adapter for Ebusta.
It accepts Telegram updates in `polling` or `webhook` mode, routes user commands through `gateway`, and renders paginated search results back to Telegram chats using Telegram HTML parse mode.

## Supported Commands

- `/search <query>`
- `/search <query> page <n>`
- `/page <n>`
- `/next`
- `/prev`
- `/help`
- `/start`

## Configuration

Example `ebusta.yaml` section:

```yaml
telegram_bot:
  enabled: true
  mode: "polling"
  listen_host: "0.0.0.0"
  listen_port: 8088
  bot_token: "123456:telegram-token"
  gateway_url: "http://gateway:8443"
  page_size: 5
  webhook_url: ""
  webhook_secret: ""
  timeouts:
    read_timeout_sec: 5
    write_timeout_sec: 10
    shutdown_timeout_sec: 10
    poll_timeout_sec: 30
  debug: false
```

Notes:

- `enabled: false` disables the component completely.
- `mode: polling` does not require a public HTTP endpoint.
- `mode: webhook` requires `webhook_url` and a reachable listener.
- `gateway_url` must point to the public HTTP gateway, not to internal gRPC services.

## Local Run

Build:

```bash
make build-telegram-bot
```

Run:

```bash
EBUSTA_CONFIG=./ebusta.yaml ./bin/telegram-bot
```

## Docker Run

Build runtime image:

```bash
make build-telegram-bot
docker compose build telegram-bot
```

Start only the bot profile:

```bash
docker compose --profile telegram up -d telegram-bot
```

Stop:

```bash
docker compose --profile telegram stop telegram-bot
```

## Manual Verification

### Happy Path

1. Set a valid `telegram_bot.bot_token`.
2. Start the stack with `gateway` and `telegram-bot`.
3. Send `/search tolstoy` to the bot.
4. Expected result:
   - bot returns a formatted page of books;
   - navigation buttons appear when more than one page exists.

### Pagination Path

1. Send `/search tolstoy`.
2. Press `Next` or send `/page 2`.
3. Expected result:
   - the bot shows the next page;
   - trace-linked logs appear in the bot and gateway.

### Error Path

1. Stop `gateway` or set an invalid `gateway_url`.
2. Send `/search tolstoy`.
3. Expected result:
   - user sees a readable failure message;
   - logs contain the same `trace_id` for the failed interaction.

## Trace Behaviour

- A new `trace_id` is generated per update in the transport layer.
- The same `trace_id` is propagated to `gateway` through `internal/gatewayclient`.
- Failures should be correlated through bot logs and gateway logs by that `trace_id`.

## Current Limitations

- Polling mode does not expose an HTTP health endpoint.
- Webhook mode requires external webhook registration and reachable routing.
- Session storage is in-memory only; restart clears user pagination state.
