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
- `/start book_<index>`

## Configuration

Example `ebusta.yaml` section:

```yaml
telegram_bot:
  enabled: true
  bot_username: "ebusta_test_bot"
  mode: "polling"
  mock_mode: false
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
- `bot_username` must match the Telegram bot username without `@`.
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
   - each book title is rendered as a clickable Telegram deep link.

### Book Selection Path

1. Send `/search tolstoy`.
2. Click a book title.
3. Expected result:
   - Telegram re-enters the bot through `/start book_<index>`;
   - bot sends a detail message with title and authors only;
   - detail message contains `📥 Скачать` and `◀️ Назад` buttons.

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

### Download Behaviour

- For files smaller than 20 MB:
  - bot downloads the file through `gateway`;
  - bot uploads it to Telegram as a document.

- For files 20 MB or larger:
  - bot does not expose internal download URLs;
  - bot responds with a user-facing message explaining that the file is too large for Telegram and should be obtained through the web interface.

## Trace Behaviour

- A new `trace_id` is generated per update in the transport layer.
- The same `trace_id` is propagated to `gateway` through `internal/gatewayclient`.
- Failures should be correlated through bot logs and gateway logs by that `trace_id`.

## Current Limitations

- Polling mode does not expose an HTTP health endpoint.
- Webhook mode requires external webhook registration and reachable routing.
- Session storage is in-memory only; restart clears user pagination state.


## Testing and CI

Fast local bot checks:

```bash
make test-telegram-bot
make build-telegram-bot
```

Docker e2e check with mock Telegram transport and mock gateway:

```bash
make test-e2e-telegram
```

This scenario does not require:

- a real Telegram bot token;
- access to the real Telegram Bot API;
- the full Ebusta distributed stack.

Instead it uses:

- `telegram_bot.mock_mode: true`;
- a local webhook endpoint exposed by `telegram-bot`;
- a mock Telegram outbound client exposed through `/_mock/messages`;
- a lightweight gateway-compatible mock service for `/search`.

### Mock Mode

When `telegram_bot.mock_mode: true`:

- `bot_token` may be empty;
- outbound Telegram calls are not sent to Telegram;
- sent and edited messages are recorded by the mock transport;
- webhook updates can be posted directly to `/webhook`;
- recorded operations can be inspected at `/_mock/messages`.

### CI Recommendation

At minimum, CI should run:

```bash
make test-telegram-bot
make build-telegram-bot
```

If Docker is available in CI, also run:

```bash
make test-e2e-telegram
```
