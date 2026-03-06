# Telegram Bot Architecture

Version: 1.0
Last Updated: 2026-03-06

## Purpose

This document describes the target architecture for the new `cmd/telegram-bot` component in `ebusta`.

The bot is a production-facing external adapter that integrates with Telegram Bot API and uses `gateway` as the only search entrypoint.

## Layers

The component is split into the following layers.

### Transport

Responsible for:

- receiving updates from Telegram via polling or webhook;
- mapping Telegram-specific payloads into internal command inputs;
- sending messages, editing messages, and answering callbacks.

Files/packages:

- `internal/telegrambot/transport`
- `cmd/telegram-bot/main.go`

### Use Cases

Responsible for:

- handling `/search`, `/page`, `/next`, `/prev`, `/help`;
- invoking `gatewayclient`;
- loading and storing per-user session state;
- mapping internal errors into user-facing messages.

Files/packages:

- `internal/telegrambot/usecase`

### Presenter

Responsible for:

- formatting search results into Telegram-friendly text;
- building inline keyboards for pagination and follow-up actions;
- enforcing Telegram-specific output constraints such as text length and markup escaping.

Files/packages:

- `internal/telegrambot/presenter`

### Sessions

Responsible for:

- storing per-user state needed for navigation;
- remembering last query, current page, page size, and last result metadata.

Files/packages:

- `internal/telegrambot/session`

### Commands

Responsible for:

- parsing user text commands into structured command objects;
- providing a transport-neutral parser reusable across future adapters.

Files/packages:

- `internal/botcommand`

## Interaction Flow

### Search Flow

1. Telegram sends an update.
2. Transport layer extracts `user_id`, `chat_id`, message text, callback data.
3. Transport layer generates or attaches a `trace_id`.
4. Parsed command is passed to the use case handler.
5. Use case validates input through `internal/edge`.
6. Use case calls `internal/gatewayclient.Search(...)`.
7. Search results are converted into `presenter.PresenterResult`.
8. Telegram presenter formats text and inline keyboard.
9. Transport sends the final message via Telegram API.

### Pagination Flow

1. User presses inline button or sends `/page`, `/next`, `/prev`.
2. Transport maps callback/message to internal command.
3. Use case loads session by user.
4. Use case recalculates target page and performs a new search via `gatewayclient`.
5. Presenter renders updated text and keyboard.
6. Transport edits the existing Telegram message or sends a new one, depending on mode.

## Reused Components

The bot reuses existing project components wherever possible.

- `internal/gatewayclient`
  - unified HTTP client to `gateway`
  - trace propagation via `X-Trace-Id`
  - `errutil.AppError` handling

- `internal/presenter`
  - `BookDTO`
  - `PresenterResult`
  - `Pagination`

- `internal/edge`
  - line validation
  - optional per-user rate limiting

- `internal/errutil`
  - structured error model
  - trace-aware application errors

- `internal/logger`
  - structured logging

## Commands

Supported command set for the first implementation:

- `/search <query>`
- `/search <query> page <n>`
- `/page <n>`
- `/next`
- `/prev`
- `/help`

Inline callback actions are used for:

- previous page
- next page
- direct page navigation where applicable

## Error Handling

The bot never returns raw infrastructure errors to the user.

Rules:

- validation errors become short actionable messages;
- gateway availability issues become user-friendly temporary failure messages;
- empty search results become normal functional responses, not errors;
- all logs and all downstream calls must carry `trace_id`.

## Trace Propagation

- a `trace_id` is generated at update ingress if absent;
- the same `trace_id` is passed into use cases;
- `gatewayclient` propagates it to `gateway`;
- request-scoped logs include `trace_id`.

## Testing Plan

### Unit Tests

- `internal/botcommand`
  - command parsing cases

- `internal/telegrambot/session`
  - CRUD semantics
  - concurrent access

- `internal/telegrambot/presenter`
  - result formatting
  - keyboard generation
  - long text handling

- `internal/telegrambot/usecase`
  - `/search`
  - `/page`
  - `/next`
  - `/prev`
  - `/help`
  - error mapping

- `internal/telegrambot/transport`
  - update mapping
  - callback decoding

### Integration-Oriented Tests

- webhook HTTP handler with fake Telegram update payload
- polling handler with mocked Telegram client
- gateway interaction via mocked `gatewayclient`

## Non-Goals For This Stage

- IRC refactoring
- replacement of `json-gateway`
- asynchronous background workflows
- advanced multi-message rich media flows

## Implementation Notes

- keep Telegram SDK types out of use case and presenter layers;
- keep `gateway` as the only external search dependency;
- prefer composition over adapter-specific inheritance;
- preserve clean boundaries between transport, use case, presenter, and session storage.
