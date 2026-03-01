# TRACE.md

## TraceID: сквозная трассировка

### Обязательные правила
1. **Каждый входящий запрос** (HTTP/gRPC) должен проверять наличие TraceID в заголовках:
   - HTTP: `X-Trace-Id`
   - gRPC: metadata key `x-trace-id`
2. Если TraceID отсутствует или пуст, сервис **генерирует новый** по шаблону `<префикс>-<timestamp>` (например, `gw-1643723900`).
3. Сгенерированный или полученный TraceID **обязан**:
   - присутствовать во всех логах, относящихся к запросу (через поле `trace_id`);
   - передаваться далее при вызове downstream-сервисов (в metadata или поле proto);
   - возвращаться клиенту в ответе (в заголовке `X-Trace-Id` для HTTP, или в поле `trace_id` для JSON-ответов, или в gRPC-статусе).
4. При формировании ошибки (как транспортной, так и бизнес-) TraceID **обязательно** включается в структуру ошибки (например, `ApiErrorDetails.trace_id`).

### Префиксы генерации (рекомендованные)
| Компонент | Префикс |
|-----------|---------|
| gateway | `gw` |
| orchestrator | `orch` |
| datamanager | `dm` |
| downloader | `dl` |
| archive-node | `arch` |
| tier-node | `tier` |
| plasma-node | `plasma` |
| irc-adapter | `irc` |
| telegram-adapter | `tg` |
| web-frontend | `wf` |
| cli | `cli` |

### Пример HTTP-запроса с TraceID

GET /search?q=king HTTP/1.1
Host: localhost:8443
X-Trace-Id: gw-1234567890
text


### Пример gRPC-метаданных

x-trace-id: orch-1234567890
text


### Ссылки
- [internal/logger/context.go](../../internal/logger/context.go) – реализация.
- [internal/errutil/trace.go](../../internal/errutil/trace.go) – утилиты.
