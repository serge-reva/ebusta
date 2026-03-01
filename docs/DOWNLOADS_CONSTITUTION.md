# DOWNLOADS_CONSTITUTION.md

## Конституция сервисов скачивания (StorageNode)

### 1. Границы (неприкосновенны)
- **Единственный внешний интерфейс** – `downloader` (HTTP), который обращается к `plasma-node` (gRPC).
- **Иерархия**: `plasma-node` (in-memory) → `tier-node` (диск) → `archive-node` (zip-архивы).
- Ни один другой сервис не должен обращаться к узлам хранения напрямую, кроме `downloader`.

### 2. Интерфейс StorageNode (gRPC)
Все узлы реализуют единый proto-сервис:

service StorageNode {
rpc Has(HasRequest) returns (HasResponse);
rpc GetMeta(GetMetaRequest) returns (GetMetaResponse);
rpc GetStream(GetStreamRequest) returns (stream Chunk);
rpc Put(stream PutRequest) returns (PutResponse);
}
text

- `Has`, `GetMeta`, `GetStream` – обязательны к реализации.
- `Put` – опционален (только для узлов, принимающих запись).

### 3. TraceID
- Каждый gRPC-вызов должен передавать TraceID в метаданных `x-trace-id`.
- Если TraceID отсутствует – узел генерирует и **обязан** включить его в логи и ответные метаданные.
- При ошибках TraceID возвращается в поле `Error.request_id`.

### 4. Обработка ошибок
Все ошибки преобразуются в `errutil.AppError` и затем в gRPC status с кодом согласно таблице:

| Ситуация | gRPC код | Внутренний код |
|----------|----------|----------------|
| Невалидный SHA1 | `InvalidArgument` | `INVALID_ARGUMENT` |
| Книга не найдена | `NotFound` | `NOT_FOUND` |
| Ошибка чтения zip/файла | `Internal` | `ZIP_ERROR` / `IO_ERROR` |
| Превышен размер plasma | `ResourceExhausted` | `PLASMA_ITEM_TOO_LARGE` |
| Родительский узел недоступен | `Unavailable` | `UPSTREAM_UNAVAILABLE` |

### 5. Политика вытеснения (PlasmaNode)
- Используется комбинация LFU + время создания (при равенстве хитов вытесняется старейший).
- Метрики: `plasma_hits_total`, `plasma_misses_total`, `plasma_evictions_total` (expvar).

### 6. Эволюция
- Добавление нового типа узла требует обновления данной конституции и прохождения тестов (`make e2e-downloads`).

### Ссылки
- [API_ERROR_MAPPING.md](API_ERROR_MAPPING.md)
- [TRACE.md](TRACE.md)
- [internal/downloads/plasma/node.go](../../internal/downloads/plasma/node.go)
