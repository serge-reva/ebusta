# Logger Module for ebusta

Полноценный логгер с уровнями, множественными выходами и встроенной поддержкой TraceID.

## Особенности

- **Уровни логирования**: DEBUG, INFO, WARN, ERROR, FATAL
- **Множественные выходы**: stdout, stderr, файлы, кастомные Writer
- **Форматирование**: Text (с цветами) и JSON
- **TraceID**: Всегда включен, интеграция с контекстом
- **Конфигурация**: Через переменные окружения или код
- **Совместимость**: Помощники миграции с `log.Printf`

## Установка

```bash
# Скопируйте папку logger в ваш проект
cp -r go-modules/logger /path/to/ebusta/internal/logger
```

## Быстрый старт

### Простейшее использование

```go
import "ebusta/internal/logger"

func main() {
    log := logger.Default()
    log.Info("req-123", "Application started")
}
```

### С компонентом

```go
log := logger.DefaultWithComponent("web-frontend")
log.Info("req-123", "Search request received")
// Output: 2024-02-15 12:00:00.000 [INFO] [req-123] [web-frontend] Search request received
```

### С контекстом

```go
func Handler(ctx context.Context) {
    log := logger.Default()
    log.InfoCtx(ctx, "Processing request")
}
```

## Уровни логирования

```go
log.Debug("req-123", "Detailed debug info")  // Только при DEBUG уровне
log.Info("req-123", "General information")
log.Warn("req-123", "Warning message")
log.Error("req-123", "Error occurred", err)
log.Fatal("req-123", "Fatal error", err)     // Вызывает os.Exit(1)
```

## Форматирование

### Text (по умолчанию)

```
2024-02-15 12:00:00.000 [INFO] [req-123] [web-frontend] Search request received
```

### JSON

```go
log := logger.New(logger.Config{
    Formatter: logger.NewJSONFormatter(),
})
log.Info("req-123", "Search request")
// Output: {"time":"2024-02-15T12:00:00Z","level":"INFO","trace_id":"req-123","message":"Search request"}
```

## Множественные выходы

### Консоль + файл

```go
fileOut, _ := logger.NewFileOutput("/var/log/ebusta/app.log", true)
defer fileOut.Close()

multiOut := logger.NewMultiOutput(
    &logger.StdoutOutput{},
    fileOut,
)

log := logger.New(logger.Config{
    Output: multiOut,
})
```

### Динамическое добавление выходов

```go
log := logger.Default()
log.AddOutput(fileOut)  // Добавляет файл к существующему stdout
```

## Конфигурация через переменные окружения

```bash
# Уровень логирования
export LOG_LEVEL=DEBUG    # DEBUG, INFO, WARN, ERROR, FATAL

# Формат
export LOG_FORMAT=json    # text, json, prettyjson

# Выход
export LOG_OUTPUT=stdout  # stdout, stderr, file:/path/to/file.log

# Дополнительные опции
export LOG_FILE_APPEND=true
export LOG_COLORS=false
export LOG_COMPONENT=my-service
```

```go
// Автоматически читает переменные окружения
log := logger.NewFromEnv()
```

## Интеграция с HTTP

### Middleware

```go
func main() {
    log := logger.DefaultWithComponent("web-frontend")

    handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        ctx := r.Context()
        log.InfofCtx(ctx, "Request: %s %s", r.Method, r.URL.Path)
        // ...
    })

    // Middleware добавляет TraceID в контекст и заголовки
    http.Handle("/", logger.HTTPMiddleware(handler, log))
}
```

### Извлечение TraceID из запроса

```go
func Handler(w http.ResponseWriter, r *http.Request) {
    ctx := logger.ContextFromRequest(r, "http")
    traceID := logger.TraceIDFromContext(ctx)

    log := logger.Default()
    log.Infof(traceID, "Processing request")
}
```

## Интеграция с gRPC

```go
func (s *MyService) GetMeta(ctx context.Context, req *Request) (*Response, error) {
    // TraceID из gRPC metadata
    traceID := logger.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = logger.GenerateTraceID("grpc")
    }

    log := logger.DefaultWithComponent("tier-node")
    log.Infof(traceID, "GetMeta sha1=%s", req.GetSha1())

    // Передача TraceID в исходящий контекст
    outCtx := logger.GRPCContextWithTraceID(ctx, traceID)
    // ... вызов другого сервиса с outCtx ...

    return &Response{...}, nil
}
```

## Миграция с log.Printf

### Было

```go
log.Printf("[%s] [web-frontend] Search request: %s", traceID, query)
```

### Стало

```go
// Вариант 1: Явный логгер
log := logger.DefaultWithComponent("web-frontend")
log.Infof(traceID, "Search request: %s", query)

// Вариант 2: Package-level функция
logger.Componentf("web-frontend", traceID, "Search request: %s", query)
```

## Структурированные поля

```go
log := logger.Default()

// Добавить поля
logWithUser := log.WithField("user_id", "user-123")
logWithUser.Info("req-123", "User action")

// Множественные поля
logWithFields := log.WithFields(map[string]interface{}{
    "user_id":  "user-123",
    "platform": "web",
    "version":  "1.0.0",
})
logWithFields.Info("req-123", "Request processed")
```

## Архитектура

```
internal/logger/
├── level.go      # Уровни логирования
├── output.go     # Выходы (stdout, file, multi)
├── formatter.go  # Форматирование (text, JSON)
├── logger.go     # Основной тип Logger
├── context.go    # Работа с контекстом и TraceID
├── config.go     # Конфигурация
└── global.go     # Глобальный логгер и convenience функции
```

## Примеры использования в ebusta

### web-frontend

```go
package main

import (
    "net/http"
    "ebusta/internal/logger"
)

var log = logger.DefaultWithComponent("web-frontend")

func searchHandler(w http.ResponseWriter, r *http.Request) {
    ctx := logger.ContextFromRequest(r, "http")
    traceID := logger.TraceIDFromContext(ctx)

    log.InfofCtx(ctx, "Search request: %s", r.URL.Query().Get("q"))
    // ...
}
```

### orchestrator

```go
package main

import (
    "context"
    "ebusta/internal/logger"
)

var log = logger.DefaultWithComponent("orchestrator")

func (s *Server) Search(ctx context.Context, req *SearchRequest) (*SearchResponse, error) {
    traceID := logger.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = logger.GenerateTraceID("orch")
    }

    log.Infof(traceID, "Search query=%s limit=%d", req.Query, req.Limit)
    // ...
}
```

### plasma-node

```go
package plasma

import (
    "context"
    "ebusta/internal/logger"
)

var log = logger.DefaultWithComponent("plasma")

func (n *Node) GetMeta(ctx context.Context, req *GetMetaRequest) (*GetMetaResponse, error) {
    traceID := logger.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = logger.GenerateTraceID("plasma")
    }

    if e, ok := n.items[sha1]; ok {
        log.Infof(traceID, "HIT GetMeta sha1=%s hits=%d", sha1, e.hits)
        return &GetMetaResponse{Meta: e.meta}, nil
    }

    log.Infof(traceID, "MISS GetMeta sha1=%s", sha1)
    // ...
}
```

## Расширение

### Кастомный форматтер

```go
type MyFormatter struct{}

func (f *MyFormatter) Format(entry *logger.Entry) ([]byte, error) {
    // Ваша логика форматирования
}
```

### Кастомный выход

```go
type MyOutput struct {
    // ...
}

func (o *MyOutput) Write(p []byte) (n int, err error) {
    // Ваша логика записи
}

func (o *MyOutput) Close() error { return nil }
func (o *MyOutput) Name() string { return "my-output" }
```
