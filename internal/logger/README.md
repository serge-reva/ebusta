# Logger Module for ebusta

Полноценный логгер с уровнями, множественными выходами и интеграцией с `internal/config`.

## Установка

```bash
# Скопируйте папку logger в internal/
cp -r logger /path/to/ebusta/internal/logger
```

## Интеграция с internal/config

### 1. Добавьте LoggerConfig в internal/config/config.go

```go
package config

// ... existing code ...

// LoggerConfig - секция конфигурации логгера
type LoggerConfig struct {
	Level         string        `yaml:"level"`
	Format        string        `yaml:"format"`
	DisableColors bool          `yaml:"disable_colors"`
	Outputs       OutputsConfig `yaml:"outputs"`
}

type OutputsConfig struct {
	Console ConsoleConfig    `yaml:"console"`
	File    FileOutputConfig `yaml:"file"`
}

type ConsoleConfig struct {
	Enabled   bool `yaml:"enabled"`
	UseStderr bool `yaml:"use_stderr"`
}

type FileOutputConfig struct {
	Enabled bool   `yaml:"enabled"`
	Path    string `yaml:"path"`
	Append  bool   `yaml:"append"`
}

// Добавьте в главный Config:
type Config struct {
	OpenSearch   OpenSearchConfig `yaml:"opensearch"`
	// ... existing fields ...
	Logger       LoggerConfig     `yaml:"logger"`  // <-- ДОБАВИТЬ
}
```

### 2. Добавьте секцию logger в ebusta.yaml

```yaml
# ebusta.yaml
logger:
  level: INFO           # DEBUG, INFO, WARN, ERROR, FATAL
  format: text          # text, json
  disable_colors: false
  outputs:
    console:
      enabled: true
      use_stderr: false
    file:
      enabled: true
      path: /var/log/ebusta/app.log
      append: true

# ... остальной конфиг ...
```

### 3. Инициализация в main.go

```go
package main

import (
	"ebusta/internal/config"
	"ebusta/internal/logger"
)

func main() {
	// Загрузка конфига
	cfg := config.Get()

	// Инициализация логгера
	log := logger.InitFromConfig(cfg.Logger, "my-component")

	// Использование
	log.Info("req-123", "Application started")
}
```

## Быстрый старт (без конфига)

```go
package main

import "ebusta/internal/logger"

var log = logger.Default()

func main() {
	log.Info("req-123", "Hello world")
}
```

## Уровни логирования

```go
log.Debug("req-123", "Detailed debug info")
log.Info("req-123", "General information")
log.Warn("req-123", "Warning message")
log.Error("req-123", "Error occurred", err)
log.Fatal("req-123", "Fatal error", err)  // os.Exit(1)
```

## С компонентом

```go
log := logger.Default().WithComponent("plasma")
log.Info("req-123", "HIT GetMeta sha1=abc123")
// Output: 2024-02-15 12:00:00.000 [INFO] [req-123] [plasma] HIT GetMeta sha1=abc123
```

## С контекстом

```go
func Handler(ctx context.Context) {
	log := logger.GetGlobal()
	log.InfoCtx(ctx, "Processing request")
}
```

## Множественные выходы

Конфигурация для stdout + файл:

```yaml
logger:
  level: INFO
  outputs:
    console:
      enabled: true
    file:
      enabled: true
      path: /var/log/ebusta/app.log
      append: true
```

## Миграция с log.Printf

### Было
```go
log.Printf("[%s] [plasma] HIT GetMeta sha1=%s", traceID, sha1)
```

### Стало
```go
// Вариант 1: Создать логгер с компонентом
var log = logger.Default().WithComponent("plasma")
log.Infof(traceID, "HIT GetMeta sha1=%s", sha1)

// Вариант 2: Package-level функция
logger.Componentf("plasma", traceID, "HIT GetMeta sha1=%s", sha1)
```

## Примеры интеграции

### web-frontend

```go
package main

import (
	"net/http"
	"ebusta/internal/config"
	"ebusta/internal/logger"
)

var log *logger.Logger

func main() {
	cfg := config.Get()
	log = logger.InitFromConfig(cfg.Logger, "web-frontend")

	http.HandleFunc("/search", searchHandler)
	http.ListenAndServe(":8080", nil)
}

func searchHandler(w http.ResponseWriter, r *http.Request) {
	ctx := logger.ContextFromRequest(r, "http")
	log.InfofCtx(ctx, "Search: %s", r.URL.Query().Get("q"))
}
```

### plasma-node

```go
package plasma

import (
	"context"
	"ebusta/internal/logger"
)

var log = logger.Default().WithComponent("plasma")

func (n *Node) GetMeta(ctx context.Context, req *GetMetaRequest) (*GetMetaResponse, error) {
	traceID := logger.TraceIDFromContext(ctx)
	if traceID == "" {
		traceID = logger.GenerateTraceID("plasma")
	}

	log.Infof(traceID, "HIT GetMeta sha1=%s", req.GetSha1())
	// ...
}
```

### orchestrator

```go
package main

import (
	"ebusta/internal/config"
	"ebusta/internal/logger"
)

func main() {
	cfg := config.Get()
	log := logger.InitFromConfig(cfg.Logger, "orchestrator")

	traceID := logger.GenerateTraceID("orch")
	log.Infof(traceID, "Search query=%s", query)
}
```

## Форматы вывода

### Text (по умолчанию)
```
2024-02-15 12:00:00.000 [INFO] [req-123] [web-frontend] Search request received
```

### JSON
```yaml
logger:
  format: json
```
```json
{"time":"2024-02-15T12:00:00Z","level":"INFO","trace_id":"req-123","component":"web-frontend","message":"Search request received"}
```

## Файлы модуля

```
internal/logger/
├── level.go      # Уровни: DEBUG, INFO, WARN, ERROR, FATAL
├── output.go     # Выходы: Stdout, Stderr, File, Multi
├── formatter.go  # Форматирование: Text, JSON
├── logger.go     # Основной тип Logger
├── context.go    # TraceID из context, HTTP, gRPC
├── config.go     # Интеграция с internal/config
└── global.go     # Глобальный логгер, convenience функции
```
